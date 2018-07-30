/*
 * Copyright (c) 2018 Trail of Bits, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <gflags/gflags.h>
#include <glog/logging.h>

#include <llvm/ADT/DepthFirstIterator.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/Analysis/LoopInfo.h>

#include <clang/AST/Expr.h>
#include <clang/Basic/TargetInfo.h>

#include <algorithm>
#include <unordered_set>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/ast_remill/GenerateAST.h"

namespace fcd {

namespace {

using BBEdge = std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>;
using BBGraph =
    std::unordered_map<llvm::BasicBlock *, std::vector<llvm::BasicBlock *>>;

static void CFGSlice(llvm::BasicBlock *source, llvm::BasicBlock *sink,
                     BBGraph &result) {
  // Clear the output container
  result.clear();
  // Adds a path to the result slice BBGraph
  auto AddPath = [&result](std::vector<llvm::BasicBlock *> &path) {
    for (unsigned i = 1; i < path.size(); ++i) {
      result[path[i - 1]].push_back(path[i]);
      result[path[i]];
    }
  };
  // DFS walk the CFG from `source` to `sink`
  for (auto it = llvm::df_begin(source); it != llvm::df_end(source); ++it) {
    for (auto succ : llvm::successors(*it)) {
      // Construct the path up to this node while
      // checking if `succ` is already on the path
      std::vector<llvm::BasicBlock *> path;
      bool on_path = false;
      for (unsigned i = 0; i < it.getPathLength(); ++i) {
        auto node = it.getPath(i);
        on_path = node == succ;
        path.push_back(node);
      }
      // Check if the path leads to `sink`
      path.push_back(succ);
      if (!it.nodeVisited(succ)) {
        if (succ == sink) {
          AddPath(path);
        }
      } else if (result.count(succ) && !on_path) {
        AddPath(path);
      }
    }
  }
}

static clang::CompoundStmt *CreateCompoundStmt(
    clang::ASTContext &ctx, std::vector<clang::Stmt *> &stmts) {
  return new (ctx) clang::CompoundStmt(ctx, stmts, clang::SourceLocation(),
                                       clang::SourceLocation());
}

static clang::IfStmt *CreateIfStmt(clang::ASTContext &ctx, clang::Expr *cond,
                                   clang::Stmt *then) {
  return new (ctx)
      clang::IfStmt(ctx, clang::SourceLocation(), /* IsConstexpr=*/false,
                    /* init=*/nullptr,
                    /* var=*/nullptr, cond, then);
}

static clang::BreakStmt *CreateBreakStmt(clang::ASTContext &ctx) {
  return new (ctx) clang::BreakStmt(clang::SourceLocation());
}

static clang::Expr *CreateBoolBinOp(clang::ASTContext &ctx,
                                    clang::BinaryOperatorKind opc,
                                    clang::Expr *lhs, clang::Expr *rhs) {
  CHECK(lhs || rhs) << "No operand given for binary logical expression";

  if (!lhs) {
    return rhs;
  } else if (!rhs) {
    return lhs;
  } else {
    return new (ctx)
        clang::BinaryOperator(lhs, rhs, opc, ctx.BoolTy, clang::VK_RValue,
                              clang::OK_Ordinary, clang::SourceLocation(),
                              /*fpContractable=*/false);
  }
}

static clang::Expr *CreateAndExpr(clang::ASTContext &ctx, clang::Expr *lhs,
                                  clang::Expr *rhs) {
  return CreateBoolBinOp(ctx, clang::BO_LAnd, lhs, rhs);
}

static clang::Expr *CreateOrExpr(clang::ASTContext &ctx, clang::Expr *lhs,
                                 clang::Expr *rhs) {
  return CreateBoolBinOp(ctx, clang::BO_LOr, lhs, rhs);
}

static clang::Expr *CreateNotExpr(clang::ASTContext &ctx, clang::Expr *op) {
  CHECK(op) << "No operand given for unary logical expression";

  return new (ctx)
      clang::UnaryOperator(op, clang::UO_LNot, ctx.BoolTy, clang::VK_RValue,
                           clang::OK_Ordinary, clang::SourceLocation());
}

static clang::Expr *CreateTrueExpr(clang::ASTContext &ctx) {
  return clang::IntegerLiteral::Create(
      ctx, llvm::APInt(1, 1), ctx.UnsignedIntTy, clang::SourceLocation());
}

static bool IsRegionBlock(llvm::Region *region, llvm::BasicBlock *block) {
  return region->getRegionInfo()->getRegionFor(block) == region;
}

static bool IsSubregionEntry(llvm::Region *region, llvm::BasicBlock *block) {
  for (auto &subregion : *region) {
    if (subregion->getEntry() == block) {
      return true;
    }
  }
  return false;
}

static bool IsSubregionExit(llvm::Region *region, llvm::BasicBlock *block) {
  for (auto &subregion : *region) {
    if (subregion->getExit() == block) {
      return true;
    }
  }
  return false;
}

static llvm::Region *GetSubregion(llvm::Region *region,
                                  llvm::BasicBlock *block) {
  if (!region->contains(block)) {
    return nullptr;
  } else {
    return region->getSubRegionNode(block);
  }
}

}  // namespace

clang::Expr *GenerateAST::CreateEdgeCond(llvm::BasicBlock *from,
                                         llvm::BasicBlock *to) {
  // Construct the edge condition for CFG edge `(from, to)`
  clang::Expr *result = nullptr;
  auto term = from->getTerminator();
  switch (term->getOpcode()) {
    // Handle conditional branches
    case llvm::Instruction::Br: {
      auto br = llvm::cast<llvm::BranchInst>(term);
      if (br->isConditional()) {
        // Get the edge condition
        result = llvm::cast<clang::Expr>(
            ast_gen->GetOrCreateStmt(br->getCondition()));
        // Negate if `br` jumps to `to` when `expr` is false
        if (to == br->getSuccessor(1)) {
          result = CreateNotExpr(*ast_ctx, result);
        }
      }
    } break;
    // Handle returns
    case llvm::Instruction::Ret:
      break;

    default:
      LOG(FATAL) << "Unknown terminator instruction";
      break;
  }
  return result;
}

clang::Expr *GenerateAST::GetOrCreateReachingCond(llvm::BasicBlock *block) {
  auto &cond = reaching_conds[block];
  if (!cond) {
    // Gather reaching conditions from predecessors of the block
    for (auto pred : llvm::predecessors(block)) {
      auto pred_cond = reaching_conds[pred];
      auto edge_cond = CreateEdgeCond(pred, block);
      // Construct reaching condition from `pred` to `block` as
      // `reach_cond[pred] && edge_cond(pred, block)`
      if (pred_cond || edge_cond) {
        auto conj_cond = CreateAndExpr(*ast_ctx, pred_cond, edge_cond);
        // Append `conj_cond` to reaching conditions of other
        // predecessors via an `||`
        cond = CreateOrExpr(*ast_ctx, cond, conj_cond);
      }
    }
  }
  return cond;
}

std::vector<clang::Stmt *> GenerateAST::CreateBasicBlockStmts(
    llvm::BasicBlock *block) {
  std::vector<clang::Stmt *> result;
  for (auto &inst : *block) {
    if (auto stmt = ast_gen->GetOrCreateStmt(&inst)) {
      result.push_back(stmt);
    }
  }
  return result;
}

std::vector<clang::Stmt *> GenerateAST::CreateRegionStmts(
    llvm::Region *region) {
  std::vector<clang::Stmt *> result;
  for (auto block : rpo_walk) {
    // Check if the block is a subregion entry
    auto subregion = GetSubregion(region, block);
    // Ignore blocks that are neither a subregion or a region block
    if (!subregion && !IsRegionBlock(region, block)) {
      continue;
    }
    // If the block is a head of a subregion, get the compound statement of
    // the subregion otherwise create a new compound and gate it behind a
    // reaching condition.
    clang::CompoundStmt *compound = nullptr;
    if (subregion) {
      CHECK(compound = region_stmts[subregion]);
    } else {
      // Create a compound, wrapping the block
      auto block_body = CreateBasicBlockStmts(block);
      block_stmts[block] = CreateCompoundStmt(*ast_ctx, block_body);
      compound = block_stmts[block];
    }
    // The block is always reached if there's no condition, or the block is
    // the entry
    auto cond = GetOrCreateReachingCond(block);
    if (!cond || block == region->getEntry()) {
      cond = CreateTrueExpr(*ast_ctx);
    }
    // Gate the compound and store it
    result.push_back(CreateIfStmt(*ast_ctx, cond, compound));
  }
  return result;
}

clang::CompoundStmt *GenerateAST::StructureAcyclicRegion(llvm::Region *region) {
  DLOG(INFO) << "Region " << region->getNameStr() << " is acyclic";
  auto region_body = CreateRegionStmts(region);
  return CreateCompoundStmt(*ast_ctx, region_body);
}

clang::CompoundStmt *GenerateAST::StructureCyclicRegion(llvm::Region *region) {
  DLOG(INFO) << "Region " << region->getNameStr() << " is cyclic";
  auto region_body = CreateRegionStmts(region);
  // Get the loop for which the entry block of the region is a header
  // loops->getLoopFor(region->getEntry())->print(llvm::errs());
  auto loop = region->outermostLoopInRegion(loops, region->getEntry());
  // Only add loop specific control-flow to regions which contain
  // a recognized natural loop. Cyclic regions may only be fragments
  // of a larger loop structure.
  if (loop) {
    // Get loop exit blocks
    llvm::SmallVector<BBEdge, 2> exits;
    loop->getExitEdges(exits);
    for (auto edge : exits) {
      auto from = const_cast<llvm::BasicBlock *>(edge.first);
      auto to = const_cast<llvm::BasicBlock *>(edge.second);
      // Create edge condition
      auto cond = CreateEdgeCond(from, to);
      // Find the statement corresponding to the exiting block
      auto it =
          std::find(region_body.begin(), region_body.end(), block_stmts[from]);
      // Create a loop exiting `break` statement
      auto exit_stmt = CreateIfStmt(*ast_ctx, cond, CreateBreakStmt(*ast_ctx));
      // Insert it after the exiting block statement
      region_body.insert(std::next(it), exit_stmt);
    }
  }
  // Structure the rest of the loop body as a acyclic region
  return CreateCompoundStmt(*ast_ctx, region_body);
}

clang::CompoundStmt *GenerateAST::StructureRegion(llvm::Region *region) {
  DLOG(INFO) << "Structuring region " << region->getNameStr();
  auto &region_stmt = region_stmts[region];
  if (!region_stmt) {
    // Compute reaching conditions
    for (auto block : rpo_walk) {
      if (IsRegionBlock(region, block)) {
        GetOrCreateReachingCond(block);
      }
    }
    // Structure
    region_stmt = loops->isLoopHeader(region->getEntry())
                      ? StructureCyclicRegion(region)
                      : StructureAcyclicRegion(region);
  } else {
    LOG(WARNING) << "Asking to re-structure region: " << region->getNameStr()
                 << "; returning current region instead";
  }
  return region_stmt;
}

char GenerateAST::ID = 0;

GenerateAST::GenerateAST(void) : ModulePass(GenerateAST::ID) {}

void GenerateAST::getAnalysisUsage(llvm::AnalysisUsage &usage) const {
  usage.addRequired<llvm::DominatorTreeWrapperPass>();
  usage.addRequired<llvm::RegionInfoPass>();
  usage.addRequired<llvm::LoopInfoWrapperPass>();
}

bool GenerateAST::runOnModule(llvm::Module &module) {
  clang::CompilerInstance ins;
  auto inv = std::make_shared<clang::CompilerInvocation>();

  const char *tmp[] = {""};
  ins.setDiagnostics(ins.createDiagnostics(new clang::DiagnosticOptions).get());
  clang::CompilerInvocation::CreateFromArgs(*inv, tmp, tmp,
                                            ins.getDiagnostics());

  inv->getTargetOpts().Triple = module.getTargetTriple();
  ins.setInvocation(inv);
  ins.setTarget(clang::TargetInfo::CreateTargetInfo(
      ins.getDiagnostics(), ins.getInvocation().TargetOpts));

  ins.createFileManager();
  ins.createSourceManager(ins.getFileManager());
  ins.createPreprocessor(clang::TU_Complete);
  ins.createASTContext();

  ast_ctx = &ins.getASTContext();
  ast_gen = std::make_unique<ASTGenerator>(ins);

  for (auto &var : module.globals()) {
    ast_gen->VisitGlobalVar(var);
  }

  for (auto &func : module.functions()) {
    ast_gen->VisitFunctionDecl(func);
  }

  for (auto &func : module.functions()) {
    if (!func.isDeclaration()) {
      // Get single-entry, single-exit regions
      regions = &getAnalysis<llvm::RegionInfoPass>(func).getRegionInfo();
      // Get loops
      loops = &getAnalysis<llvm::LoopInfoWrapperPass>(func).getLoopInfo();
      // Get a reverse post-order walk for iterating over region blocks in
      // structurization
      llvm::ReversePostOrderTraversal<llvm::Function *> rpo(&func);
      rpo_walk.assign(rpo.begin(), rpo.end());
      // Recursively walk regions in post-order and structure
      std::function<void(llvm::Region *)> POWalkSubRegions;
      POWalkSubRegions = [&](llvm::Region *region) {
        for (auto &subregion : *region) {
          POWalkSubRegions(&*subregion);
        }
        StructureRegion(region);
      };
      // Call the above declared bad boy
      POWalkSubRegions(regions->getTopLevelRegion());
      // Get the function declaration AST node for `func`
      auto fdecl =
          llvm::cast<clang::FunctionDecl>(ast_gen->GetOrCreateDecl(&func));
      // Set it's body to the compound of the top-level region
      fdecl->setBody(region_stmts[regions->getTopLevelRegion()]);
    }
  }

  // ins.getASTContext().getTranslationUnitDecl()->dump();
  ins.getASTContext().getTranslationUnitDecl()->print(llvm::outs());

  return true;
}

llvm::ModulePass *createGenerateASTPass(void) { return new GenerateAST; }

}  // namespace fcd

// using namespace llvm;
// using GenerateAST = fcd::GenerateAST;
// INITIALIZE_PASS(GenerateAST, "generate_ast",
//                 "Generate clang AST from LLVM IR", true, false)