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

#include <clang/AST/Expr.h>
#include <clang/Basic/TargetInfo.h>

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

clang::CompoundStmt *CreateCompoundStmt(clang::ASTContext &ctx,
                                        std::vector<clang::Stmt *> &stmts) {
  return new (ctx) clang::CompoundStmt(ctx, stmts, clang::SourceLocation(),
                                       clang::SourceLocation());
}

clang::IfStmt *CreateIfStmt(clang::ASTContext &ctx, clang::Expr *cond,
                            clang::Stmt *then) {
  return new (ctx)
      clang::IfStmt(ctx, clang::SourceLocation(), /* IsConstexpr=*/false,
                    /* init=*/nullptr,
                    /* var=*/nullptr, cond, then);
}

clang::Expr *CreateBoolBinOp(clang::ASTContext &ctx,
                             clang::BinaryOperatorKind opc, clang::Expr *lhs,
                             clang::Expr *rhs) {
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

clang::Expr *CreateAndExpr(clang::ASTContext &ctx, clang::Expr *lhs,
                           clang::Expr *rhs) {
  return CreateBoolBinOp(ctx, clang::BO_LAnd, lhs, rhs);
}

clang::Expr *CreateOrExpr(clang::ASTContext &ctx, clang::Expr *lhs,
                          clang::Expr *rhs) {
  return CreateBoolBinOp(ctx, clang::BO_LOr, lhs, rhs);
}

clang::Expr *CreateNotExpr(clang::ASTContext &ctx, clang::Expr *op) {
  CHECK(op) << "No operand given for unary logical expression";

  return new (ctx)
      clang::UnaryOperator(op, clang::UO_LNot, ctx.BoolTy, clang::VK_RValue,
                           clang::OK_Ordinary, clang::SourceLocation());
}

clang::Expr *CreateTrueExpr(clang::ASTContext &ctx) {
  return clang::IntegerLiteral::Create(ctx, llvm::APInt(1, 1), ctx.BoolTy,
                                       clang::SourceLocation());
}

}  // namespace

void GenerateAST::StructureAcyclicRegion(
    llvm::Region *region, std::vector<llvm::BasicBlock *> &rpo_walk) {
  DLOG(INFO) << "Structuring acyclic region " << region->getNameStr();
  BBGraph slice;
  CFGSlice(region->getEntry(), region->getExit(), slice);
  std::vector<llvm::BasicBlock *> region_blocks;
  for (auto block : rpo_walk) {
    // Ignore non-slice blocks
    if (slice.count(block) == 0) {
      continue;
    }
    // Ignore block for which we already have reaching conditions.
    // These reaching conditions have been computed as part of a
    // previous region.
    if (reaching_conds[block]) {
      continue;
    }
    // Gather reaching conditions from predecessors of the block
    for (auto pred : llvm::predecessors(block)) {
      auto cond = reaching_conds[pred];
      // Construct the reaching condition to corresponding to the
      // CFG edge from `pred` to `block`. Each terminator type
      // has it's own handling.
      auto term = pred->getTerminator();
      switch (term->getOpcode()) {
        // Handle conditional branches
        case llvm::Instruction::Br: {
          auto br = llvm::cast<llvm::BranchInst>(term);
          if (br->isConditional()) {
            // Get the edge condition
            auto expr = llvm::cast<clang::Expr>(
                ast_gen->GetOrCreateStmt(br->getCondition()));
            // Negate if `br` jumps to `block` when `expr` is false
            if (block == br->getSuccessor(1)) {
              expr = CreateNotExpr(*ast_ctx, expr);
            }
            // Append `cond` to the predecessor condition via an `&&`
            cond = CreateAndExpr(*ast_ctx, cond, expr);
          }
        } break;
        // Handle returns
        case llvm::Instruction::Ret:
          break;

        default:
          LOG(FATAL) << "Unknown terminator instruction";
          break;
      }
      // Append `cond` to reaching conditions of other predecessors via an `||`
      reaching_conds[block] =
          CreateOrExpr(*ast_ctx, reaching_conds[block], cond);
    }
    // Remember processed blocks
    region_blocks.push_back(block);
  }
  // Gather statements that must NOT be gated behind reaching conds
  std::unordered_set<clang::Stmt *> cond_exprs;
  for (auto block : region_blocks) {
    if (auto cond = reaching_conds[block]) {
      cond_exprs.insert(cond->child_begin(), cond->child_end());
    }
  }
  // Create a compound statement representing the region
  std::vector<clang::Stmt *> region_stmts;
  for (auto block : region_blocks) {
    std::vector<clang::Stmt *> block_stmts;
    for (auto &inst : *block) {
      auto stmt = ast_gen->GetOrCreateStmt(&inst);
      if (cond_exprs.count(stmt) == 0) {
        block_stmts.push_back(stmt);
      }
    }

    auto compound = CreateCompoundStmt(*ast_ctx, block_stmts);

    auto cond = reaching_conds[block] ? reaching_conds[block]
                                      : CreateTrueExpr(*ast_ctx);

    region_stmts.push_back(CreateIfStmt(*ast_ctx, cond, compound));
  }
  CreateCompoundStmt(*ast_ctx, region_stmts)->dump();
}

void GenerateAST::StructureCyclicRegion(llvm::Region *region) {
  DLOG(INFO) << "Structuring cyclic region " << region->getNameStr();
}

char GenerateAST::ID = 0;

GenerateAST::GenerateAST(void) : ModulePass(GenerateAST::ID) {}

void GenerateAST::getAnalysisUsage(llvm::AnalysisUsage &usage) const {
  usage.addRequired<llvm::DominatorTreeWrapperPass>();
  usage.addRequired<llvm::RegionInfoPass>();
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
      // Compute back edges using a DFS walk of the CFG
      llvm::SmallVector<BBEdge, 10> back_edges;
      llvm::FindFunctionBackedges(func, back_edges);
      // Extract loop headers
      std::unordered_set<const llvm::BasicBlock *> loop_headers;
      for (auto edge : back_edges) {
        loop_headers.insert(edge.second);
      }
      // Get single-entry, single-exit regions
      auto regions = &getAnalysis<llvm::RegionInfoPass>(func).getRegionInfo();
      // Get a post-order walk for iterating over regions
      std::vector<llvm::BasicBlock *> po_walk(llvm::po_begin(&func),
                                              llvm::po_end(&func));
      // Get a reverse post-order walk for iterating over region blocks in
      // structurization
      std::vector<llvm::BasicBlock *> rpo_walk(po_walk.rbegin(),
                                               po_walk.rend());
      // Walk the CFG in post-order and structurize regions
      for (auto block : po_walk) {
        // Check if `block` is the head of a region
        auto region = regions->getRegionFor(block);
        if (block == region->getEntry()) {
          // Check if `region` contains a cycle
          if (loop_headers.count(block) > 0) {
            StructureCyclicRegion(region);
          } else {
            StructureAcyclicRegion(region, rpo_walk);
          }
        }
      }

      std::vector<clang::Stmt *> body;
      for (auto pair : reaching_conds) {
        body.push_back(pair.second);
      }
      auto fdecl =
          llvm::cast<clang::FunctionDecl>(ast_gen->GetOrCreateDecl(&func));
      fdecl->setBody(new (ast_ctx) clang::CompoundStmt(
          *ast_ctx, body, clang::SourceLocation(), clang::SourceLocation()));
    }
  }

  // ins.getASTContext().getTranslationUnitDecl()->dump();
  // ins.getASTContext().getTranslationUnitDecl()->print(llvm::outs());

  return true;
}

llvm::ModulePass *createGenerateASTPass(void) { return new GenerateAST; }

}  // namespace fcd

// using namespace llvm;
// using GenerateAST = fcd::GenerateAST;
// INITIALIZE_PASS(GenerateAST, "generate_ast",
//                 "Generate clang AST from LLVM IR", true, false)