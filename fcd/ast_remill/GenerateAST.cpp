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

#include <algorithm>
#include <unordered_set>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/ast_remill/GenerateAST.h"
#include "fcd/ast_remill/Util.h"

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
    if (!cond) {
      cond = CreateTrueExpr(*ast_ctx);
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
      compound = CreateCompoundStmt(*ast_ctx, block_body);
    }
    // Gate the compound behind a reaching condition
    block_stmts[block] =
        CreateIfStmt(*ast_ctx, GetOrCreateReachingCond(block), compound);
    // Store the compound
    result.push_back(block_stmts[block]);
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
    // Construct the initial loop body
    std::vector<clang::Stmt *> loop_body;
    for (auto block : rpo_walk) {
      if (loop->contains(block)) {
        if (IsRegionBlock(region, block) || IsSubregionEntry(region, block)) {
          auto stmt = block_stmts[block];
          auto it = std::find(region_body.begin(), region_body.end(), stmt);
          region_body.erase(it);
          loop_body.push_back(stmt);
        }
      }
    }
    // Get loop exit blocks
    llvm::SmallVector<BBEdge, 2> exits;
    loop->getExitEdges(exits);
    // Insert `break` statements
    for (auto edge : exits) {
      auto from = const_cast<llvm::BasicBlock *>(edge.first);
      auto to = const_cast<llvm::BasicBlock *>(edge.second);
      // Create edge condition
      auto cond = CreateEdgeCond(from, to);
      // Find the statement corresponding to the exiting block
      auto it =
          std::find(loop_body.begin(), loop_body.end(), block_stmts[from]);
      // Create a loop exiting `break` statement
      std::vector<clang::Stmt *> break_stmt({CreateBreakStmt(*ast_ctx)});
      auto exit_stmt = CreateIfStmt(*ast_ctx, cond,
                                    CreateCompoundStmt(*ast_ctx, break_stmt));
      // Insert it after the exiting block statement
      loop_body.insert(std::next(it), exit_stmt);
    }
    // Create the loop statement
    auto loop_stmt = CreateWhileStmt(*ast_ctx, CreateTrueExpr(*ast_ctx),
                                     CreateCompoundStmt(*ast_ctx, loop_body));
    // Insert it at the beginning of the region body
    region_body.insert(region_body.begin(), loop_stmt);
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

GenerateAST::GenerateAST(clang::CompilerInstance &ins,
                         fcd::IRToASTVisitor &ast_gen)
    : ModulePass(GenerateAST::ID),
      ast_ctx(&ins.getASTContext()),
      ast_gen(&ast_gen) {}

void GenerateAST::getAnalysisUsage(llvm::AnalysisUsage &usage) const {
  usage.addRequired<llvm::RegionInfoPass>();
  usage.addRequired<llvm::LoopInfoWrapperPass>();
}

bool GenerateAST::runOnModule(llvm::Module &module) {
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

  return true;
}

llvm::ModulePass *createGenerateASTPass(clang::CompilerInstance &ins,
                                        fcd::IRToASTVisitor &gen) {
  return new GenerateAST(ins, gen);
}

}  // namespace fcd

// using namespace llvm;
// using GenerateAST = fcd::GenerateAST;
// INITIALIZE_PASS(GenerateAST, "generate_ast",
//                 "Generate clang AST from LLVM IR", true, false)