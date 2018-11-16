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

#ifndef FCD_AST_GENERATEAST_H_
#define FCD_AST_GENERATEAST_H_

#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/RegionInfo.h>
#include <llvm/IR/Module.h>

#include <unordered_set>

#include "fcd/ast_remill/IRToASTVisitor.h"

namespace fcd {

class GenerateAST : public llvm::ModulePass {
 private:
  clang::ASTContext *ast_ctx;
  fcd::IRToASTVisitor *ast_gen;
  std::unordered_map<llvm::BasicBlock *, clang::Expr *> reaching_conds;
  std::unordered_map<llvm::BasicBlock *, clang::IfStmt *> block_stmts;
  std::unordered_map<llvm::Region *, clang::CompoundStmt *> region_stmts;

  llvm::DominatorTree *domtree;
  llvm::RegionInfo *regions;
  llvm::LoopInfo *loops;

  std::vector<llvm::BasicBlock *> rpo_walk;

  clang::Expr *CreateEdgeCond(llvm::BasicBlock *from, llvm::BasicBlock *to);
  clang::Expr *GetOrCreateReachingCond(llvm::BasicBlock *block);
  std::vector<clang::Stmt *> CreateBasicBlockStmts(llvm::BasicBlock *block);
  std::vector<clang::Stmt *> CreateRegionStmts(llvm::Region *region);

  using BBSet = std::unordered_set<llvm::BasicBlock *>;

  void RefineLoopSuccessors(llvm::Loop *loop, BBSet &members,
                            BBSet &successors);

  clang::CompoundStmt *StructureAcyclicRegion(llvm::Region *region);
  clang::CompoundStmt *StructureCyclicRegion(llvm::Region *region);
  clang::CompoundStmt *StructureRegion(llvm::Region *region);

 public:
  static char ID;

  GenerateAST(clang::CompilerInstance &ins, fcd::IRToASTVisitor &ast_gen);

  void getAnalysisUsage(llvm::AnalysisUsage &usage) const override;
  bool runOnModule(llvm::Module &module) override;
};

llvm::ModulePass *createGenerateASTPass(clang::CompilerInstance &ins,
                                        fcd::IRToASTVisitor &ast_gen);
}  // namespace fcd

namespace llvm {
void initializeGenerateASTPass(PassRegistry &);
}

#endif  // FCD_AST_GENERATEAST_H_
