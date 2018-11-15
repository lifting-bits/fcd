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

#ifndef FCD_AST_NESTEDCONDPROP_H_
#define FCD_AST_NESTEDCONDPROP_H_

#include <llvm/IR/Module.h>

#include <z3++.h>

#include <clang/AST/RecursiveASTVisitor.h>

#include "fcd/ast_remill/IRToASTVisitor.h"
#include "fcd/ast_remill/TransformVisitor.h"
#include "fcd/ast_remill/Z3ConvVisitor.h"

namespace fcd {

class NestedCondProp : public llvm::ModulePass,
                       public TransformVisitor<NestedCondProp> {
 private:
  clang::ASTContext *ast_ctx;
  fcd::IRToASTVisitor *ast_gen;
  std::unique_ptr<z3::context> z3_ctx;
  std::unique_ptr<fcd::Z3ConvVisitor> z3_gen;

  std::unordered_map<clang::IfStmt *, clang::Expr *> parent_conds;

 public:
  static char ID;

  bool shouldTraversePostOrder() { return false; }

  NestedCondProp(clang::CompilerInstance &ins, fcd::IRToASTVisitor &ast_gen);

  bool VisitIfStmt(clang::IfStmt *stmt);

  bool runOnModule(llvm::Module &module) override;
};

llvm::ModulePass *createNestedCondPropPass(clang::CompilerInstance &ins,
                                           fcd::IRToASTVisitor &ast_gen);
}  // namespace fcd

namespace llvm {
void initializeNestedCondPropPass(PassRegistry &);
}

#endif  // FCD_AST_NESTEDCONDPROP_H_
