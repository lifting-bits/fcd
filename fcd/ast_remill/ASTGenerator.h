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

#ifndef FCD_AST_ASTGENERATOR_H_
#define FCD_AST_ASTGENERATOR_H_

#include <llvm/IR/InstVisitor.h>

#include <clang/AST/ASTContext.h>
#include <clang/Frontend/CompilerInstance.h>

#include <memory>
#include <unordered_map>

namespace fcd {

class ASTGenerator : public llvm::InstVisitor<ASTGenerator> {
 private:
  clang::CompilerInstance *cc_ins;
  clang::ASTContext &ast_ctx;

  std::unordered_map<llvm::Value *, clang::Decl *> decls;
  std::unordered_map<llvm::Value *, clang::Stmt *> stmts;

  clang::Expr *GetOperandExpr(clang::DeclContext *decl_ctx, llvm::Value *val);
  clang::FunctionDecl *GetFunctionDecl(llvm::Instruction *inst);

 public:
  ASTGenerator(clang::CompilerInstance &ins);

  void VisitGlobalVar(llvm::GlobalVariable &var);
  void VisitFunctionDecl(llvm::Function &func);
  void VisitFunctionDefn(llvm::Function &func);
  void VisitBasicBlock(llvm::BasicBlock &block);

  void visitCallInst(llvm::CallInst &inst);
  void visitGetElementPtrInst(llvm::GetElementPtrInst &inst);
  void visitAllocaInst(llvm::AllocaInst &inst);
  void visitLoadInst(llvm::LoadInst &inst);
  void visitStoreInst(llvm::StoreInst &inst);
  void visitReturnInst(llvm::ReturnInst &inst);
  void visitBinaryOperator(llvm::BinaryOperator &inst);
  void visitCmpInst(llvm::CmpInst &inst);
  // void visitInstruction(llvm::Instruction &inst);
};

}  // namespace fcd

#endif  // FCD_AST_ASTGENERATOR_H_