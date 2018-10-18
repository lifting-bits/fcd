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

#include "fcd/ast_remill/NestedScopeCombiner.h"

namespace fcd {

namespace {

static clang::CompoundStmt *CreateCompoundStmt(
    clang::ASTContext &ctx, std::vector<clang::Stmt *> &stmts) {
  return new (ctx) clang::CompoundStmt(ctx, stmts, clang::SourceLocation(),
                                       clang::SourceLocation());
}

}  // namespace

char NestedScopeCombiner::ID = 0;

NestedScopeCombiner::NestedScopeCombiner(clang::CompilerInstance &ins,
                                         fcd::IRToASTVisitor &ast_gen)
    : ModulePass(NestedScopeCombiner::ID),
      ast_ctx(&ins.getASTContext()),
      ast_gen(&ast_gen) {}

bool NestedScopeCombiner::VisitIfStmt(clang::IfStmt *ifstmt) {
  // DLOG(INFO) << "VisitIfStmt";
  auto cond = ifstmt->getCond();
  auto then = ifstmt->getThen();
  // Substitute `then`
  auto iter = substitutions.find(then);
  if (iter != substitutions.end()) {
    ifstmt->setThen(iter->second);
  }
  // Determine whether `cond` is a constant expression that is always true and
  // `ifstmt` should be replaced by `then` in it's parent nodes.
  llvm::APSInt val;
  bool is_const = cond->isIntegerConstantExpr(val, *ast_ctx);
  if (is_const && val.getBoolValue()) {
    substitutions[ifstmt] = then;
  }
  return true;
}

bool NestedScopeCombiner::VisitWhileStmt(clang::WhileStmt *loop) {
  // DLOG(INFO) << "VisitWhileStmt";
  auto body = loop->getBody();
  auto iter = substitutions.find(body);
  if (iter != substitutions.end()) {
    loop->setBody(iter->second);
  }
  return true;
}

bool NestedScopeCombiner::VisitFunctionDecl(clang::FunctionDecl *fdecl) {
  // DLOG(INFO) << "VisitFunctionDecl";
  if (auto body = fdecl->getBody()) {
    auto iter = substitutions.find(body);
    if (iter != substitutions.end()) {
      fdecl->setBody(iter->second);
    }
  }
  return true;
}

bool NestedScopeCombiner::VisitCompoundStmt(clang::CompoundStmt *compound) {
  // DLOG(INFO) << "VisitCompoundStmt";
  std::vector<clang::Stmt *> new_body;
  for (auto stmt : compound->body()) {
    auto iter = substitutions.find(stmt);
    if (iter != substitutions.end()) {
      auto sub = iter->second;
      if (auto child = llvm::dyn_cast<clang::CompoundStmt>(sub)) {
        new_body.insert(new_body.end(), child->body_begin(), child->body_end());
      } else {
        new_body.push_back(sub);
      }
    } else {
      new_body.push_back(stmt);
    }
  }
  substitutions[compound] = CreateCompoundStmt(*ast_ctx, new_body);
  return true;
}

bool NestedScopeCombiner::runOnModule(llvm::Module &module) {
  LOG(INFO) << "Combining nested scopes";
  TraverseDecl(ast_ctx->getTranslationUnitDecl());
  return true;
}

llvm::ModulePass *createNestedScopeCombinerPass(clang::CompilerInstance &ins,
                                                fcd::IRToASTVisitor &gen) {
  return new NestedScopeCombiner(ins, gen);
}
}  // namespace fcd