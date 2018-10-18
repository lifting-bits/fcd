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

#include "fcd/ast_remill/DeadStmtElim.h"

namespace fcd {

namespace {

static clang::CompoundStmt *CreateCompoundStmt(
    clang::ASTContext &ctx, std::vector<clang::Stmt *> &stmts) {
  return new (ctx) clang::CompoundStmt(ctx, stmts, clang::SourceLocation(),
                                       clang::SourceLocation());
}

}  // namespace

char DeadStmtElim::ID = 0;

DeadStmtElim::DeadStmtElim(clang::CompilerInstance &ins,
                           fcd::IRToASTVisitor &ast_gen)
    : ModulePass(DeadStmtElim::ID),
      ast_ctx(&ins.getASTContext()),
      ast_gen(&ast_gen) {}

bool DeadStmtElim::VisitIfStmt(clang::IfStmt *ifstmt) {
  // DLOG(INFO) << "VisitIfStmt";
  auto then = ifstmt->getThen();
  // Apply results from previous visitors
  if (stmts.find(then) != stmts.end()) {
    ifstmt->setThen(stmts[then]);
  }
  // Optimize this statement
  llvm::APSInt val;
  bool is_const = ifstmt->getCond()->isIntegerConstantExpr(val, *ast_ctx);
  auto compound = llvm::dyn_cast<clang::CompoundStmt>(ifstmt->getThen());
  bool is_empty = compound ? compound->body_empty() : false;
  if ((is_const && !val.getBoolValue()) || is_empty) {
    stmts[ifstmt] = nullptr;
  }
  return true;
}

bool DeadStmtElim::VisitWhileStmt(clang::WhileStmt *loop) {
  // DLOG(INFO) << "VisitWhileStmt";
  auto body = loop->getBody();
  // Apply results from previous visitors
  if (stmts.find(body) != stmts.end()) {
    loop->setBody(stmts[body]);
  }
  return true;
}

bool DeadStmtElim::VisitCompoundStmt(clang::CompoundStmt *compound) {
  // DLOG(INFO) << "VisitCompoundStmt";
  std::vector<clang::Stmt *> new_body;
  for (auto stmt : compound->body()) {
    // Apply results from previous visitors
    auto iter = stmts.find(stmt);
    if (iter != stmts.end()) {
      stmt = iter->second;
    }
    // Filter out nullptr statements
    if (!stmt) {
      continue;
    }
    // Add only necessary statements
    if (auto expr = llvm::dyn_cast<clang::Expr>(stmt)) {
      if (expr->HasSideEffects(*ast_ctx)) {
        new_body.push_back(stmt);
      }
    } else {
      new_body.push_back(stmt);
    }
  }
  // Create the a new compound
  if (new_body.size() < compound->size()) {
    stmts[compound] = CreateCompoundStmt(*ast_ctx, new_body);
  }
  return true;
}

bool DeadStmtElim::runOnModule(llvm::Module &module) {
  LOG(INFO) << "Eliminating dead statements";
  TraverseDecl(ast_ctx->getTranslationUnitDecl());
  return true;
}

llvm::ModulePass *createDeadStmtElimPass(clang::CompilerInstance &ins,
                                         fcd::IRToASTVisitor &gen) {
  return new DeadStmtElim(ins, gen);
}
}  // namespace fcd