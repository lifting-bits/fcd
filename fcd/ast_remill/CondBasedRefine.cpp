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

#include "fcd/ast_remill/CondBasedRefine.h"
#include "fcd/ast_remill/Util.h"

namespace fcd {

namespace {

using IfStmtSet = std::unordered_set<clang::IfStmt *>;

static IfStmtSet GetIfStmts(clang::CompoundStmt *compound) {
  IfStmtSet result;
  for (auto stmt : compound->body()) {
    if (auto ifstmt = llvm::dyn_cast<clang::IfStmt>(stmt)) {
      result.insert(ifstmt);
    }
  }
  return result;
}

static std::vector<clang::Stmt *> MergeIfStmtBodies(IfStmtSet &ifstmts) {
  std::vector<clang::Stmt *> result;
  for (auto stmt : ifstmts) {
    auto then = stmt->getThen();
    if (auto comp = llvm::dyn_cast<clang::CompoundStmt>(then)) {
      result.insert(result.end(), comp->body_begin(), comp->body_end());
    } else {
      LOG(FATAL) << "Then branch must be a clang::CompoundStmt!";
    }
  }
  return result;
}

}  // namespace

char CondBasedRefine::ID = 0;

CondBasedRefine::CondBasedRefine(clang::CompilerInstance &ins,
                                 fcd::IRToASTVisitor &ast_gen)
    : ModulePass(CondBasedRefine::ID),
      ast_ctx(&ins.getASTContext()),
      ast_gen(&ast_gen),
      z3_ctx(new z3::context()),
      z3_gen(new fcd::Z3ConvVisitor(ast_ctx, z3_ctx.get())) {}

void CondBasedRefine::GetBranchCandidates(clang::Expr *cond,
                                          const IfStmtSet &stmts,
                                          IfStmtSet &thens, IfStmtSet &elses) {
  auto z3_cond = z3_gen->GetOrCreateZ3Expr(cond);
  for (auto cand : stmts) {
    auto z3_cand = z3_gen->GetOrCreateZ3Expr(cand->getCond());
    auto then_test = (z3_cond && !z3_cand).simplify();
    auto else_test = (z3_cond || z3_cand).simplify();
    if (then_test.bool_value() == Z3_L_FALSE) {
      thens.insert(cand);
    } else if (else_test.bool_value() == Z3_L_TRUE) {
      elses.insert(cand);
    }
  }
}

void CondBasedRefine::CreateIfThenElseStmts(IfStmtSet stmts) {
  while (!stmts.empty()) {
    auto ifstmt = *stmts.begin();
    auto cond = ifstmt->getCond();
    IfStmtSet thens, elses;
    GetBranchCandidates(cond, stmts, thens, elses);
    stmts.erase(ifstmt);
    if (thens.size() + elses.size() > 1) {
      for (auto stmt : thens) {
        substitutions[stmt] = nullptr;
        stmts.erase(stmt);
      }
      for (auto stmt : elses) {
        substitutions[stmt] = nullptr;
        stmts.erase(stmt);
      }
      auto then_body = MergeIfStmtBodies(thens);
      auto else_body = MergeIfStmtBodies(elses);
      auto stmt =
          CreateIfStmt(*ast_ctx, cond, CreateCompoundStmt(*ast_ctx, then_body));
      stmt->setElse(CreateCompoundStmt(*ast_ctx, else_body));
      substitutions[ifstmt] = stmt;
    }
  }
}

bool CondBasedRefine::VisitIfStmt(clang::IfStmt *ifstmt) {
  // DLOG(INFO) << "VisitIfStmt";
  auto then = ifstmt->getThen();
  auto iter = substitutions.find(then);
  if (iter != substitutions.end()) {
    if (auto sub = iter->second) {
      ifstmt->setThen(sub);
    }
  }
  return true;
}

bool CondBasedRefine::VisitWhileStmt(clang::WhileStmt *loop) {
  // DLOG(INFO) << "VisitWhileStmt";
  auto body = loop->getBody();
  auto iter = substitutions.find(body);
  if (iter != substitutions.end()) {
    if (auto sub = iter->second) {
      loop->setBody(sub);
    }
  }
  return true;
}

bool CondBasedRefine::VisitFunctionDecl(clang::FunctionDecl *fdecl) {
  // DLOG(INFO) << "VisitFunctionDecl";
  if (auto body = fdecl->getBody()) {
    auto iter = substitutions.find(body);
    if (iter != substitutions.end()) {
      if (auto sub = iter->second) {
        fdecl->setBody(sub);
      }
    }
  }
  return true;
}

bool CondBasedRefine::VisitCompoundStmt(clang::CompoundStmt *compound) {
  // DLOG(INFO) << "VisitCompoundStmt";
  // Create if-then-else substitutions for IfStmts in `compound`
  CreateIfThenElseStmts(GetIfStmts(compound));
  // Apply previously computed substitutions
  std::vector<clang::Stmt *> new_body;
  for (auto stmt : compound->body()) {
    auto iter = substitutions.find(stmt);
    if (iter != substitutions.end()) {
      if (auto sub = iter->second) {
        new_body.push_back(sub);
      }
    } else {
      new_body.push_back(stmt);
    }
  }
  substitutions[compound] = CreateCompoundStmt(*ast_ctx, new_body);
  return true;
}

bool CondBasedRefine::runOnModule(llvm::Module &module) {
  LOG(INFO) << "Condition-based refinement";
  TraverseDecl(ast_ctx->getTranslationUnitDecl());
  return true;
}

llvm::ModulePass *createCondBasedRefinePass(clang::CompilerInstance &ins,
                                            fcd::IRToASTVisitor &gen) {
  return new CondBasedRefine(ins, gen);
}
}  // namespace fcd