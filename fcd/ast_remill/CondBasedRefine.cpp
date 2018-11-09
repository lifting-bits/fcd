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

#include <algorithm>

#include "fcd/ast_remill/CondBasedRefine.h"

namespace fcd {

namespace {

static std::set<clang::IfStmt *> GetIfStmts(clang::CompoundStmt *compound) {
  std::set<clang::IfStmt *> result;
  for (auto stmt : compound->body()) {
    if (auto ifstmt = llvm::dyn_cast<clang::IfStmt>(stmt)) {
      result.insert(ifstmt);
    }
  }
  return result;
}

static std::vector<z3::expr> SplitClause(z3::expr expr) {
  std::vector<z3::expr> result;
  if (expr.decl().decl_kind() == Z3_OP_AND) {
    for (unsigned i = 0; i < expr.num_args(); ++i) {
      result.push_back(expr.arg(i));
    }
  } else {
    result.push_back(expr);
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

z3::expr CondBasedRefine::ThenTest(z3::expr lhs, z3::expr rhs) {
  auto Pred = [](z3::expr a, z3::expr b) {
    auto test = (!a && b).simplify();
    return test.bool_value() != Z3_L_FALSE;
  };

  auto lhs_c = SplitClause(lhs);
  auto rhs_c = SplitClause(rhs);
  auto match = std::mismatch(lhs_c.begin(), lhs_c.end(), rhs_c.begin(), Pred);

  if (match.first == lhs_c.end() || match.second == rhs_c.end()) {
    return z3_ctx->bool_val(false);
  }

  return *match.first;
}

z3::expr CondBasedRefine::ElseTest(z3::expr lhs, z3::expr rhs) {
  auto Pred = [](z3::expr a, z3::expr b) {
    auto test = (a || b).simplify();
    return test.bool_value() != Z3_L_TRUE;
  };

  auto lhs_c = SplitClause(lhs);
  auto rhs_c = SplitClause(rhs);
  auto match = std::mismatch(lhs_c.begin(), lhs_c.end(), rhs_c.begin(), Pred);

  if (match.first == lhs_c.end() || match.second == rhs_c.end()) {
    return z3_ctx->bool_val(false);
  }

  return *match.first;
}

clang::IfStmt *CondBasedRefine::MergeIfStmts(clang::IfStmt *lhs,
                                             clang::IfStmt *rhs) {
  auto z3_lhs = z3_gen->Z3BoolCast(z3_gen->GetOrCreateZ3Expr(lhs->getCond()));
  auto z3_rhs = z3_gen->Z3BoolCast(z3_gen->GetOrCreateZ3Expr(rhs->getCond()));

  std::vector<clang::Stmt *> then_body({lhs});
  
  auto then_test = ThenTest(z3_lhs, z3_rhs);
  if (then_test.bool_value() != Z3_L_FALSE) {
    auto cond = z3_gen->GetOrCreateCExpr(then_test);
    then_body.push_back(rhs);
    auto thens = CreateCompoundStmt(*ast_ctx, then_body);
    return CreateIfStmt(*ast_ctx, cond, thens);
  }

  auto else_test = ElseTest(z3_lhs, z3_rhs);
  if (else_test.bool_value() != Z3_L_FALSE) {
    auto cond = z3_gen->GetOrCreateCExpr(else_test);
    std::vector<clang::Stmt *> else_body({rhs});
    auto thens = CreateCompoundStmt(*ast_ctx, then_body);
    auto result = CreateIfStmt(*ast_ctx, cond, thens);
    result->setElse(CreateCompoundStmt(*ast_ctx, else_body));
    return result;
  }

  return nullptr;
}

void CondBasedRefine::CreateIfThenElseStmts(std::set<clang::IfStmt *> stmts) {
  while (!stmts.empty()) {
    auto sub = *stmts.begin();
    stmts.erase(sub);
    auto lhs = sub;
    std::set<clang::IfStmt *> merged;
    for (auto rhs : stmts) {
      if (auto ifstmt = MergeIfStmts(lhs, rhs)) {
        lhs = ifstmt;
        merged.insert(rhs);
      }
    }
    if (lhs != sub) {
      substitutions[sub] = lhs;
      for (auto ifstmt : merged) {
        substitutions[ifstmt] = nullptr;
        stmts.erase(ifstmt);
      }
    }
  }
}

bool CondBasedRefine::VisitCompoundStmt(clang::CompoundStmt *compound) {
  // DLOG(INFO) << "VisitCompoundStmt";
  // Create if-then-else substitutions for IfStmts in `compound`
  CreateIfThenElseStmts(GetIfStmts(compound));
  // Apply created if-then-else substitutions
  ReplaceChildren(compound, substitutions);
  // Create a replacement for `compound`
  std::vector<clang::Stmt *> new_body;
  for (auto stmt : compound->body()) {
    if (stmt) {
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