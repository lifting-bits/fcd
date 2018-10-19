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

#include "fcd/ast_remill/Z3CondSimplify.h"

namespace fcd {

char Z3CondSimplify::ID = 0;

Z3CondSimplify::Z3CondSimplify(clang::CompilerInstance &ins,
                               fcd::IRToASTVisitor &ast_gen)
    : ModulePass(Z3CondSimplify::ID),
      ast_ctx(&ins.getASTContext()),
      ast_gen(&ast_gen),
      z3_ctx(new z3::context()),
      z3_gen(new fcd::Z3ConvVisitor(ast_ctx, z3_ctx.get())) {}

bool Z3CondSimplify::VisitIfStmt(clang::IfStmt *stmt) {
  auto expr = z3_gen->GetOrCreateZ3Expr(stmt->getCond());
  z3::goal cond(*z3_ctx);
  cond.add(expr);
  // Custom Z3 bitvector predicate simplifier
  auto simplify =
      // Simplify boolean structure with AIGs
      z3::tactic(*z3_ctx, "aig") &
      // Propagate bounds over bit-vectors
      z3::tactic(*z3_ctx, "propagate-bv-bounds") &
      // Tseitin transformation
      z3::tactic(*z3_ctx, "tseitin-cnf") &
      // Contextual simplification
      z3::tactic(*z3_ctx, "ctx-simplify");
  // Apply on condition
  auto app = simplify(cond);
  CHECK(app.size() == 1) << "Unexpected multiple goals in application!";
  auto result = app[0].as_expr().simplify();
  stmt->setCond(z3_gen->GetOrCreateCExpr(result));
  return true;
}

bool Z3CondSimplify::runOnModule(llvm::Module &module) {
  LOG(INFO) << "Simplifying conditions using Z3";
  TraverseDecl(ast_ctx->getTranslationUnitDecl());
  return true;
}

llvm::ModulePass *createZ3CondSimplifyPass(clang::CompilerInstance &ins,
                                           fcd::IRToASTVisitor &gen) {
  return new Z3CondSimplify(ins, gen);
}
}  // namespace fcd