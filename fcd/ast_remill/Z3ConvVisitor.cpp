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

#include "fcd/ast_remill/Z3ConvVisitor.h"

namespace fcd {

namespace {

static z3::sort GetZ3Sort(z3::context &ctx, clang::QualType type) {
  return ctx.bool_sort();
}

}  // namespace

Z3ConvVisitor::Z3ConvVisitor(clang::ASTContext *c_ctx, z3::context *z_ctx)
    : ast_ctx(c_ctx), z3_ctx(z_ctx) {}

z3::expr *Z3ConvVisitor::GetOrCreateZ3Expr(clang::Expr *expr) {
  auto &z3_expr = z3_exprs[expr];
  if (!z3_expr) {
    TraverseStmt(expr);
  }
  return z3_expr;
}

clang::Expr *Z3ConvVisitor::GetOrCreateCExpr(z3::expr *expr) { return nullptr; }

bool Z3ConvVisitor::VisitUnaryOperator(clang::UnaryOperator *op) {
  op->dump();
  return true;
}

bool Z3ConvVisitor::VisitBinaryOperator(clang::BinaryOperator *op) {
  op->dump();
  return true;
}

bool Z3ConvVisitor::VisitDeclRefExpr(clang::DeclRefExpr *ref) {
  // auto &constant = z3_exprs[ref];
  // if (!constant) {
  //   constant = &z3_ctx->constant(ref->getDecl()->getNameAsString().c_str(),
  //                               GetZ3Sort(*z3_ctx, ref->getType()));
  // }
  return true;
}

bool Z3ConvVisitor::VisitIntegerLiteral(clang::IntegerLiteral *lit) {
  lit->dump();
  return true;
}

}  // namespace fcd