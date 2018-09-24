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

// namespace {

// }  // namespace

Z3ConvVisitor::Z3ConvVisitor(clang::ASTContext *c_ctx, z3::context *z_ctx)
    : ast_ctx(c_ctx), z3_ctx(z_ctx) {}

z3::sort Z3ConvVisitor::GetZ3Sort(clang::QualType &type) {
  auto bitwidth = ast_ctx->getTypeSize(type);
  // Booleans
  if (type->isBooleanType()) {
    return z3_ctx->bool_sort();
  }
  // Floating points
  if (type->isRealFloatingType()) {
    switch (bitwidth) {
      case 16:
        return z3::to_sort(*z3_ctx, Z3_mk_fpa_sort_16(*z3_ctx));
        break;

      case 32:
        return z3::to_sort(*z3_ctx, Z3_mk_fpa_sort_32(*z3_ctx));
        break;

      case 64:
        return z3::to_sort(*z3_ctx, Z3_mk_fpa_sort_64(*z3_ctx));
        break;

      case 128:
        return z3::to_sort(*z3_ctx, Z3_mk_fpa_sort_128(*z3_ctx));
        break;

      default:
        LOG(FATAL) << "Unsupported floating-point bitwidth!";
        break;
    }
  }
  // Default to bitvectors
  return z3::to_sort(*z3_ctx, Z3_mk_bv_sort(*z3_ctx, bitwidth));
}

z3::expr Z3ConvVisitor::GetOrCreateZ3Expr(clang::Expr *expr) {
  auto &z3_ast = z3_exprs[expr];
  if (!z3_ast) {
    TraverseStmt(expr);
  }
  return z3::to_expr(*z3_ctx, z3_ast);
}

clang::Expr *Z3ConvVisitor::GetOrCreateCExpr(z3::expr expr) { return nullptr; }

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