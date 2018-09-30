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

#define GOOGLE_STRIP_LOG 1

#include <gflags/gflags.h>
#include <glog/logging.h>

#include "fcd/ast_remill/Z3ConvVisitor.h"

namespace fcd {

// namespace {

// }  // namespace

Z3ConvVisitor::Z3ConvVisitor(clang::ASTContext *c_ctx, z3::context *z_ctx)
    : ast_ctx(c_ctx), z3_ctx(z_ctx), z3_expr_vec(*z3_ctx) {}

// Translates
z3::sort Z3ConvVisitor::GetZ3Sort(clang::QualType type) {
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

// Inserts a `clang::Expr` <=> `z3::expr` mapping into
void Z3ConvVisitor::InsertZ3Expr(clang::Expr *c_expr, z3::expr z3_expr) {
  auto iter = z3_expr_map.find(c_expr);
  CHECK(iter == z3_expr_map.end());
  z3_expr_map[c_expr] = z3_expr_vec.size();
  z3_expr_vec.push_back(z3_expr);
}

// Retrieves a `z3::expr` corresponding to `c_expr`.
// The `z3::expr` needs to be created and inserted by
// `Z3ConvVisistor::InsertZ3Expr` first.
z3::expr Z3ConvVisitor::GetZ3Expr(clang::Expr *c_expr) {
  auto iter = z3_expr_map.find(c_expr);
  CHECK(iter != z3_expr_map.end());
  return z3_expr_vec[iter->second];
}

// If `expr` is not boolean, returns a `z3::expr` that corresponds
// to the non-boolean to boolean expression cast in C. Otherwise
// returns `expr`.
z3::expr Z3ConvVisitor::Z3BoolCast(z3::expr expr) {
  if (expr.is_bool()) {
    return expr;
  } else {
    auto cast = expr != z3_ctx->num_val(0, expr.get_sort());
    return cast.simplify();
  }
}

// Retrieves or creates`z3::expr`s from `clang::Expr`.
z3::expr Z3ConvVisitor::GetOrCreateZ3Expr(clang::Expr *c_expr) {
  if (z3_expr_map.find(c_expr) == z3_expr_map.end()) {
    TraverseStmt(c_expr);
  }
  return GetZ3Expr(c_expr);
}

// Retrieves or creates `clang::Expr` from `z3::expr`.
clang::Expr *Z3ConvVisitor::GetOrCreateCExpr(z3::expr expr) { return nullptr; }

// Translates clang unary operators expressions to Z3 equivalents.
bool Z3ConvVisitor::VisitUnaryOperator(clang::UnaryOperator *c_op) {
  DLOG(INFO) << "VisitUnaryOperator: "
             << c_op->getOpcodeStr(c_op->getOpcode()).str();
  if (z3_expr_map.find(c_op) == z3_expr_map.end()) {
    // Get operand
    auto operand = GetOrCreateZ3Expr(c_op->getSubExpr());
    // Create z3 unary op
    switch (c_op->getOpcode()) {
      case clang::UO_LNot:
        InsertZ3Expr(c_op, !Z3BoolCast(operand));
        break;

      default:
        LOG(FATAL) << "Unknown clang::UnaryOperator operation";
        break;
    }
  }
  return true;
}

// Translates clang binary operators expressions to Z3 equivalents.
bool Z3ConvVisitor::VisitBinaryOperator(clang::BinaryOperator *c_op) {
  DLOG(INFO) << "VisitBinaryOperator: " << c_op->getOpcodeStr().str();
  if (z3_expr_map.find(c_op) == z3_expr_map.end()) {
    // Get operands
    auto lhs = GetOrCreateZ3Expr(c_op->getLHS());
    auto rhs = GetOrCreateZ3Expr(c_op->getRHS());
    // Create z3 binary op
    switch (c_op->getOpcode()) {
      case clang::BO_LAnd:
        InsertZ3Expr(c_op, Z3BoolCast(lhs) && Z3BoolCast(rhs));
        break;

      case clang::BO_LOr:
        InsertZ3Expr(c_op, Z3BoolCast(lhs) || Z3BoolCast(rhs));
        break;

      case clang::BO_EQ:
        InsertZ3Expr(c_op, lhs == rhs);
        break;

      default:
        LOG(FATAL) << "Unknown clang::BinaryOperator operation";
        break;
    }
  }
  return true;
}

// Translates clang variable references to Z3 constants.
bool Z3ConvVisitor::VisitDeclRefExpr(clang::DeclRefExpr *c_ref) {
  auto ref_decl = c_ref->getDecl();
  auto ref_name = ref_decl->getNameAsString();
  DLOG(INFO) << "VisitDeclRefExpr: " << ref_name;
  if (z3_expr_map.find(c_ref) == z3_expr_map.end()) {
    auto z3_name = ref_name + "_" + std::to_string(c_decl_ref_cnts[ref_decl]++);
    auto z3_sort = GetZ3Sort(c_ref->getType());
    InsertZ3Expr(c_ref, z3_ctx->constant(z3_name.c_str(), z3_sort));
  }
  return true;
}

// Translates clang literals references to Z3 numeral values.
bool Z3ConvVisitor::VisitIntegerLiteral(clang::IntegerLiteral *c_lit) {
  auto lit_val = c_lit->getValue().getLimitedValue();
  DLOG(INFO) << "VisitIntegerLiteral: " << lit_val;
  if (z3_expr_map.find(c_lit) == z3_expr_map.end()) {
    auto z3_sort = GetZ3Sort(c_lit->getType());
    InsertZ3Expr(c_lit, z3_ctx->num_val(lit_val, z3_sort));
  }
  return true;
}

}  // namespace fcd