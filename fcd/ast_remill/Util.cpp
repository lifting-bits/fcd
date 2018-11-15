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

#include "fcd/ast_remill/Util.h"

namespace fcd {

namespace {

static clang::Expr *CreateBoolBinOp(clang::ASTContext &ctx,
                                    clang::BinaryOperatorKind opc,
                                    clang::Expr *lhs, clang::Expr *rhs) {
  CHECK(lhs || rhs) << "No operand given for binary logical expression";

  if (!lhs) {
    return rhs;
  } else if (!rhs) {
    return lhs;
  } else {
    return new (ctx)
        clang::BinaryOperator(lhs, rhs, opc, ctx.BoolTy, clang::VK_RValue,
                              clang::OK_Ordinary, clang::SourceLocation(),
                              /*fpContractable=*/false);
  }
}

}  // namespace

bool ReplaceChildren(clang::Stmt *stmt, StmtMap &repl_map) {
  auto change = false;
  for (auto c_it = stmt->child_begin(); c_it != stmt->child_end(); ++c_it) {
    auto s_it = repl_map.find(*c_it);
    if (s_it != repl_map.end()) {
      *c_it = s_it->second;
      change = true;
    }
  }
  return change;
}

clang::IdentifierInfo *CreateIdentifier(clang::ASTContext &ctx,
                                        std::string name) {
  std::string str = "";
  for (auto chr : name) {
    str.push_back(std::isalnum(chr) ? chr : '_');
  }
  return &ctx.Idents.get(str);
}

clang::DeclRefExpr *CreateDeclRefExpr(clang::ASTContext &ast_ctx,
                                      clang::ValueDecl *val) {
  DLOG(INFO) << "Creating DeclRefExpr for " << val->getNameAsString();
  return clang::DeclRefExpr::Create(
      ast_ctx, clang::NestedNameSpecifierLoc(), clang::SourceLocation(), val,
      false, val->getLocation(), val->getType(), clang::VK_LValue);
}

clang::CompoundStmt *CreateCompoundStmt(clang::ASTContext &ctx,
                                        std::vector<clang::Stmt *> &stmts) {
  return new (ctx) clang::CompoundStmt(ctx, stmts, clang::SourceLocation(),
                                       clang::SourceLocation());
}

clang::IfStmt *CreateIfStmt(clang::ASTContext &ctx, clang::Expr *cond,
                            clang::Stmt *then) {
  return new (ctx)
      clang::IfStmt(ctx, clang::SourceLocation(), /* IsConstexpr=*/false,
                    /* init=*/nullptr,
                    /* var=*/nullptr, cond, then);
}

clang::WhileStmt *CreateWhileStmt(clang::ASTContext &ctx, clang::Expr *cond,
                                  clang::Stmt *body) {
  return new (ctx)
      clang::WhileStmt(ctx, nullptr, cond, body, clang::SourceLocation());
}

clang::DoStmt *CreateDoStmt(clang::ASTContext &ctx, clang::Expr *cond,
                            clang::Stmt *body) {
  return new (ctx)
      clang::DoStmt(body, cond, clang::SourceLocation(),
                    clang::SourceLocation(), clang::SourceLocation());
}

clang::BreakStmt *CreateBreakStmt(clang::ASTContext &ctx) {
  return new (ctx) clang::BreakStmt(clang::SourceLocation());
}

clang::ParenExpr *CreateParenExpr(clang::ASTContext &ctx, clang::Expr *expr) {
  return new (ctx)
      clang::ParenExpr(clang::SourceLocation(), clang::SourceLocation(), expr);
}

clang::Expr *CreateNotExpr(clang::ASTContext &ctx, clang::Expr *op) {
  CHECK(op) << "No operand given for unary logical expression";
  return new (ctx) clang::UnaryOperator(
      CreateParenExpr(ctx, op), clang::UO_LNot, ctx.BoolTy, clang::VK_RValue,
      clang::OK_Ordinary, clang::SourceLocation());
}

clang::BinaryOperator *CreateBinaryOperator(clang::ASTContext &ast_ctx,
                                            clang::BinaryOperatorKind opc,
                                            clang::Expr *lhs, clang::Expr *rhs,
                                            clang::QualType res_type) {
  return new (ast_ctx)
      clang::BinaryOperator(lhs, rhs, opc, res_type, clang::VK_RValue,
                            clang::OK_Ordinary, clang::SourceLocation(),
                            /*fpContractable=*/false);
}

clang::Expr *CreateAndExpr(clang::ASTContext &ctx, clang::Expr *lhs,
                           clang::Expr *rhs) {
  return CreateBoolBinOp(ctx, clang::BO_LAnd, lhs, rhs);
}

clang::Expr *CreateOrExpr(clang::ASTContext &ctx, clang::Expr *lhs,
                          clang::Expr *rhs) {
  return CreateBoolBinOp(ctx, clang::BO_LOr, lhs, rhs);
}

clang::Expr *CreateTrueExpr(clang::ASTContext &ctx) {
  return clang::IntegerLiteral::Create(
      ctx, llvm::APInt(1, 1), ctx.UnsignedIntTy, clang::SourceLocation());
}

}  // namespace fcd