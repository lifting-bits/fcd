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

#ifndef FCD_AST_UTIL_H_
#define FCD_AST_UTIL_H_

#include <clang/AST/ASTContext.h>
#include <clang/AST/Expr.h>

#include <unordered_map>

namespace fcd {

using StmtMap = std::unordered_map<clang::Stmt *, clang::Stmt *>;

void ReplaceChildren(clang::Stmt *stmt, StmtMap &repl_map);

clang::IdentifierInfo *CreateIdentifier(clang::ASTContext &ctx,
                                        std::string name);

clang::DeclRefExpr *CreateDeclRefExpr(clang::ASTContext &ast_ctx,
                                      clang::ValueDecl *val);

clang::CompoundStmt *CreateCompoundStmt(clang::ASTContext &ctx,
                                        std::vector<clang::Stmt *> &stmts);

clang::IfStmt *CreateIfStmt(clang::ASTContext &ctx, clang::Expr *cond,
                            clang::Stmt *then);

clang::WhileStmt *CreateWhileStmt(clang::ASTContext &ctx, clang::Expr *cond,
                                  clang::Stmt *body);

clang::DoStmt *CreateDoStmt(clang::ASTContext &ctx, clang::Expr *cond,
                            clang::Stmt *body);

clang::BreakStmt *CreateBreakStmt(clang::ASTContext &ctx);

clang::DeclRefExpr *CreateDeclRefExpr(clang::ASTContext &ast_ctx,
                                      clang::ValueDecl *val);

clang::ParenExpr *CreateParenExpr(clang::ASTContext &ctx, clang::Expr *expr);

clang::Expr *CreateNotExpr(clang::ASTContext &ctx, clang::Expr *op);

clang::Expr *CreateAndExpr(clang::ASTContext &ctx, clang::Expr *lhs,
                           clang::Expr *rhs);

clang::Expr *CreateOrExpr(clang::ASTContext &ctx, clang::Expr *lhs,
                          clang::Expr *rhs);

clang::BinaryOperator *CreateBinaryOperator(clang::ASTContext &ast_ctx,
                                            clang::BinaryOperatorKind opc,
                                            clang::Expr *lhs, clang::Expr *rhs,
                                            clang::QualType res_type);

clang::Expr *CreateTrueExpr(clang::ASTContext &ctx);

}  // namespace fcd

#endif  // FCD_AST_UTIL_H_