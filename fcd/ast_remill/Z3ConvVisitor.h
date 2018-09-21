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

#ifndef FCD_AST_Z3CONVVISITOR_H_
#define FCD_AST_Z3CONVVISITOR_H_

#include <clang/AST/RecursiveASTVisitor.h>

#include <z3++.h>

#include <unordered_map>

namespace fcd {

class Z3ConvVisitor
    : public clang::RecursiveASTVisitor<Z3ConvVisitor> {
 private:
    clang::ASTContext *ast_ctx;
    z3::context *z3_ctx;
    
    std::unordered_map<clang::Expr *, z3::expr> z3_exprs;
    std::unordered_map<unsigned, clang::Expr *> c_exprs;

 public:
    z3::expr GetOrCreateZ3Expr(clang::Expr *expr);
    clang::Expr *GetOrCreateCExpr(z3::expr expr);

    Z3ConvVisitor(clang::ASTContext *c_ctx, z3::context *z_ctx);

    bool shouldTraversePostOrder() { return true; }

    bool VisitUnaryOperator(clang::UnaryOperator *op);
    bool VisitBinaryOperator(clang::BinaryOperator *op);
    bool VisitDeclRefExpr(clang::DeclRefExpr *ref);
    bool VisitIntegerLiteral(clang::IntegerLiteral *lit);
};

}  // namespace fcd

#endif  // FCD_AST_Z3CONVVISITOR_H_
