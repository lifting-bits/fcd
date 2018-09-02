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

#ifndef FCD_AST_ASTTOZ3EXPRVISITOR_H_
#define FCD_AST_ASTTOZ3EXPRVISITOR_H_

#include <clang/AST/RecursiveASTVisitor.h>

#include <z3++.h>

namespace fcd {

class ASTToZ3ExprVisitor
    : public clang::RecursiveASTVisitor<ASTToZ3ExprVisitor> {
 private:
    clang::ASTContext *ast_ctx;
    z3::context z3_ctx;

 public:
    ASTToZ3ExprVisitor(clang::ASTContext *ctx);
    bool VisitIfStmt(clang::IfStmt *stmt);
};

}  // namespace fcd

#endif  // FCD_AST_ASTTOZ3EXPRVISITOR_H_
