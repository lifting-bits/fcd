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

#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/ASTMatchers/ASTMatchers.h>

#include "fcd/ast_remill/LoopRefine.h"

namespace fcd {

namespace {

using namespace clang::ast_matchers;

template <typename StmtFromTy, typename StmtToTy>
class InferenceRule : public MatchFinder::MatchCallback {
 protected:
  StatementMatcher cond;
  const StmtFromTy *match;
  StmtToTy *substitution;

 public:
  InferenceRule(StatementMatcher matcher)
      : cond(matcher), match(nullptr), substitution(nullptr) {}

  const StatementMatcher &GetCondition() const { return cond; }
  operator bool() { return match; }

  virtual StmtToTy *GetOrCreateSubstitution(clang::ASTContext &ctx,
                                            StmtFromTy *stmt) = 0;
};

// Matches `while(1)`, `if(1)`, etc.
static const auto cond_true = hasCondition(integerLiteral(equals(true)));
// Matches `{ break; }`
static const auto comp_break =
    compoundStmt(has(breakStmt()), statementCountIs(1));

class WhileRule : public InferenceRule<clang::WhileStmt, clang::WhileStmt> {
 public:
  WhileRule()
      : InferenceRule(forEachDescendant(ifStmt(
            stmt().bind("if"), hasThen(comp_break),
            hasParent(compoundStmt(
                stmt().bind("body"),
                hasParent(whileStmt(stmt().bind("while"), cond_true))))))) {}

  void run(const MatchFinder::MatchResult &result) {
    auto if_break = result.Nodes.getNodeAs<clang::IfStmt>("if");
    auto loop_body = result.Nodes.getNodeAs<clang::CompoundStmt>("body");
    if (loop_body->body_front() == if_break) {
      match = result.Nodes.getNodeAs<clang::WhileStmt>("while");
    }
  }

  clang::WhileStmt *GetOrCreateSubstitution(clang::ASTContext &ctx,
                                            clang::WhileStmt *loop) {
    CHECK(loop == match)
        << "Substituted WhileStmt is not the matched WhileStmt!";
    auto comp = llvm::cast<clang::CompoundStmt>(loop->getBody());
    auto cond = llvm::cast<clang::IfStmt>(comp->body_back())->getCond();
    std::vector<clang::Stmt *> new_body(comp->body_begin() + 1,
                                        comp->body_end());

    return CreateWhileStmt(ctx, CreateNotExpr(ctx, cond),
                           CreateCompoundStmt(ctx, new_body));
  }
};

class DoWhileRule : public InferenceRule<clang::WhileStmt, clang::DoStmt> {
 public:
  DoWhileRule()
      : InferenceRule(forEachDescendant(ifStmt(
            stmt().bind("if"), hasThen(comp_break),
            hasParent(compoundStmt(
                stmt().bind("body"),
                hasParent(whileStmt(stmt().bind("while"), cond_true))))))) {}

  void run(const MatchFinder::MatchResult &result) {
    auto if_break = result.Nodes.getNodeAs<clang::IfStmt>("if");
    auto loop_body = result.Nodes.getNodeAs<clang::CompoundStmt>("body");
    if (loop_body->body_back() == if_break) {
      match = result.Nodes.getNodeAs<clang::WhileStmt>("while");
    }
  }

  clang::DoStmt *GetOrCreateSubstitution(clang::ASTContext &ctx,
                                         clang::WhileStmt *loop) {
    CHECK(loop == match)
        << "Substituted WhileStmt is not the matched WhileStmt!";
    auto comp = llvm::cast<clang::CompoundStmt>(loop->getBody());
    auto cond = llvm::cast<clang::IfStmt>(comp->body_back())->getCond();
    std::vector<clang::Stmt *> new_body(comp->body_begin(),
                                        comp->body_end() - 1);

    return CreateDoStmt(ctx, CreateNotExpr(ctx, cond),
                        CreateCompoundStmt(ctx, new_body));
  }
};

static const auto has_break = hasDescendant(breakStmt());

class CondToSeqRule : public InferenceRule<clang::WhileStmt, clang::WhileStmt> {
 public:
  CondToSeqRule()
      : InferenceRule(whileStmt(
            stmt().bind("while"), cond_true,
            hasBody(compoundStmt(
                has(ifStmt(hasThen(unless(has_break)), hasElse(has_break))),
                statementCountIs(1))))) {}

  void run(const MatchFinder::MatchResult &result) {
    match = result.Nodes.getNodeAs<clang::WhileStmt>("while");
  }

  clang::WhileStmt *GetOrCreateSubstitution(clang::ASTContext &ctx,
                                            clang::WhileStmt *loop) {
    CHECK(loop == match)
        << "Substituted WhileStmt is not the matched WhileStmt!";
    auto body = llvm::cast<clang::CompoundStmt>(loop->getBody());
    auto ifstmt = llvm::cast<clang::IfStmt>(body->body_front());
    auto inner_loop =
        CreateWhileStmt(ctx, ifstmt->getCond(), ifstmt->getThen());
    std::vector<clang::Stmt *> new_body({inner_loop});
    if (auto comp = llvm::dyn_cast<clang::CompoundStmt>(ifstmt->getElse())) {
      new_body.insert(new_body.end(), comp->body_begin(), comp->body_end());
    } else {
      new_body.push_back(ifstmt->getElse());
    }

    return CreateWhileStmt(ctx, loop->getCond(),
                           CreateCompoundStmt(ctx, new_body));
  }
};

class CondToSeqNegRule
    : public InferenceRule<clang::WhileStmt, clang::WhileStmt> {
 public:
  CondToSeqNegRule()
      : InferenceRule(whileStmt(
            stmt().bind("while"), cond_true,
            hasBody(compoundStmt(
                has(ifStmt(hasThen(has_break), hasElse(unless(has_break)))),
                statementCountIs(1))))) {}

  void run(const MatchFinder::MatchResult &result) {
    match = result.Nodes.getNodeAs<clang::WhileStmt>("while");
  }

  clang::WhileStmt *GetOrCreateSubstitution(clang::ASTContext &ctx,
                                            clang::WhileStmt *loop) {
    CHECK(loop == match)
        << "Substituted WhileStmt is not the matched WhileStmt!";
    auto body = llvm::cast<clang::CompoundStmt>(loop->getBody());
    auto ifstmt = llvm::cast<clang::IfStmt>(body->body_front());
    auto cond = CreateNotExpr(ctx, ifstmt->getCond());
    auto inner_loop = CreateWhileStmt(ctx, cond, ifstmt->getElse());
    std::vector<clang::Stmt *> new_body({inner_loop});
    if (auto comp = llvm::dyn_cast<clang::CompoundStmt>(ifstmt->getThen())) {
      new_body.insert(new_body.end(), comp->body_begin(), comp->body_end());
    } else {
      new_body.push_back(ifstmt->getThen());
    }

    return CreateWhileStmt(ctx, loop->getCond(),
                           CreateCompoundStmt(ctx, new_body));
  }
};

}  // namespace

char LoopRefine::ID = 0;

LoopRefine::LoopRefine(clang::CompilerInstance &ins,
                       fcd::IRToASTVisitor &ast_gen)
    : ModulePass(LoopRefine::ID),
      ast_ctx(&ins.getASTContext()),
      ast_gen(&ast_gen) {}

bool LoopRefine::VisitWhileStmt(clang::WhileStmt *loop) {
  // DLOG(INFO) << "VisitWhileStmt";
  clang::ast_matchers::MatchFinder finder;

  CondToSeqRule cond_to_seq_r;
  finder.addMatcher(cond_to_seq_r.GetCondition(), &cond_to_seq_r);

  CondToSeqNegRule cond_to_seqn_r;
  finder.addMatcher(cond_to_seqn_r.GetCondition(), &cond_to_seqn_r);

  WhileRule while_r;
  finder.addMatcher(while_r.GetCondition(), &while_r);

  DoWhileRule do_while_r;
  finder.addMatcher(do_while_r.GetCondition(), &do_while_r);

  finder.match(*loop, *ast_ctx);

  clang::Stmt *sub = nullptr;
  if (cond_to_seq_r) {
    sub = cond_to_seq_r.GetOrCreateSubstitution(*ast_ctx, loop);
  } else if (cond_to_seqn_r) {
    sub = cond_to_seqn_r.GetOrCreateSubstitution(*ast_ctx, loop);
  } else if (while_r) {
    sub = while_r.GetOrCreateSubstitution(*ast_ctx, loop);
  } else if (do_while_r) {
    sub = do_while_r.GetOrCreateSubstitution(*ast_ctx, loop);
  }

  if (sub) {
    substitutions[loop] = sub;
  }

  return true;
}

bool LoopRefine::runOnModule(llvm::Module &module) {
  LOG(INFO) << "Rule-based loop refinement";
  Initialize();
  TraverseDecl(ast_ctx->getTranslationUnitDecl());
  return changed;
}

llvm::ModulePass *createLoopRefinePass(clang::CompilerInstance &ins,
                                       fcd::IRToASTVisitor &gen) {
  return new LoopRefine(ins, gen);
}
}  // namespace fcd