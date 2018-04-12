//
// pass_backend.h
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

//
// The algorithm used here is based off K. Yakdan, S. Eschweiler, E.
// Gerhards-Padilla and M. Smith's research paper "No More Gotos", accessible
// from the Internet Society's website:
// http://www.internetsociety.org/doc/no-more-gotos-decompilation-using-pattern-independent-control-flow-structuring-and-semantics
//

#ifndef FCD_AST_PASS_BACKEND_H_
#define FCD_AST_PASS_BACKEND_H_

#include <llvm/Analysis/DominanceFrontier.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>

#include <deque>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "function.h"
#include "pass.h"
#include "statements.h"

class PreAstContext;

// XXX Make this a legit LLVM backend?
// Doesn't sound like a bad idea, but I don't really know where to start.
class AstBackEnd : public llvm::ModulePass {
 private:
  std::unique_ptr<PreAstContext> blockGraph;
  std::deque<FunctionNode> nodes;
  std::deque<std::unique_ptr<AstModulePass>> passes;
  FunctionNode *output;

  void runOnFunction(llvm::Function &func);

 public:
  static char ID;

  AstBackEnd(void);

  llvm::StringRef getPassName(void) const override { return "AST Back-End"; }

  void getAnalysisUsage(llvm::AnalysisUsage &usage) const override;
  bool runOnModule(llvm::Module &module) override;

  void addPass(AstModulePass *pass);
};

AstBackEnd *createAstBackEnd(void);

namespace llvm {
void initializeAstBackEndPass(PassRegistry &);
}

#endif  // FCD_AST_PASS_BACKEND_H_
