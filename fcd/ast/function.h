//
// function.h
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#ifndef fcd__ast_function_h
#define fcd__ast_function_h

#include "ast_context.h"
#include "statements.h"

#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>

#include <list>
#include <unordered_map>

// The FunctionNode's lifetime is tied to the lifetime of its memory pool
// (because the lifetime of almost everything it contains is), but it is not
// itself intended to be allocated through the DumbAllocator interface.
// FunctionNode needs more complex data structures, and thus has a non-trivial
// destructor.
class FunctionNode {
 private:
  llvm::Function* function;
  AstContext context;
  StatementReference body;

 public:
  FunctionNode(llvm::Function& fn)
      : function(&fn), context(fn.getParent()) {}

  AstContext& getContext() { return context; }
  llvm::Function& getFunction() { return *function; }

  llvm::Type& getReturnType() const { return *function->getReturnType(); }

  StatementList& getBody() { return *body; }
  bool hasBody() const { return !body->empty(); }

  void print(llvm::raw_ostream& os);
  void dump() const;
};

#endif /* fcd__ast_function_h */
