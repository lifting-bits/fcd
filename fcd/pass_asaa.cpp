//
// pass_asaa.cpp
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

// This file is borrowed and recycled from a patch from Justin Holewinski that
// never made it to the main repository.
// http://lists.cs.uiuc.edu/pipermail/llvm-commits/Week-of-Mon-20111010/129632.html

#include "fcd/pass_asaa.h"

namespace fcd {

llvm::AliasResult AddressSpaceAAResult::alias(const llvm::MemoryLocation& a,
                                              const llvm::MemoryLocation& b) {
  if (auto ptrty_a = llvm::dyn_cast<llvm::PointerType>(a.Ptr->getType())) {
    if (auto ptrty_b = llvm::dyn_cast<llvm::PointerType>(b.Ptr->getType())) {
      if (ptrty_a->getAddressSpace() != ptrty_b->getAddressSpace()) {
        return llvm::NoAlias;
      }
    }
  }
  return AAResultBase::alias(a, b);
}

char AddressSpaceAAWrapperPass::ID = 0;

bool AddressSpaceAAWrapperPass::doInitialization(llvm::Module& module) {
  result.reset(new AddressSpaceAAResult);
  return false;
}

bool AddressSpaceAAWrapperPass::doFinalization(llvm::Module& module) {
  result.reset();
  return false;
}

void AddressSpaceAAWrapperPass::getAnalysisUsage(
    llvm::AnalysisUsage& usage) const {
  usage.addRequired<llvm::TargetLibraryInfoWrapperPass>();
  usage.setPreservesAll();
}

llvm::ImmutablePass* createAddressSpaceAliasAnalysis() {
  return new AddressSpaceAAWrapperPass;
}

static llvm::RegisterPass<AddressSpaceAAWrapperPass> asaa(
    "asaa", "NoAlias for pointers in different address spaces", false, true);

}  // namespace fcd
