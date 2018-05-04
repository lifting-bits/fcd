//
// pass_asaa.h
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#ifndef FCD_PASS_ASAA_H_
#define FCD_PASS_ASAA_H_

#include "fcd/compat/AliasAnalysis.h"

namespace fcd {

class AddressSpaceAAResult : public llvm::AAResultBase<AddressSpaceAAResult> {
  friend llvm::AAResultBase<AddressSpaceAAResult>;

 public:
  AddressSpaceAAResult(const llvm::TargetLibraryInfo* TLI = nullptr);

  bool invalidate(llvm::Function& func,
                  const llvm::PreservedAnalyses& analyses) {
    // Stateless.
    return false;
  }

  llvm::AliasResult alias(const llvm::MemoryLocation& a,
                          const llvm::MemoryLocation& b);
};

class AddressSpaceAAWrapperPass : public llvm::ImmutablePass,
                                  public AliasAnalysis {
 private:
  std::unique_ptr<AddressSpaceAAResult> result;

 public:
  static char ID;

  AddressSpaceAAWrapperPass() : ImmutablePass(ID) {}
  ~AddressSpaceAAWrapperPass() {}

  AddressSpaceAAResult& getResult() { return *result; }
  const AddressSpaceAAResult& getResult() const { return *result; }

  bool doInitialization(llvm::Module& module) override;
  bool doFinalization(llvm::Module& module) override;
  void getAnalysisUsage(llvm::AnalysisUsage& usage) const override;

  llvm::AliasResult alias(const llvm::MemoryLocation& a,
                          const llvm::MemoryLocation& b) override;
};

llvm::ImmutablePass* createAddressSpaceAliasAnalysis();

}  // namespace fcd

#endif  // FCD_PASS_ASAA_H_
