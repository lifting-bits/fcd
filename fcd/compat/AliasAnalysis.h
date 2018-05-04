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

#ifndef FCD_COMPAT_ALIASANALYSIS_H_
#define FCD_COMPAT_ALIASANALYSIS_H_

#include <llvm/Analysis/AliasAnalysis.h>

#include "remill/BC/Version.h"

#if LLVM_VERSION_NUMBER <= LLVM_VERSION(3, 6)

#include <llvm/Pass.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Target/TargetLibraryInfo.h>

#elif LLVM_VERSION_NUMBER >= LLVM_VERSION(3, 7)

#include <llvm/Analysis/TargetLibraryInfo.h>

#endif

#if LLVM_VERSION_NUMBER <= LLVM_VERSION(3, 7)

namespace llvm {

#if LLVM_VERSION_NUMBER <= LLVM_VERSION(3, 6)

using MemoryLocation = llvm::AliasAnalysis::Location;
using AliasResult = llvm::AliasAnalysis::AliasResult;
using AliasResult::MayAlias;
using AliasResult::NoAlias;
using TargetLibraryInfoWrapperPass = llvm::TargetLibraryInfo;

#endif

class AAResults {
 public:
  template <typename T>
  void addAAResult(T &AAResult) {}
};

template <class C>
class AAResultBase {
 public:
  AAResultBase(const llvm::TargetLibraryInfo &TLI) {}
  virtual llvm::AliasResult alias(const llvm::MemoryLocation &LocA,
                                  const llvm::MemoryLocation &LocB) = 0;
};

}  // namespace llvm

using AliasAnalysis = llvm::AliasAnalysis;

#else

class AliasAnalysis {
 public:
  AliasAnalysis() {}
  virtual llvm::AliasResult alias(const llvm::MemoryLocation &LocA,
                                  const llvm::MemoryLocation &LocB) {
    return llvm::MayAlias;
  }
};

#endif

#endif  // FCD_COMPAT_ALIASANALYSIS_H_
