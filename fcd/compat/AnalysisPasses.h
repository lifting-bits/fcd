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

#ifndef FCD_COMPAT_ANALYSISPASSES_H_
#define FCD_COMPAT_ANALYSISPASSES_H_

#include <llvm/Analysis/Passes.h>

#include "remill/BC/Version.h"

#if LLVM_VERSION_NUMBER <= LLVM_VERSION(3, 7)

namespace llvm {

static llvm::Pass *createBasicAAWrapperPass(void) {
  return llvm::createBasicAliasAnalysisPass();
}

static llvm::Pass *createScopedNoAliasAAWrapperPass(void) {
#if LLVM_VERSION_NUMBER >= LLVM_VERSION(3, 6)
  return llvm::createScopedNoAliasAAPass();
#else
  return llvm::createNoAAPass();
#endif
}


static llvm::Pass *createTypeBasedAAWrapperPass(void) {
  return llvm::createTypeBasedAliasAnalysisPass();
}

template <typename T>
static llvm::Pass *createExternalAAWrapperPass(T Callback) {
  return llvm::createNoAAPass();
}

}  // namespace llvm

#else

#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>

#endif

#endif  // FCD_COMPAT_ANALYSISPASSES_H_
