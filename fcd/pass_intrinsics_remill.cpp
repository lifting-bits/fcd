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

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>

#include <map>
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/pass_intrinsics_remill.h"

namespace fcd {

namespace {

static const char *sPrefix;

}  // namespace

char RemillFixIntrinsics::ID = 0;

RemillFixIntrinsics::RemillFixIntrinsics(void)
    : ModulePass(RemillFixIntrinsics::ID) {
  sPrefix = cPrefix;
}

void RemillFixIntrinsics::getAnalysisUsage(llvm::AnalysisUsage &usage) const {}

bool RemillFixIntrinsics::runOnModule(llvm::Module &module) {
  return true;
}

llvm::ModulePass *createRemillFixIntrinsicsPass(void) {
  return new RemillFixIntrinsics;
}

static llvm::RegisterPass<RemillFixIntrinsics> remill_stackrec(
    "remill_intrinsics", "Remill's Intrinsics Cleanup", true, false);

}  // namespace fcd

using namespace llvm;
using RemillFixIntrinsics = fcd::RemillFixIntrinsics;
INITIALIZE_PASS(RemillFixIntrinsics, "remill_intrinsics",
                "Remill's Intrinsics Cleanup", true, false)
