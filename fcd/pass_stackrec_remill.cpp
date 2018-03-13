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

#include <iostream>
#include <memory>
#include <sstream>

#include "remill/BC/Util.h"

#include "fcd/pass_stackrec_remill.h"

namespace fcd {

namespace {

static const char *sPrefix;

static std::string TrimPrefix(std::string str) {
  auto ref = llvm::StringRef(str);
  ref.consume_front(sPrefix);
  return ref.str();
}

}  // namespace

char RemillStackRecovery::ID = 0;

RemillStackRecovery::RemillStackRecovery()
    : ModulePass(RemillStackRecovery::ID) {
  sPrefix = cPrefix;
}

void RemillStackRecovery::getAnalysisUsage(llvm::AnalysisUsage &usage) const {}

bool RemillStackRecovery::runOnModule(llvm::Module &module) { 
  for (auto &func : module) {
    func.dump();
  }
  return true;
}

llvm::ModulePass *createRemillStackRecoveryPass() {
  return new RemillStackRecovery;
}

static llvm::RegisterPass<RemillStackRecovery> remill_stackrec(
    "remill_stackrec", "Remill's Stack Frame Recovery", true, false);

}  // namespace fcd

using namespace llvm;
using RemillStackRecovery = fcd::RemillStackRecovery;
INITIALIZE_PASS(RemillStackRecovery, "remill_stackrec",
                "Remill's Stack Frame Recovery", true, false)
