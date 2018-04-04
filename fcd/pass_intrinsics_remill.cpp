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
#include <map>
#include <sstream>
#include <unordered_map>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/pass_intrinsics_remill.h"

namespace fcd {

namespace {

static const char *sPrefix;
static llvm::Module *sModule;

static std::string TrimPrefix(std::string pref, std::string str) {
  auto ref = llvm::StringRef(str);
  ref.consume_front(pref);
  return ref.str();
}

static bool IsRemillIntrinsicWithUse(llvm::Function *func) {
  if (func->getName().startswith("__remill")) {
    if (func->isDeclaration()) {
      return func->hasNUsesOrMore(1);
    }
  }
  return false;
}

static bool IsRemillLiftingArgType(llvm::Type *type) {
  return type == remill::StatePointerType(sModule) ||
         type == remill::MemoryPointerType(sModule);
}

}  // namespace

char RemillFixIntrinsics::ID = 0;

RemillFixIntrinsics::RemillFixIntrinsics(void)
    : ModulePass(RemillFixIntrinsics::ID) {
  sPrefix = cPrefix;
}

void RemillFixIntrinsics::getAnalysisUsage(llvm::AnalysisUsage &usage) const {}

bool RemillFixIntrinsics::runOnModule(llvm::Module &module) {
  sModule = &module;
  std::unordered_map<llvm::Function *, llvm::Function *> funcs;
  for (auto &func : module) {
    if (IsRemillIntrinsicWithUse(&func)) {
      // std::cerr << "RemillFixIntrinsics: " << func.getName().str() <<
      // std::endl;

      std::vector<llvm::Type *> arg_types;
      for (auto &arg : func.args()) {
        auto arg_type = arg.getType();
        if (!IsRemillLiftingArgType(arg_type)) {
          arg_types.push_back(arg_type);
        }
      }

      auto ret_type = func.getReturnType();
      if (IsRemillLiftingArgType(ret_type)) {
        ret_type = llvm::Type::getVoidTy(module.getContext());
      }

      auto func_type = llvm::FunctionType::get(ret_type, arg_types, false);

      std::stringstream ss;
      ss << "__fcd" << TrimPrefix("__remill", func.getName().str());
      auto new_func = llvm::dyn_cast<llvm::Function>(
          module.getOrInsertFunction(ss.str(), func_type));

      CHECK(new_func != nullptr);

      funcs[&func] = new_func;
    }
  }

  llvm::IRBuilder<> ir(module.getContext());
  for (auto item : funcs) {
    auto old_func = item.first;
    auto new_func = item.second;
    for (auto call : remill::CallersOf(old_func)) {
      std::vector<llvm::Value *> params;
      for (auto &arg : call->arg_operands()) {
        auto arg_type = arg->getType();
        if (!IsRemillLiftingArgType(arg_type)) {
          params.push_back(arg);
        }
      }
      ir.SetInsertPoint(call);
      auto new_call = ir.CreateCall(new_func, params);
      auto memptrty = remill::MemoryPointerType(&module);
      if (call->getType() == memptrty) {
        call->replaceAllUsesWith(llvm::UndefValue::get(memptrty));
      } else {
        call->replaceAllUsesWith(new_call);
      }
      call->eraseFromParent();
    }
    old_func->replaceAllUsesWith(llvm::UndefValue::get(old_func->getType()));
    old_func->eraseFromParent();
  }
  // module.dump();
  return true;
}

llvm::ModulePass *createRemillFixIntrinsicsPass(void) {
  return new RemillFixIntrinsics;
}

static llvm::RegisterPass<RemillFixIntrinsics> remill_stackrec(
    "remill_intrinsics", "Remill's Intrinsics Cleanup", true, false);

}  // namespace fcd

// using namespace llvm;
// using RemillFixIntrinsics = fcd::RemillFixIntrinsics;
// INITIALIZE_PASS(RemillFixIntrinsics, "remill_intrinsics",
//                 "Remill's Intrinsics Cleanup", true, false)
