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

#include <sstream>
#include <unordered_map>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/pass_intrinsics_remill.h"

namespace fcd {

namespace {

static llvm::Module *sModule;

static std::string TrimPrefix(std::string pref, std::string str) {
  auto ref = llvm::StringRef(str);
  if (ref.startswith(pref)) {
    ref = ref.drop_front(pref.size());
  }
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
    : ModulePass(RemillFixIntrinsics::ID) {}

void RemillFixIntrinsics::getAnalysisUsage(llvm::AnalysisUsage &usage) const {}

bool RemillFixIntrinsics::runOnModule(llvm::Module &module) {
  sModule = &module;
  std::unordered_map<llvm::Function *, llvm::Function *> funcs;
  // Create "__fcd*" replacements for "__remill*" intrinsics
  for (auto &func : module) {
    if (IsRemillIntrinsicWithUse(&func)) {
      // Gather non-lifting arguments from `func`.
      std::vector<llvm::Type *> arg_types;
      for (auto &arg : func.args()) {
        auto arg_type = arg.getType();
        if (!IsRemillLiftingArgType(arg_type)) {
          arg_types.push_back(arg_type);
        }
      }
      // Determine return type of `new_func`
      // Replace `Memory` with `void`.
      auto ret_type = func.getReturnType();
      if (IsRemillLiftingArgType(ret_type)) {
        ret_type = llvm::Type::getVoidTy(module.getContext());
      }
      // Create the type for `new_func`
      auto func_type = llvm::FunctionType::get(ret_type, arg_types, false);
      // Create the name for `new_func`
      // This simply replaces the "__remill" prefix with "__fcd"
      std::stringstream ss;
      ss << "__fcd" << TrimPrefix("__remill", func.getName().str());
      // Create `new_func`
      auto new_func = llvm::dyn_cast<llvm::Function>(
          module.getOrInsertFunction(ss.str(), func_type));
      CHECK(new_func != nullptr);
      // Map `new_func` to the old `func` for later use.
      funcs[&func] = new_func;
    }
  }
  // Replace calls to "__remill*" intrinsics with
  // calls to "__fcd*" replacements.
  llvm::IRBuilder<> ir(module.getContext());
  for (auto item : funcs) {
    auto old_func = item.first;
    auto new_func = item.second;
    for (auto call : remill::CallersOf(old_func)) {
      // Gather non-lifting parameter values
      std::vector<llvm::Value *> params;
      for (auto &arg : call->arg_operands()) {
        auto arg_type = arg->getType();
        if (!IsRemillLiftingArgType(arg_type)) {
          params.push_back(arg);
        }
      }
      // Create a call to the "__fcd*" replacement
      ir.SetInsertPoint(call);
      auto new_call = ir.CreateCall(new_func, params);
      auto mem_ptr = remill::MemoryPointerType(&module);
      // Replace the "__remill*" intrinsic call return value
      // to either `undef` if the return type is a `Memory`
      // pointer or the return value of the new call otherwise.
      if (call->getType() == mem_ptr) {
        call->replaceAllUsesWith(llvm::UndefValue::get(mem_ptr));
      } else {
        call->replaceAllUsesWith(new_call);
      }
      call->eraseFromParent();
    }
    // Remove the original "_remill*" intrinsic
    old_func->replaceAllUsesWith(llvm::UndefValue::get(old_func->getType()));
    old_func->eraseFromParent();
  }
  return true;
}

llvm::ModulePass *createRemillFixIntrinsicsPass(void) {
  return new RemillFixIntrinsics;
}

static llvm::RegisterPass<RemillFixIntrinsics> remill_intrinsics(
    "remill_intrinsics", "Remill's Intrinsics Cleanup", true, false);

}  // namespace fcd

// using namespace llvm;
// using RemillFixIntrinsics = fcd::RemillFixIntrinsics;
// INITIALIZE_PASS(RemillFixIntrinsics, "remill_intrinsics",
//                 "Remill's Intrinsics Cleanup", true, false)
