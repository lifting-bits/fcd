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

#include <iostream>
#include <memory>
#include <sstream>
#include <unordered_map>

#include "remill/BC/ABI.h"
#include "remill/BC/Util.h"

#include "fcd/pass_argrec_remill.h"

namespace fcd {

namespace {

static llvm::Type* AnalyzeRetType(llvm::Function* func) {
  for (auto& block : *func) {
    if (auto ret = llvm::dyn_cast<llvm::ReturnInst>(block.getTerminator())) {
      for (auto& inst : block) {
        // if (auto load = llvm::dyn_cast<llvm::StoreInst>(inst)){

        // }
      }
    }
  }
  auto module = func->getParent();
  return remill::MemoryPointerType(module);
}

static llvm::Function* DeclareParametrizedFunc(llvm::Function* func,
                                               CallingConvention& cc) {
  auto cc_reg_vars = cc.CallConvRegVars();

  std::vector<llvm::Type*> params;
  for (auto reg_name : cc_reg_vars) {
    params.push_back(remill::FindVarInFunction(func, reg_name)->getType());
  }

  std::stringstream ss;
  ss << "argrec_" << func->getName().str();

  auto module = func->getParent();

  auto ret_type = AnalyzeRetType(func);
  auto cc_type = llvm::FunctionType::get(ret_type, params, false);
  auto cc_func = llvm::dyn_cast<llvm::Function>(
      module->getOrInsertFunction(ss.str(), cc_type));

  CHECK(cc_func != nullptr);

  for (auto& arg : cc_func->args()) {
    arg.setName(cc_reg_vars[arg.getArgNo()]);
  }

  return cc_func;
}

static void LiftedArgsToLocals(llvm::Function* func) {
  auto loc_state = remill::FindVarInFunction(func, "state");
  auto arg_state = remill::NthArgument(func, remill::kStatePointerArgNum);
  arg_state->replaceAllUsesWith(loc_state);

  auto loc_pc = remill::FindVarInFunction(func, "curr_pc");
  auto arg_pc = remill::NthArgument(func, remill::kPCArgNum);
  arg_pc->replaceAllUsesWith(loc_pc);

  auto loc_mem = remill::FindVarInFunction(func, "memory");
  auto arg_mem = remill::NthArgument(func, remill::kMemoryPointerArgNum);
  arg_mem->replaceAllUsesWith(loc_mem);
}

static bool IsLiftedFunction(llvm::Function* func) {
  auto module = func->getParent();
  if (func->getFunctionType() == remill::LiftedFunctionType(module)) {
    if (!func->getName().startswith("__remill")) {
      return !func->empty();
    }
  }

  return false;
}

}  // namespace

char RemillArgumentRecovery::ID = 0;

RemillArgumentRecovery::RemillArgumentRecovery()
    : ModulePass(RemillArgumentRecovery::ID),
      cc(CallingConvention(remill::GetTargetArch()->DefaultCallingConv())) {}

void RemillArgumentRecovery::getAnalysisUsage(
    llvm::AnalysisUsage& usage) const {}

bool RemillArgumentRecovery::runOnModule(llvm::Module& module) {
  for (auto& func : module) {
    if (IsLiftedFunction(&func)) {
      auto cc_func = DeclareParametrizedFunc(&func, cc);
      LiftedArgsToLocals(&func);
      remill::ValueMap value_map;
      remill::CloneFunctionInto(&func, cc_func, value_map);
      cc_func->dump();
    }
  }

  return true;
}

llvm::ModulePass* createRemillArgumentRecoveryPass() {
  return new RemillArgumentRecovery;
}

static llvm::RegisterPass<RemillArgumentRecovery> remill_argrec(
    "remill_argrec", "Remill's Argument Recovery", true, false);

}  // namespace fcd

// using namespace llvm;
// using RemillArgumentRecovery = fcd::RemillArgumentRecovery;
// INITIALIZE_PASS(RemillArgumentRecovery, "remill_argrec",
//                 "Remill's Argument Recovery", true, false)
