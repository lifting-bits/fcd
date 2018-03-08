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
#include <unordered_map>
#include <unordered_set>

#include "remill/BC/ABI.h"
#include "remill/BC/Util.h"

#include "fcd/pass_argrec_remill.h"

namespace fcd {

namespace {

static const char* sPrefix;

static std::unordered_set<const char*> RegisterAliasSet(const char* reg) {
  static const char* aliases[][9] = {{"AL", "AH", "AX", "EAX", "RAX", nullptr},
                                     {"CL", "CH", "CX", "ECX", "RCX", nullptr},
                                     {"DL", "DH", "DX", "EDX", "RDX", nullptr},
                                     {"SIL", "SI", "ESI", "RSI", nullptr},
                                     {"DIL", "DI", "EDI", "RDI", nullptr},
                                     {"SPL", "SP", "ESP", "RSP", nullptr},
                                     {"R8B", "R8W", "R8D", "R8", nullptr},
                                     {"R9B", "R9W", "R9D", "R9", nullptr},
                                     {nullptr}};

  static std::unordered_map<const char*, const char**> resolver;
  if (resolver.empty()) {
    for (unsigned i = 0; aliases[i][0] != nullptr; ++i) {
      for (unsigned j = 0; aliases[i][j] != nullptr; ++j) {
        resolver[aliases[i][j]] = aliases[i];
      }
    }
  }

  std::unordered_set<const char*> result({reg});
  auto iter = resolver.find(reg);
  if (iter != resolver.end()) {
    for (unsigned i = 0; iter->second[i] != nullptr; ++i) {
      result.insert(iter->second[i]);
    }
  }

  return result;
}

static std::unordered_set<llvm::User*> UsersOfVar(llvm::Function* func,
                                                  llvm::Value* var) {
  std::unordered_set<llvm::User*> result;
  if (var->hasNUsesOrMore(2)) {
    for (auto var_user : var->users()) {
      if (auto addr = llvm::dyn_cast<llvm::LoadInst>(var_user)) {
        for (auto addr_user : addr->users()) {
          result.insert(addr_user);
        }
      }
    }
  }
  return result;
}

template <typename T>
static llvm::User* FirstUserOfVar(llvm::Function* func, llvm::Value* var,
                                  T& instruction_list) {
  auto users = UsersOfVar(func, var);
  if (!users.empty()) {
    for (auto& inst : instruction_list) {
      if (users.count(&inst) > 0) {
        return &inst;
      }
    }
  }
  return nullptr;
}

template <typename T>
static std::pair<llvm::User*, const char*> FirstUserOfReg(llvm::Function* func,
                                                          const char* reg,
                                                          T& instruction_list) {
  std::unordered_map<llvm::User*, const char*> users;
  for (auto alias : RegisterAliasSet(reg)) {
    auto var = remill::FindVarInFunction(func, alias);
    if (auto user = FirstUserOfVar(func, var, instruction_list)) {
      users[user] = alias;
    }
  }

  if (!users.empty()) {
    for (auto& inst : instruction_list) {
      auto it = users.find(&inst);
      if (it != users.end()) {
        return *it;
      }
    }
  }
  return {nullptr, nullptr};
}

static llvm::Type* AnalyzeRetType(llvm::Function* func, CallingConvention& cc) {
  std::unordered_set<llvm::Type*> found_types;
  auto ret_regs = cc.ReturnRegs();
  for (auto& block : *func) {
    if (llvm::isa<llvm::ReturnInst>(block.getTerminator())) {
      auto ilist = llvm::make_range(block.rbegin(), block.rend());
      for (auto reg : ret_regs) {
        auto user = FirstUserOfReg(func, reg, ilist);
        if (user.first != nullptr) {
          if (auto store = llvm::dyn_cast<llvm::StoreInst>(user.first)) {
            found_types.insert(store->getValueOperand()->getType());
          }
        }
      }
    }
  }
  // TODO: Check if the types are compatible
  // instead of demanding they are the same.
  CHECK(found_types.size() < 2);
  return found_types.empty() ? llvm::Type::getVoidTy(func->getContext())
                             : *found_types.begin();
}

static llvm::Function* DeclareParametrizedFunc(llvm::Function* func,
                                               CallingConvention& cc) {
  std::vector<const char*> used_regs;
  // Get parameter regs from the callconv. Also add the stack pointer reg,
  // since it's used to access parameters passed by stack. Also add aliases.
  auto cc_regs = cc.ParamRegs();
  auto ilist = llvm::instructions(func);
  cc_regs.push_back(cc.StackPointerVarName());
  for (auto reg : cc_regs) {
    auto user = FirstUserOfReg(func, reg, ilist);
    if (user.first != nullptr) {
      if (llvm::isa<llvm::LoadInst>(user.first)) {
        used_regs.push_back(user.second);
      }
    }
  }

  // Gather parameter types from register variable alloca's
  std::vector<llvm::Type*> params;
  for (auto reg : used_regs) {
    auto var = remill::FindVarInFunction(func, reg);
    auto inst = llvm::dyn_cast<llvm::AllocaInst>(var);
    CHECK(inst != nullptr);
    auto type = llvm::dyn_cast<llvm::PointerType>(inst->getAllocatedType());
    CHECK(type != nullptr);
    params.push_back(type->getElementType());
  }

  auto ret = AnalyzeRetType(func, cc);

  std::stringstream cc_func_name;
  cc_func_name << sPrefix << func->getName().str();

  auto cc_type = llvm::FunctionType::get(ret, params, false);
  auto cc_func = llvm::dyn_cast<llvm::Function>(
      func->getParent()->getOrInsertFunction(cc_func_name.str(), cc_type));

  CHECK(cc_func != nullptr);

  for (auto& arg : cc_func->args()) {
    std::stringstream cc_arg_name;
    cc_arg_name << sPrefix << used_regs[arg.getArgNo()];
    arg.setName(cc_arg_name.str());
  }

  return cc_func;
}

static void LiftedArgsToLocals(llvm::Function* func) {
  auto module = func->getParent();
  auto& entry = func->getEntryBlock();

  llvm::IRBuilder<> ir(&entry, entry.begin());

  auto mem_type = remill::MemoryPointerType(module)->getElementType();
  auto arg_mem = remill::NthArgument(func, remill::kMemoryPointerArgNum);
  auto loc_mem = ir.CreateAlloca(mem_type, nullptr, "loc_memory");
  arg_mem->replaceAllUsesWith(loc_mem);

  ir.SetInsertPoint(loc_mem);

  auto pc_type = remill::AddressType(module);
  auto arg_pc = remill::NthArgument(func, remill::kPCArgNum);
  auto loc_pc = ir.CreateAlloca(pc_type, nullptr, "loc_pc");
  arg_pc->replaceAllUsesWith(loc_pc);

  ir.SetInsertPoint(loc_pc);

  auto state_type = remill::StatePointerType(module)->getElementType();
  auto arg_state = remill::NthArgument(func, remill::kStatePointerArgNum);
  auto loc_state = ir.CreateAlloca(state_type, nullptr, "loc_state");
  arg_state->replaceAllUsesWith(loc_state);
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
      cc(CallingConvention(remill::GetTargetArch()->DefaultCallingConv())) {
  sPrefix = cPrefix;
}

void RemillArgumentRecovery::getAnalysisUsage(
    llvm::AnalysisUsage& usage) const {}

bool RemillArgumentRecovery::runOnModule(llvm::Module& module) {
  for (auto& func : module) {
    if (IsLiftedFunction(&func)) {
      // func.dump();
      auto cc_func = DeclareParametrizedFunc(&func, cc);

      LiftedArgsToLocals(&func);

      remill::ValueMap val_map;
      remill::CloneFunctionInto(&func, cc_func, val_map);

      llvm::IRBuilder<> ir(cc_func->getEntryBlock().getTerminator());
      for (auto& arg : cc_func->args()) {
        auto name = arg.getName();
        name.consume_front(cPrefix);
        auto var = remill::FindVarInFunction(cc_func, name.str());
        auto ptr = ir.CreateLoad(var);
        ir.CreateStore(&arg, ptr);
        var->setName("");
        arg.setName(name);
      }
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
