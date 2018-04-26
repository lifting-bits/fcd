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

static std::string sPrefix;
static const remill::Arch *sArch;

static std::unordered_set<const char *> RegisterAliasSet(const char *reg) {
  static const char *AMD64Aliases[][9] = {
      {"AL", "AH", "AX", "EAX", "RAX", nullptr},
      {"CL", "CH", "CX", "ECX", "RCX", nullptr},
      {"DL", "DH", "DX", "EDX", "RDX", nullptr},
      {"SIL", "SI", "ESI", "RSI", nullptr},
      {"DIL", "DI", "EDI", "RDI", nullptr},
      {"SPL", "SP", "ESP", "RSP", nullptr},
      {"R8B", "R8W", "R8D", "R8", nullptr},
      {"R9B", "R9W", "R9D", "R9", nullptr},
      {nullptr}};

  static const char *AArch64Aliases[][35] = {{"W0", "X0", nullptr},
                                             {"W1", "X1", nullptr},
                                             {"W3", "X3", nullptr},
                                             {"W3", "X3", nullptr},
                                             {"W4", "X4", nullptr},
                                             {"W5", "X5", nullptr},
                                             {"W6", "X6", nullptr},
                                             {"W7", "X7", nullptr},
                                             {"W8", "X8", nullptr},
                                             {"W9", "X9", nullptr},
                                             {"W10", "X10", nullptr},
                                             {"X11", "X11", nullptr},
                                             {"W12", "X12", nullptr},
                                             {"W13", "X13", nullptr},
                                             {"W14", "X14", nullptr},
                                             {"W15", "X15", nullptr},
                                             {"W16", "X16", nullptr},
                                             {"W17", "X17", nullptr},
                                             {"W18", "X18", nullptr},
                                             {"W19", "X19", nullptr},
                                             {"W20", "X20", nullptr},
                                             {"W21", "X21", nullptr},
                                             {"W22", "X22", nullptr},
                                             {"W23", "X23", nullptr},
                                             {"W24", "X24", nullptr},
                                             {"W25", "X25", nullptr},
                                             {"W26", "X26", nullptr},
                                             {"W27", "X27", nullptr},
                                             {"W28", "X28", nullptr},
                                             {"W29", "X29", nullptr},
                                             {"W30", "X30", nullptr},
                                             {"W31", "X31", nullptr},
                                             {"WZR", "ZR", nullptr},
                                             {"WSP", "SP", nullptr},
                                             {nullptr}};

  auto GetAlias = [&](unsigned i, unsigned j) {
    return sArch->IsAMD64() ? AMD64Aliases[i][j] : AArch64Aliases[i][j];
  };

  auto GetAliasSet = [&](unsigned i) {
    return sArch->IsAMD64() ? AMD64Aliases[i] : AArch64Aliases[i];
  };

  static std::unordered_map<const char *, const char **> resolver;
  if (resolver.empty()) {
    for (unsigned i = 0; GetAlias(i, 0) != nullptr; ++i) {
      for (unsigned j = 0; GetAlias(i, j) != nullptr; ++j) {
        resolver[GetAlias(i, j)] = GetAliasSet(i);
      }
    }
  }

  std::unordered_set<const char *> result({reg});
  auto iter = resolver.find(reg);
  if (iter != resolver.end()) {
    for (unsigned i = 0; iter->second[i] != nullptr; ++i) {
      result.insert(iter->second[i]);
    }
  }

  return result;
}

static std::unordered_set<llvm::User *> UsersOfVar(llvm::Function *func,
                                                   llvm::Value *var) {
  std::unordered_set<llvm::User *> result;
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
static llvm::User *FirstUserOfVar(llvm::Function *func, llvm::Value *var,
                                  T &instruction_list) {
  auto users = UsersOfVar(func, var);
  if (!users.empty()) {
    for (auto &inst : instruction_list) {
      if (users.count(&inst) > 0) {
        return &inst;
      }
    }
  }
  return nullptr;
}

template <typename T>
static std::pair<llvm::User *, const char *> FirstUserOfReg(
    llvm::Function *func, const char *reg, T &instruction_list) {
  std::unordered_map<llvm::User *, const char *> users;
  for (auto alias : RegisterAliasSet(reg)) {
    auto var = remill::FindVarInFunction(func, alias);
    if (auto user = FirstUserOfVar(func, var, instruction_list)) {
      users[user] = alias;
    }
  }

  if (!users.empty()) {
    for (auto &inst : instruction_list) {
      auto it = users.find(&inst);
      if (it != users.end()) {
        return *it;
      }
    }
  }
  return {nullptr, nullptr};
}

static std::vector<llvm::BasicBlock *> TerminalBlocksOf(llvm::Function *func) {
  std::vector<llvm::BasicBlock *> result;
  for (auto &block : *func) {
    if (llvm::isa<llvm::ReturnInst>(block.getTerminator())) {
      result.push_back(&block);
    }
  }
  return result;
}

// This function looks at blocks that have a llvm::ReturnInst in them. Traverses
// them backwards and looks for a first use of an alias of a return regiser
// defined by the callconv. If the user is a store, then the type of the value
// stored into the register is declared a candidate for the return type of the
// function. If all found candidates are of the same type, the type is declared
// the return type of the function. Otherwise the return type is set to void.
static llvm::Type *RecoverRetType(llvm::Function *func, CallingConvention &cc) {
  std::unordered_set<llvm::Type *> found_types;
  auto ret_regs = cc.ReturnRegs();
  for (auto block : TerminalBlocksOf(func)) {
    auto ilist = llvm::make_range(block->rbegin(), block->rend());
    for (auto reg : ret_regs) {
      auto user = FirstUserOfReg(func, reg, ilist);
      if (user.first != nullptr) {
        if (auto store = llvm::dyn_cast<llvm::StoreInst>(user.first)) {
          found_types.insert(store->getValueOperand()->getType());
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

static void LoadReturnRegToRetInsts(llvm::Function *func,
                                    CallingConvention &cc) {
  llvm::IRBuilder<> ir(func->getContext());
  auto ret_type = func->getReturnType();
  if (!ret_type->isVoidTy()) {
    auto reg = cc.ReturnRegForType(ret_type);
    auto var = remill::FindVarInFunction(func, reg);
    for (auto block : TerminalBlocksOf(func)) {
      auto term = block->getTerminator();
      ir.SetInsertPoint(term);
      auto val = ir.CreateLoad(ir.CreateLoad(var));
      ir.CreateRet(val);
      term->eraseFromParent();
    }
  } else {
    for (auto block : TerminalBlocksOf(func)) {
      auto term = block->getTerminator();
      ir.SetInsertPoint(term);
      ir.CreateRetVoid();
      term->eraseFromParent();
    }
  }
}

static std::string TrimPrefix(std::string str) {
  auto ref = llvm::StringRef(str);
  if (ref.startswith(sPrefix)) {
    ref = ref.drop_front(sPrefix.size());
  }
  return ref.str();
}

static void UpdateCalls(llvm::Function *old_func, llvm::Function *new_func,
                        CallingConvention &cc) {
  llvm::IRBuilder<> ir(new_func->getContext());
  for (auto old_call : remill::CallersOf(old_func)) {
    auto caller = old_call->getFunction();
    if (caller->getName().startswith(sPrefix)) {
      ir.SetInsertPoint(old_call);
      std::vector<llvm::Value *> params;
      for (auto &arg : new_func->args()) {
        auto name = TrimPrefix(arg.getName().str());
        auto arg_var = remill::FindVarInFunction(caller, name);
        params.push_back(ir.CreateLoad(ir.CreateLoad(arg_var)));
      }

      auto new_call = ir.CreateCall(new_func, params);
      auto ret_type = new_func->getReturnType();

      if (!ret_type->isVoidTy()) {
        auto ret_reg = cc.ReturnRegForType(ret_type);
        auto ret_var = remill::FindVarInFunction(caller, ret_reg);
        ir.CreateStore(new_call, ir.CreateLoad(ret_var));
      }
      old_call->replaceAllUsesWith(
          old_call->getArgOperand(remill::kMemoryPointerArgNum));
      old_call->eraseFromParent();
    }
  }
}

static llvm::Function *DeclareParametrizedFunc(llvm::Function *func,
                                               CallingConvention &cc) {
  std::vector<const char *> used_regs;
  // Get parameter regs from the callconv. Also add the stack pointer reg,
  // since it's used to access parameters passed by stack. Also add aliases.
  auto cc_regs = cc.ParamRegs();
  auto ilist = llvm::instructions(func);
  cc_regs.insert(cc_regs.begin(), cc.StackPointerVarName());
  for (auto reg : cc_regs) {
    auto user = FirstUserOfReg(func, reg, ilist);
    if (user.first != nullptr) {
      if (llvm::isa<llvm::LoadInst>(user.first)) {
        used_regs.push_back(user.second);
      }
    }
  }

  // Gather parameter types from register variable alloca's
  std::vector<llvm::Type *> params;
  for (auto reg : used_regs) {
    auto var = remill::FindVarInFunction(func, reg);
    auto inst = llvm::dyn_cast<llvm::AllocaInst>(var);
    CHECK(inst != nullptr);
    auto type = llvm::dyn_cast<llvm::PointerType>(inst->getAllocatedType());
    CHECK(type != nullptr);
    params.push_back(type->getElementType());
  }

  auto ret = RecoverRetType(func, cc);

  std::stringstream cc_func_name;
  cc_func_name << sPrefix << func->getName().str();

  auto cc_type = llvm::FunctionType::get(ret, params, false);
  auto cc_func = llvm::dyn_cast<llvm::Function>(
      func->getParent()->getOrInsertFunction(cc_func_name.str(), cc_type));

  CHECK(cc_func != nullptr);

  for (auto &arg : cc_func->args()) {
    std::stringstream cc_arg_name;
    cc_arg_name << sPrefix << used_regs[arg.getArgNo()];
    arg.setName(cc_arg_name.str());
  }

  return cc_func;
}

static void StoreRegArgsToLocals(llvm::Function *func) {
  llvm::IRBuilder<> ir(func->getEntryBlock().getTerminator());
  for (auto &arg : func->args()) {
    auto name = TrimPrefix(arg.getName().str());
    auto var = remill::FindVarInFunction(func, name);
    auto ptr = ir.CreateLoad(var);
    ir.CreateStore(&arg, ptr);
  }
}

static void ConvertRemillArgsToLocals(llvm::Function *func) {
  auto module = func->getParent();
  auto &entry = func->getEntryBlock();

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

static bool IsLiftedFunction(llvm::Function *func) {
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

RemillArgumentRecovery::RemillArgumentRecovery(void)
    : ModulePass(RemillArgumentRecovery::ID),
      cc(CallingConvention(remill::GetTargetArch()->DefaultCallingConv())) {
  sArch = remill::GetTargetArch();
  sPrefix = cPrefix;
}

void RemillArgumentRecovery::getAnalysisUsage(
    llvm::AnalysisUsage &usage) const {}

bool RemillArgumentRecovery::runOnModule(llvm::Module &module) {
  std::vector<llvm::Function *> new_funcs;
  for (auto &func : module) {
    if (IsLiftedFunction(&func)) {
      // Recover the argument and return types of `func` by
      // analyzing it's body and looking for the usage of
      // registers that are declared by `cc` as parameter
      // and return registers. Then declare a new function
      // with the recovered argument and return types.
      auto cc_func = DeclareParametrizedFunc(&func, cc);

      // Converts remill's `State`, `pc` and `Memory` args
      // to local variables so that they make room for
      // recovered register arguments.
      ConvertRemillArgsToLocals(&func);

      // Clone the old function to the newly created one
      remill::ValueMap val_map;
      remill::CloneFunctionInto(&func, cc_func, val_map);

      // Create local variables for every argument in
      // `cc_func` and store parameter values to them.
      StoreRegArgsToLocals(cc_func);

      // Creates parametrized `ret` instructions based on
      // the function's return type and loads a value
      // from a return register for that type. The calling
      // convention defines which return register to use.
      LoadReturnRegToRetInsts(cc_func, cc);

      new_funcs.push_back(cc_func);
    }
  }

  for (auto func : new_funcs) {
    // Replace all uses of the old function with the new
    // one with the recovered parameter and return types.
    // Then delete the old function from the module.
    auto name = TrimPrefix(func->getName());
    auto old_func = remill::FindFunction(&module, name);
    UpdateCalls(old_func, func, cc);
    for (auto &arg : func->args()) {
      auto arg_name = TrimPrefix(arg.getName());
      auto var = remill::FindVarInFunction(func, arg_name);
      arg.takeName(var);
      arg.removeAttr(
          llvm::AttributeSet::get(arg.getContext(), arg.getArgNo() + 1,
                                  {llvm::Attribute::Dereferenceable}));
    }
    func->takeName(old_func);
    old_func->replaceAllUsesWith(llvm::UndefValue::get(old_func->getType()));
    old_func->eraseFromParent();
  }

  return true;
}

llvm::ModulePass *createRemillArgumentRecoveryPass(void) {
  return new RemillArgumentRecovery;
}

static llvm::RegisterPass<RemillArgumentRecovery> remill_argrec(
    "remill_argrec", "Remill's Argument Recovery", true, false);

}  // namespace fcd

// using namespace llvm;
// using RemillArgumentRecovery = fcd::RemillArgumentRecovery;
// INITIALIZE_PASS(RemillArgumentRecovery, "remill_argrec",
//                 "Remill's Argument Recovery", true, false)
