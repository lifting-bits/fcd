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

#include "remill/BC/IntrinsicTable.h"
#include "remill/BC/Util.h"

#include "fcd/pass_stackrec_remill.h"

namespace fcd {

namespace {

static const char *sPrefix;

static std::vector<llvm::BinaryOperator *> FindStackAddrOps(
    llvm::Argument *sp, llvm::Function *func) {
  std::vector<llvm::BinaryOperator *> result;
  for (auto user : sp->users()) {
    if (auto op = llvm::dyn_cast<llvm::BinaryOperator>(user)) {
      auto idx =
          op->getOperand(0) == sp ? op->getOperand(1) : op->getOperand(0);
      if (llvm::isa<llvm::ConstantInt>(idx)) {
        switch (op->getOpcode()) {
          case llvm::Instruction::Add:
          case llvm::Instruction::Sub:
            result.push_back(op);
            break;
          default:
            break;
        }
      }
    }
  }
  return result;
}

static int64_t GetStackIdx(llvm::Argument *sp, llvm::BinaryOperator *op) {
  auto val = op->getOperand(0) == sp ? op->getOperand(1) : op->getOperand(0);
  auto idx = llvm::dyn_cast<llvm::ConstantInt>(val);
  CHECK(idx) << "Non-SP operand has to be a constant integer.";
  switch (op->getOpcode()) {
    case llvm::Instruction::Add:
      return idx->getSExtValue();
      break;
    case llvm::Instruction::Sub:
      return -idx->getSExtValue();
      break;
    default:
      CHECK(false) << "Invalid stack index bianry operation.";
      return 0;
      break;
  }
}

static std::vector<llvm::IntToPtrInst *> FindStackAddrCasts(
    llvm::BinaryOperator *op, unsigned pmem_addr_space) {
  std::vector<llvm::IntToPtrInst *> result;
  for (auto user : op->users()) {
    if (auto int2ptr = llvm::dyn_cast<llvm::IntToPtrInst>(user)) {
      if (int2ptr->getAddressSpace() == pmem_addr_space) {
        result.push_back(int2ptr);
      }
    }
  }
  return result;
}

// template <typename T>
// static void FindStackAccs(llvm::IntToPtrInst *ptr, std::vector<T *> &result) {
//   for (auto user : ptr->users()) {
//     if (auto acc = llvm::dyn_cast<T>(user)) {
//       switch (acc->getOpcode()) {
//         case llvm::Instruction::Load:
//         case llvm::Instruction::Store:
//           result.push_back(acc);
//           break;
//         default:
//           break;
//       }
//     }
//   }
// }

// static std::vector<llvm::StoreInst *> FindStackWrites(llvm::IntToPtrInst *ptr) {
//   std::vector<llvm::StoreInst *> result;
//   FindStackAccs(ptr, result);
//   return result;
// }

// static std::vector<llvm::LoadInst *> FindStackReads(llvm::IntToPtrInst *ptr) {
//   std::vector<llvm::LoadInst *> result;
//   FindStackAccs(ptr, result);
//   return result;
// }

static bool IsRecoverable(llvm::Function *func, CallingConvention &cc) {
  auto sp_name = cc.StackPointerVarName();
  if (func->arg_size() > 0) {
    if (remill::NthArgument(func, 0)->getName() == sp_name) {
      return !func->empty();
    }
  }
  return false;
}

}  // namespace

char RemillStackRecovery::ID = 0;

RemillStackRecovery::RemillStackRecovery()
    : ModulePass(RemillStackRecovery::ID),
      cc(CallingConvention(remill::GetTargetArch()->DefaultCallingConv())) {
  sPrefix = cPrefix;
}

void RemillStackRecovery::getAnalysisUsage(llvm::AnalysisUsage &usage) const {}

bool RemillStackRecovery::runOnModule(llvm::Module &module) {
  for (auto &func : module) {
    if (IsRecoverable(&func, cc)) {
      auto sp_arg = remill::NthArgument(&func, 0);
      if (!sp_arg->user_empty()) {
        std::cerr << "Analyzing: " << func.getName().str() << std::endl;
        func.dump();
        
        llvm::IRBuilder<> ir(&*func.getEntryBlock().begin());

        for (auto op : FindStackAddrOps(sp_arg, &func)) {
          llvm::Value *var = nullptr;
          for (auto cast : FindStackAddrCasts(op, pmem_addr_space)) {
            std::stringstream ss;
            ss << "loc_stackvar_" << GetStackIdx(sp_arg, op);
            auto ptr = llvm::dyn_cast<llvm::PointerType>(cast->getDestTy());
            CHECK(ptr != nullptr);
            if (!var) {
              var = ir.CreateAlloca(ptr->getElementType(), nullptr, ss.str());
            }
            auto ref = ir.CreatePtrToInt(var, sp_arg->getType());
            cast->replaceAllUsesWith(var);
            op->replaceAllUsesWith(ref);
          }
        }

        std::cerr << "Done analyzing: " << func.getName().str() << std::endl;
        func.dump();
      }
    }
  }
  return true;
}

llvm::ModulePass *createRemillStackRecoveryPass() {
  return new RemillStackRecovery;
}

static llvm::RegisterPass<RemillStackRecovery> remill_stackrec(
    "remill_stackrec", "Remill's Stack Frame Recovery", true, false);

}  // namespace fcd

// using namespace llvm;
// using RemillStackRecovery = fcd::RemillStackRecovery;
// INITIALIZE_PASS(RemillStackRecovery, "remill_stackrec",
//                 "Remill's Stack Frame Recovery", true, false)
