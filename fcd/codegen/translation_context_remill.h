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

#ifndef FCD_CODEGEN_TRANSLATION_CONTEXT_REMILL_H_
#define FCD_CODEGEN_TRANSLATION_CONTEXT_REMILL_H_

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

#include <memory>
#include <queue>
#include <unordered_map>
#include <unordered_set>

#include "remill/Arch/Arch.h"
#include "remill/Arch/Instruction.h"
#include "remill/BC/IntrinsicTable.h"
#include "remill/BC/Lifter.h"

#include "fcd/executables/executable.h"

namespace fcd {

class RemillTranslationContext {
 private:
  const unsigned pmem_addr_space = 1;
  const remill::Arch *target_arch;
  std::unique_ptr<llvm::Module> module;
  Executable &executable;
  std::queue<uint64_t> blocks_to_lift;
  std::unordered_map<uint64_t, remill::Instruction> insts;
  std::unordered_map<uint64_t, llvm::BasicBlock *> blocks;
  std::unordered_map<uint64_t, llvm::Function *> functions;
  std::unique_ptr<remill::IntrinsicTable> intrinsics;
  std::unique_ptr<remill::InstructionLifter> lifter;

  llvm::BasicBlock *GetOrCreateBlock(uint64_t addr, llvm::Function *func);
  remill::Instruction &GetOrDecodeInst(uint64_t addr);

  llvm::BasicBlock *LiftBlock(llvm::Function *func, uint64_t addr);
  remill::Instruction &LiftInst(llvm::BasicBlock *block, uint64_t addr);

  uint64_t FindFunctionAddr(llvm::Function *func);

 public:
  RemillTranslationContext(llvm::LLVMContext &ctx, Executable &exe);
  ~RemillTranslationContext(void){};

  std::unordered_set<uint64_t> DecodeFunction(uint64_t addr);
  std::unordered_set<uint64_t> DecodeFunction(llvm::Function *func) {
    return DecodeFunction(FindFunctionAddr(func));
  }

  llvm::Function *DeclareFunction(uint64_t addr);
  llvm::Function *DefineFunction(uint64_t addr);
  llvm::Function *DefineFunction(llvm::Function *func) {
    return DefineFunction(FindFunctionAddr(func));
  }

  llvm::Module &GetModule(void) const { return *module; }
  std::unique_ptr<llvm::Module> TakeModule(void) { return std::move(module); }
  const std::unordered_map<uint64_t, llvm::Function *> &GetFunctionMap(
      void) const {
    return functions;
  }

  const std::unordered_map<uint64_t, remill::Instruction> &GetInstMap(
      void) const {
    return insts;
  }

  const StubInfo *GetStubInfo(llvm::Function *func) const;

  void FinalizeModule(void);
};

}  // namespace fcd
#endif  // FCD_CODEGEN_TRANSLATION_CONTEXT_REMILL_H_