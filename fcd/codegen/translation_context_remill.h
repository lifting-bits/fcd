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

#include "remill/Arch/Arch.h"
#include "remill/BC/IntrinsicTable.h"
#include "remill/BC/Lifter.h"

#include "fcd/executables/executable.h"

namespace fcd {

class RemillTranslationContext {
 private:
  const remill::Arch *target_arch;
  llvm::Module *module;
  Executable *executable;
  std::queue<uint64_t> blocks_to_lift;
  std::unordered_map<uint64_t, llvm::BasicBlock *> blocks;
  std::unordered_map<uint64_t, llvm::Function *> functions;
  std::unique_ptr<remill::IntrinsicTable> intrinsics;
  std::unique_ptr<remill::InstructionLifter> lifter;

  llvm::BasicBlock *GetOrCreateBlock(uint64_t addr, llvm::Function *func);

  bool LiftBlock(llvm::Function *func, uint64_t addr);
  bool LiftInst(remill::Instruction &inst, llvm::BasicBlock *block,
                uint64_t addr);

 public:
  RemillTranslationContext(llvm::LLVMContext *ctx, Executable *exe);
  ~RemillTranslationContext(){};

  llvm::Function *DeclareFunction(uint64_t addr);
  llvm::Function *DefineFunction(uint64_t addr);

  llvm::Module *GetModule() const { return module; }
  const std::unordered_map<uint64_t, llvm::Function *> &GetFunctionMap() const {
    return functions;
  }
};

}  // namespace fcd
#endif  // FCD_CODEGEN_TRANSLATION_CONTEXT_REMILL_H_