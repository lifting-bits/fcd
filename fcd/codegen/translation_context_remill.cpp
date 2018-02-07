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

#include <sstream>
#include <string>

#include <llvm/IR/Function.h>

#include "remill/Arch/Instruction.h"
#include "remill/BC/IntrinsicTable.h"
#include "remill/BC/Lifter.h"
#include "remill/BC/Util.h"

#include "fcd/codegen/translation_context_remill.h"

namespace fcd {

RemillTranslationContext::RemillTranslationContext(llvm::LLVMContext *ctx,
                                                   Executable *exe) {
  DLOG(INFO) << "Initializing TranslationContext";
  target_arch = remill::GetTargetArch();
  module = remill::LoadTargetSemantics(ctx);
  target_arch->PrepareModule(module);
  executable = exe;
}

llvm::Function *RemillTranslationContext::CreateFunction(uint64_t addr) {
  DLOG(INFO) << "Creating function for code at address " << addr;

  std::stringstream ss;
  ss << "sub_" << addr;

  auto func = remill::DeclareLiftedFunction(module, ss.str());
  remill::CloneBlockFunctionInto(func);
  auto word_type = llvm::Type::getIntNTy(
      module->getContext(), static_cast<unsigned>(target_arch->address_size));

  auto begin = executable->map(addr);
  auto bytes = reinterpret_cast<const char *>(begin);
  std::string inst_bytes(bytes, target_arch->MaxInstructionSize());

  remill::Instruction inst;
  CHECK(target_arch->DecodeInstruction(addr, inst_bytes, inst))
      << "Can't decode instruction " << inst.Serialize() << " at " << addr;

  addr += inst.NumBytes();

  remill::IntrinsicTable intrinsics(module);
  remill::InstructionLifter lifter(word_type, &intrinsics);

  CHECK(remill::kLiftedInstruction == lifter.LiftIntoBlock(inst, &func->back()))
      << "Can't decode instruction " << inst.Serialize() << " at " << addr;

  func->dump();
  return func;
}

}  // namespace fcd
