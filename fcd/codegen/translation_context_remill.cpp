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

#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>

#include <sstream>
#include <string>

#include "remill/Arch/Instruction.h"
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
  auto word_type = llvm::Type::getIntNTy(
      module->getContext(), static_cast<unsigned>(target_arch->address_size));
  intrinsics = std::make_unique<remill::IntrinsicTable>(module);
  lifter =
      std::make_unique<remill::InstructionLifter>(word_type, intrinsics.get());
}

llvm::Function *RemillTranslationContext::CreateFunction(uint64_t addr) {
  DLOG(INFO) << "Creating function for address " << std::hex << addr;
  auto func = functions[addr];
  if (func != nullptr && !func->empty()) {
    LOG(WARNING) << "Asking to re-lift function: " << func->getName().str()
                 << "; returning current function instead";
    return func;
  }

  std::stringstream ss;
  ss << "sub_" << std::hex << addr;

  func = remill::DeclareLiftedFunction(module, ss.str());
  remill::CloneBlockFunctionInto(func);

  blocks_to_lift.push(addr);
  while (!blocks_to_lift.empty()) {
    LiftBlock(func, blocks_to_lift.front());
    blocks_to_lift.pop();
  }

  func->dump();
  return func;
}

bool RemillTranslationContext::LiftBlock(llvm::Function *func, uint64_t addr) {
  DLOG(INFO) << "Creating basic block for address " << std::hex << addr;

  // Function that will create basic blocks as needed.
  auto GetOrCreateBlock = [func, this](uint64_t block_addr) {
    auto &block = this->blocks[block_addr];
    if (!block) {
      std::stringstream ss;
      ss << "block_" << std::hex << block_addr;
      block = llvm::BasicBlock::Create(func->getContext(), ss.str(), func);
    }
    return block;
  };

  auto block = GetOrCreateBlock(addr);

  for (remill::Instruction inst; LiftInst(inst, block, addr); inst.Reset()) {
    addr += inst.NumBytes();
    switch (inst.category) {
      case remill::Instruction::kCategoryConditionalBranch: {
        auto true_target = GetOrCreateBlock(inst.branch_taken_pc);
        if (true_target->empty())
          blocks_to_lift.push(inst.branch_taken_pc);

        auto false_target = GetOrCreateBlock(inst.branch_not_taken_pc);
        if (false_target->empty())
          blocks_to_lift.push(inst.branch_not_taken_pc);

        auto cond = remill::LoadBranchTaken(block);
        llvm::BranchInst::Create(true_target, false_target, cond, block);

        return true;
      }
      case remill::Instruction::kCategoryDirectJump: {
        auto target = GetOrCreateBlock(inst.branch_taken_pc);
        if (target->empty()) blocks_to_lift.push(inst.branch_taken_pc);

        llvm::BranchInst::Create(target, block);

        return true;
      }
      case remill::Instruction::kCategoryIndirectJump:
      case remill::Instruction::kCategoryFunctionReturn:
        return true;
      default:
        break;
    }
  }
  return false;
}

bool RemillTranslationContext::LiftInst(remill::Instruction &inst,
                                        llvm::BasicBlock *block,
                                        uint64_t addr) {
  auto begin = executable->map(addr);
  CHECK(begin != nullptr) << "Could not map address " << std::hex << addr;

  auto bytes = reinterpret_cast<const char *>(begin);
  std::string inst_bytes(bytes, target_arch->MaxInstructionSize());

  CHECK(target_arch->DecodeInstruction(addr, inst_bytes, inst))
      << "Can't decode instruction " << inst.Serialize() << " at " << std::hex
      << addr;

  CHECK(remill::kLiftedInstruction == lifter->LiftIntoBlock(inst, block))
      << "Can't lift instruction " << inst.Serialize() << " at " << std::hex
      << addr;

  return true;
}

}  // namespace fcd
