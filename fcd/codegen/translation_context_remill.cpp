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
#include <llvm/IR/LegacyPassManager.h>

#include <llvm/Transforms/Scalar.h>

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

llvm::Function *RemillTranslationContext::DeclareFunction(uint64_t addr) {
  DLOG(INFO) << "Declaring function for address " << std::hex << addr;
  auto &func = functions[addr];
  if (!func) {
    std::stringstream ss;
    ss << "sub_" << std::hex << addr;
    func = remill::DeclareLiftedFunction(module, ss.str());
  }
  return func;
}

llvm::BasicBlock *RemillTranslationContext::GetOrCreateBlock(
    uint64_t addr, llvm::Function *func) {
  auto &block = blocks[addr];
  if (!block) {
    std::stringstream ss;
    ss << "block_" << std::hex << addr;
    block = llvm::BasicBlock::Create(func->getContext(), ss.str(), func);
  }
  return block;
}

llvm::Function *RemillTranslationContext::DefineFunction(uint64_t addr) {
  DLOG(INFO) << "Defining function for address " << std::hex << addr;
  auto func = DeclareFunction(addr);
  if (!func->empty()) {
    LOG(WARNING) << "Asking to re-lift function: " << func->getName().str()
                 << "; returning current function instead";
    return func;
  }

  llvm::legacy::FunctionPassManager func_pass_manager(module);
  // func_pass_manager.add(llvm::createCFGSimplificationPass());
  func_pass_manager.add(llvm::createPromoteMemoryToRegisterPass());
  func_pass_manager.add(llvm::createReassociatePass());
  func_pass_manager.add(llvm::createDeadStoreEliminationPass());
  func_pass_manager.add(llvm::createDeadCodeEliminationPass());
  func_pass_manager.doInitialization();

  remill::CloneBlockFunctionInto(func);

  func->removeFnAttr(llvm::Attribute::AlwaysInline);
  func->removeFnAttr(llvm::Attribute::InlineHint);
  func->removeFnAttr(llvm::Attribute::NoReturn);
  func->addFnAttr(llvm::Attribute::NoInline);
  func->setVisibility(llvm::GlobalValue::DefaultVisibility);

  llvm::BranchInst::Create(GetOrCreateBlock(addr, func),
                           &func->getEntryBlock());

  blocks_to_lift.push(addr);
  while (!blocks_to_lift.empty()) {
    LiftBlock(func, blocks_to_lift.front());
    blocks_to_lift.pop();
  }

  func_pass_manager.run(*func);
  func_pass_manager.doFinalization();
  functions[addr] = func;
  return func;
}

bool RemillTranslationContext::LiftBlock(llvm::Function *func, uint64_t addr) {
  DLOG(INFO) << "Creating basic block for address " << std::hex << addr;

  auto block = GetOrCreateBlock(addr, func);

  for (remill::Instruction inst; LiftInst(inst, block, addr); inst.Reset()) {
    addr += inst.NumBytes();
    // TODO: If the jump target is a function, we shouldn't create a branch
    switch (inst.category) {
      case remill::Instruction::kCategoryConditionalBranch: {
        auto true_target = GetOrCreateBlock(inst.branch_taken_pc, func);
        if (true_target->empty()) blocks_to_lift.push(inst.branch_taken_pc);

        auto false_target = GetOrCreateBlock(inst.branch_not_taken_pc, func);
        if (false_target->empty())
          blocks_to_lift.push(inst.branch_not_taken_pc);

        auto cond = remill::LoadBranchTaken(block);
        llvm::BranchInst::Create(true_target, false_target, cond, block);

        return true;
      }
      case remill::Instruction::kCategoryDirectJump: {
        auto target = GetOrCreateBlock(inst.branch_taken_pc, func);
        if (target->empty()) blocks_to_lift.push(inst.branch_taken_pc);

        llvm::BranchInst::Create(target, block);

        return true;
      }
      case remill::Instruction::kCategoryIndirectJump:
      case remill::Instruction::kCategoryFunctionReturn:
        llvm::ReturnInst::Create(module->getContext(),
                                 remill::LoadMemoryPointer(block), block);
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

  if (inst.category == remill::Instruction::kCategoryDirectFunctionCall)
    DeclareFunction(inst.branch_taken_pc);

  return true;
}

}  // namespace fcd
