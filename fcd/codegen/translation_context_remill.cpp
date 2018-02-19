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
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/Scalar.h>

#include <sstream>
#include <string>

#include "remill/BC/Util.h"

#include "fcd/codegen/translation_context_remill.h"

namespace fcd {
namespace {
// Get a list of all ISELs.
// Stolen from mcsema
static std::vector<llvm::GlobalVariable *> FindISELs(llvm::Module *module) {
  std::vector<llvm::GlobalVariable *> isels;
  remill::ForEachISel(module, [&](llvm::GlobalVariable *isel,
                                  llvm::Function *) { isels.push_back(isel); });
  return isels;
}
// Remove the ISEL variables used for finding the instruction semantics.
// Stolen from mcsema
static void PrivatizeISELs(std::vector<llvm::GlobalVariable *> &isels) {
  for (auto isel : isels) {
    isel->setInitializer(nullptr);
    isel->setExternallyInitialized(false);
    isel->setLinkage(llvm::GlobalValue::PrivateLinkage);

    if (!isel->hasNUsesOrMore(2)) {
      isel->eraseFromParent();
    }
  }
}

static void RemoveFunction(llvm::Function *func) {
  if (!func->hasNUsesOrMore(1)) {
    func->eraseFromParent();
  }
}

// Remove some of the remill intrinsics.
// Stolen from mcsema
static void RemoveIntrinsics(llvm::Module *module) {
  if (auto llvm_used = module->getGlobalVariable("llvm.used")) {
    llvm_used->eraseFromParent();
  }

  // This function makes removing intrinsics tricky, so if it's there, then
  // we'll try to get the optimizer to inline it on our behalf, which should
  // drop some references :-D
  if (auto remill_used = module->getFunction("__remill_mark_as_used")) {
    std::vector<llvm::CallInst *> uses;
    for (auto use : remill_used->users()) {
      if (auto call = llvm::dyn_cast<llvm::CallInst>(use)) {
        uses.push_back(call);
      }
    }

    for (auto call : uses) {
      call->eraseFromParent();
    }

    if (remill_used->hasNUsesOrMore(1)) {
      if (remill_used->isDeclaration()) {
        remill_used->setLinkage(llvm::GlobalValue::InternalLinkage);
        remill_used->removeFnAttr(llvm::Attribute::NoInline);
        remill_used->addFnAttr(llvm::Attribute::InlineHint);
        remill_used->addFnAttr(llvm::Attribute::AlwaysInline);
        auto block =
            llvm::BasicBlock::Create(module->getContext(), "", remill_used);
        (void)llvm::ReturnInst::Create(module->getContext(), block);
      }
    }

    RemoveFunction(remill_used);
  }

  //  if (auto intrinsics = module->getFunction("__remill_intrinsics")) {
  //    intrinsics->eraseFromParent();
  //  }

  auto RemoveFunctionByName = [&module](const char *name) {
    if (auto func = module->getFunction(name)) {
      RemoveFunction(func);
    }
  };

  RemoveFunctionByName("__remill_basic_block");
  RemoveFunctionByName("__remill_defer_inlining");
  RemoveFunctionByName("__remill_intrinsics");
}

static bool IsTerminator(remill::Instruction &inst) {
  switch (inst.category) {
    case remill::Instruction::kCategoryDirectJump:
    case remill::Instruction::kCategoryIndirectJump:
    case remill::Instruction::kCategoryConditionalBranch:
    case remill::Instruction::kCategoryFunctionReturn:
    case remill::Instruction::kCategoryError:
      return true;
    default:
      return false;
  }
}
}  // namespace

RemillTranslationContext::RemillTranslationContext(llvm::LLVMContext &ctx,
                                                   Executable &exe)
    : executable(exe) {
  DLOG(INFO) << "Initializing TranslationContext";
  target_arch = remill::GetTargetArch();
  module = std::unique_ptr<llvm::Module>(remill::LoadTargetSemantics(&ctx));
  target_arch->PrepareModule(module);
  auto word_type = llvm::Type::getIntNTy(
      module->getContext(), static_cast<unsigned>(target_arch->address_size));
  intrinsics = std::make_unique<remill::IntrinsicTable>(module.get());
  lifter =
      std::make_unique<remill::InstructionLifter>(word_type, intrinsics.get());
}

uint64_t RemillTranslationContext::FindFunctionAddr(llvm::Function *func) {
  uint64_t addr = 0;
  for (auto item : functions) {
    if (item.second == func) addr = item.first;
  }
  return addr;
}

remill::Instruction &RemillTranslationContext::GetOrDecodeInst(uint64_t addr) {
  auto &inst = insts[addr];
  if (!inst.IsValid()) {
    DLOG(INFO) << "Decoding instruction at address " << std::hex << addr;

    auto begin = executable.map(addr);
    CHECK(begin != nullptr) << "Could not map address " << std::hex << addr;

    auto bytes = reinterpret_cast<const char *>(begin);
    std::string inst_bytes(bytes, target_arch->MaxInstructionSize());

    CHECK(target_arch->DecodeInstruction(addr, inst_bytes, inst))
        << "Can't decode instruction " << inst.Serialize() << " at " << std::hex
        << addr;
  }
  return inst;
}

llvm::BasicBlock *RemillTranslationContext::GetOrCreateBlock(
    uint64_t addr, llvm::Function *func) {
  auto &block = blocks[addr];
  if (!block) {
    DLOG(INFO) << "Creating basic block at address " << std::hex << addr
               << std::dec;
    std::stringstream ss;
    ss << "block_" << std::hex << addr;
    block = llvm::BasicBlock::Create(func->getContext(), ss.str(), func);
  }
  return block;
}

llvm::Function *RemillTranslationContext::DeclareFunction(uint64_t addr) {
  auto &func = functions[addr];
  if (!func) {
    DLOG(INFO) << "Declaring function at address " << std::hex << addr
               << std::dec;
    std::stringstream ss;
    ss << "sub_" << std::hex << addr;
    func = remill::DeclareLiftedFunction(module.get(), ss.str());
  }
  return func;
}

std::set<remill::Instruction *> RemillTranslationContext::DecodeFunction(
    uint64_t addr) {
  DLOG(INFO) << "Decoding function at address " << std::hex << addr << std::dec;

  std::set<remill::Instruction *> result;
  std::queue<uint64_t> insts_to_decode;
  insts_to_decode.push(addr);
  while (!insts_to_decode.empty()) {
    addr = insts_to_decode.front();
    insts_to_decode.pop();
    auto &inst = GetOrDecodeInst(addr);
    if (!result.count(&inst)) {
      result.insert(&inst);
      switch (inst.category) {
        case remill::Instruction::kCategoryConditionalBranch:
          insts_to_decode.push(inst.branch_not_taken_pc);
          // fallthrough
        case remill::Instruction::kCategoryDirectJump:
          insts_to_decode.push(inst.branch_taken_pc);
          break;
        default:
          if (!IsTerminator(inst)) {
            insts_to_decode.push(addr + inst.NumBytes());
          }
          break;
      }
    }
  }
  return result;
}

llvm::Function *RemillTranslationContext::DefineFunction(uint64_t addr) {
  DLOG(INFO) << "Defining function for address " << std::hex << addr
             << std::dec;
  auto func = DeclareFunction(addr);
  if (!func->empty()) {
    LOG(WARNING) << "Asking to re-lift function: " << func->getName().str()
                 << "; returning current function instead";
    return func;
  }

  llvm::legacy::FunctionPassManager func_pass_manager(module.get());
  func_pass_manager.add(llvm::createCFGSimplificationPass());
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
  return func;
}

llvm::BasicBlock *RemillTranslationContext::LiftBlock(llvm::Function *func,
                                                      uint64_t addr) {
  DLOG(INFO) << "Creating basic block for address " << std::hex << addr
             << std::dec;

  auto block = GetOrCreateBlock(addr, func);
  if (!block->empty()) {
    LOG(WARNING) << "Asking to re-lift block: " << block->getName().str()
                 << "; returning current block instead";
    return block;
  }

  remill::Instruction inst;
  while (!IsTerminator(inst)) {
    inst.Reset();
    inst = LiftInst(block, addr);
    addr += inst.NumBytes();
    switch (inst.category) {
      case remill::Instruction::kCategoryConditionalBranch:
        if (GetOrCreateBlock(inst.branch_not_taken_pc, func)->empty())
          blocks_to_lift.push(inst.branch_not_taken_pc);

        if (GetOrCreateBlock(inst.branch_taken_pc, func)->empty())
          blocks_to_lift.push(inst.branch_taken_pc);
        break;

      case remill::Instruction::kCategoryDirectJump:
        if (GetOrCreateBlock(inst.branch_taken_pc, func)->empty())
          blocks_to_lift.push(inst.branch_taken_pc);
        break;

      default:
        break;
    }
  }
  return block;
}

remill::Instruction &RemillTranslationContext::LiftInst(llvm::BasicBlock *block,
                                                        uint64_t addr) {
  auto &inst = GetOrDecodeInst(addr);

  CHECK(remill::kLiftedInstruction == lifter->LiftIntoBlock(inst, block))
      << "Can't lift instruction " << inst.Serialize() << " at " << std::hex
      << addr << std::dec;

  auto func = block->getParent();
  llvm::IRBuilder<> builder(block);
  switch (inst.category) {
    case remill::Instruction::kCategoryConditionalBranch: {
      auto true_block = GetOrCreateBlock(inst.branch_taken_pc, func);
      auto false_block = GetOrCreateBlock(inst.branch_not_taken_pc, func);
      auto cond = remill::LoadBranchTaken(block);
      builder.CreateCondBr(cond, true_block, false_block);
    } break;

    case remill::Instruction::kCategoryDirectJump: {
      auto iter = functions.find(inst.branch_taken_pc);
      if (iter != functions.end()) {
        remill::AddTerminatingTailCall(block, iter->second);
      } else {
        auto target = GetOrCreateBlock(inst.branch_taken_pc, func);
        builder.CreateBr(target);
      }
    } break;

    case remill::Instruction::kCategoryIndirectJump:
      remill::AddTerminatingTailCall(block, intrinsics->jump);
      break;

    case remill::Instruction::kCategoryDirectFunctionCall: {
      if (inst.branch_taken_pc != inst.next_pc) {
        auto target = DeclareFunction(inst.branch_taken_pc);
        auto args = remill::LiftedFunctionArgs(block);
        auto call = builder.CreateCall(target, args);
        call->setCallingConv(target->getCallingConv());
        auto mem_ptr = remill::LoadMemoryPointerRef(block);
        builder.CreateStore(call, mem_ptr);
      } else {
        LOG(WARNING) << "Not adding a subroutine self-call at " << std::hex
                     << inst.pc << std::dec;
      }
    } break;

    case remill::Instruction::kCategoryIndirectFunctionCall: {
      auto target = intrinsics->function_call;
      auto args = remill::LiftedFunctionArgs(block);
      auto call = builder.CreateCall(target, args);
      call->setCallingConv(target->getCallingConv());
      auto mem_ptr = remill::LoadMemoryPointerRef(block);
      builder.CreateStore(call, mem_ptr);
    } break;

    case remill::Instruction::kCategoryFunctionReturn:
      builder.CreateRet(remill::LoadMemoryPointer(block));
      break;

    case remill::Instruction::kCategoryError:
      remill::AddTerminatingTailCall(block, intrinsics->error);
      break;

    default:
      break;
  }
  return inst;
}

void RemillTranslationContext::FinalizeModule() {
  llvm::legacy::PassManager module_pass_manager;
  module_pass_manager.add(llvm::createVerifierPass());
  module_pass_manager.add(llvm::createAlwaysInlinerLegacyPass());
  auto isels = FindISELs(module.get());
  RemoveIntrinsics(module.get());
  PrivatizeISELs(isels);
  module_pass_manager.run(*module);
  RemoveIntrinsics(module.get());
}

}  // namespace fcd
