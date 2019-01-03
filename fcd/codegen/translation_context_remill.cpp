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

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>

#include <sstream>
#include <string>

#include "remill/BC/Util.h"

#include "fcd/compat/IPO.h"
#include "fcd/compat/Scalar.h"
#include "fcd/compat/AnalysisPasses.h"

#include "fcd/codegen/translation_context_remill.h"
#include "fcd/pass_argrec_remill.h"
#include "fcd/pass_asaa.h"
#include "fcd/pass_stackrec_remill.h"
#include "fcd/pass_intrinsics_remill.h"

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

// Stolen from mcsema
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

// Lower a memory read intrinsic into a `load` instruction.
// Stolen from mcsema
static void ReplaceMemReadOp(llvm::Module *module, const char *name,
                             llvm::Type *val_type, unsigned addr_space) {
  auto func = module->getFunction(name);
  if (!func) {
    return;
  }

  CHECK(func->isDeclaration())
      << "Cannot lower already implemented memory intrinsic " << name;

  auto callers = remill::CallersOf(func);
  for (auto call_inst : callers) {
    auto addr = call_inst->getArgOperand(1);

    llvm::IRBuilder<> ir(call_inst);
    llvm::Value *ptr = nullptr;
    if (auto as_int = llvm::dyn_cast<llvm::PtrToIntInst>(addr)) {
      ptr = ir.CreateBitCast(
          as_int->getPointerOperand(),
          llvm::PointerType::get(val_type, as_int->getPointerAddressSpace()));

    } else {
      ptr =
          ir.CreateIntToPtr(addr, llvm::PointerType::get(val_type, addr_space));
    }

    llvm::Value *val = ir.CreateLoad(ptr);
    if (val_type->isX86_FP80Ty()) {
      val = ir.CreateFPTrunc(val, func->getReturnType());
    }
    call_inst->replaceAllUsesWith(val);
  }
  for (auto call_inst : callers) {
    call_inst->eraseFromParent();
  }
  RemoveFunction(func);
}

// Lower a memory write intrinsic into a `store` instruction.
// Stolen from mcsema
static void ReplaceMemWriteOp(llvm::Module *module, const char *name,
                              llvm::Type *val_type, unsigned addr_space) {
  auto func = module->getFunction(name);
  if (!func) {
    return;
  }

  CHECK(func->isDeclaration())
      << "Cannot lower already implemented memory intrinsic " << name;

  auto callers = remill::CallersOf(func);

  for (auto call_inst : callers) {
    auto mem_ptr = call_inst->getArgOperand(0);
    auto addr = call_inst->getArgOperand(1);
    auto val = call_inst->getArgOperand(2);

    llvm::IRBuilder<> ir(call_inst);

    llvm::Value *ptr = nullptr;
    if (auto as_int = llvm::dyn_cast<llvm::PtrToIntInst>(addr)) {
      ptr = ir.CreateBitCast(
          as_int->getPointerOperand(),
          llvm::PointerType::get(val_type, as_int->getPointerAddressSpace()));
    } else {
      ptr =
          ir.CreateIntToPtr(addr, llvm::PointerType::get(val_type, addr_space));
    }

    if (val_type->isX86_FP80Ty()) {
      val = ir.CreateFPExt(val, val_type);
    }

    ir.CreateStore(val, ptr);
    call_inst->replaceAllUsesWith(mem_ptr);
  }
  for (auto call_inst : callers) {
    call_inst->eraseFromParent();
  }
  RemoveFunction(func);
}

// Stolen from mcsema
static void ReplaceBarrier(llvm::Module *module, const char *name) {
  auto func = module->getFunction(name);
  if (!func) {
    return;
  }

  CHECK(func->isDeclaration())
      << "Cannot lower already implemented memory intrinsic " << name;

  auto callers = remill::CallersOf(func);
  for (auto call_inst : callers) {
    auto mem_ptr = call_inst->getArgOperand(0);
    call_inst->replaceAllUsesWith(mem_ptr);
    call_inst->eraseFromParent();
  }
}

// Stolen from mcsema
static void LowerMemOps(llvm::Module *module, unsigned addr_space) {
  auto &ctx = module->getContext();

  auto ReadOpWrapper = [&](const char *name, llvm::Type *type) {
    ReplaceMemReadOp(module, name, type, addr_space);
  };

  auto WriteOpWrapper = [&](const char *name, llvm::Type *type) {
    ReplaceMemWriteOp(module, name, type, addr_space);
  };

  ReadOpWrapper("__remill_read_memory_8", llvm::Type::getInt8Ty(ctx));
  ReadOpWrapper("__remill_read_memory_16", llvm::Type::getInt16Ty(ctx));
  ReadOpWrapper("__remill_read_memory_32", llvm::Type::getInt32Ty(ctx));
  ReadOpWrapper("__remill_read_memory_64", llvm::Type::getInt64Ty(ctx));
  ReadOpWrapper("__remill_read_memory_f32", llvm::Type::getFloatTy(ctx));
  ReadOpWrapper("__remill_read_memory_f64", llvm::Type::getDoubleTy(ctx));

  WriteOpWrapper("__remill_write_memory_8", llvm::Type::getInt8Ty(ctx));
  WriteOpWrapper("__remill_write_memory_16", llvm::Type::getInt16Ty(ctx));
  WriteOpWrapper("__remill_write_memory_32", llvm::Type::getInt32Ty(ctx));
  WriteOpWrapper("__remill_write_memory_64", llvm::Type::getInt64Ty(ctx));
  WriteOpWrapper("__remill_write_memory_f32", llvm::Type::getFloatTy(ctx));
  WriteOpWrapper("__remill_write_memory_f64", llvm::Type::getDoubleTy(ctx));

  ReadOpWrapper("__remill_read_memory_f80", llvm::Type::getX86_FP80Ty(ctx));
  WriteOpWrapper("__remill_write_memory_f80", llvm::Type::getX86_FP80Ty(ctx));
}

static bool IsTerminator(remill::Instruction &inst) {
  switch (inst.category) {
    case remill::Instruction::kCategoryInvalid:
    case remill::Instruction::kCategoryError:
    case remill::Instruction::kCategoryDirectJump:
    case remill::Instruction::kCategoryIndirectJump:
    case remill::Instruction::kCategoryConditionalBranch:
    case remill::Instruction::kCategoryConditionalAsyncHyperCall:
    case remill::Instruction::kCategoryFunctionReturn:
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
  
  intrinsics = std::make_unique<remill::IntrinsicTable>(module.get());
  lifter = std::make_unique<remill::InstructionLifter>(target_arch, intrinsics.get());
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
    DLOG(INFO) << "Decoding instruction at address " << std::hex << addr
               << std::dec;

    auto begin = executable.map(addr);
    CHECK(begin != nullptr)
        << "Could not map address " << std::hex << addr << std::dec;

    auto bytes = reinterpret_cast<const char *>(begin);
    std::string inst_bytes(bytes, target_arch->MaxInstructionSize());

    if (!target_arch->DecodeInstruction(addr, inst_bytes, inst))
      LOG(WARNING) << "Can't decode instruction " << inst.Serialize() << " at "
                   << std::hex << addr << std::dec;
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
    ss << "block_" << std::hex << addr << std::dec;
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
    ss << "sub_" << std::hex << addr << std::dec;
    func = remill::DeclareLiftedFunction(module.get(), ss.str());
  }
  return func;
}

std::unordered_set<uint64_t> RemillTranslationContext::DecodeFunction(
    uint64_t addr) {
  DLOG(INFO) << "Decoding function at address " << std::hex << addr << std::dec;

  std::unordered_set<uint64_t> result;
  std::queue<uint64_t> insts_to_decode;
  insts_to_decode.push(addr);
  while (!insts_to_decode.empty()) {
    addr = insts_to_decode.front();
    insts_to_decode.pop();
    if (result.count(addr)) {
      continue;
    }
    auto &inst = GetOrDecodeInst(addr);
    result.insert(inst.pc);
    if (inst.IsValid()) {
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

  auto pc_arg = remill::NthArgument(func, 1);
  auto pc_val = llvm::ConstantInt::get(pc_arg->getType(), addr);
  pc_arg->replaceAllUsesWith(pc_val);

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

  do {
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
  } while (!IsTerminator(inst));

  return block;
}

remill::Instruction &RemillTranslationContext::LiftInst(llvm::BasicBlock *block,
                                                        uint64_t addr) {
  auto &inst = GetOrDecodeInst(addr);

  if (inst.IsValid()) {
    CHECK(remill::kLiftedInstruction == lifter->LiftIntoBlock(inst, block))
        << "Can't lift instruction " << inst.Serialize() << " at " << std::hex
        << addr << std::dec;
  }

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

    case remill::Instruction::kCategoryInvalid:
    case remill::Instruction::kCategoryError:
      remill::AddTerminatingTailCall(block, intrinsics->error);
      break;

    default:
      break;
  }
  return inst;
}

const StubInfo *RemillTranslationContext::GetStubInfo(
    llvm::Function *func) const {
  if (func->isDeclaration()) {
    return nullptr;
  }

  auto jump_intrinsic = remill::FindFunction(func->getParent(), "__fcd_jump");
  CHECK(jump_intrinsic != nullptr);

  llvm::Value *jump_target = nullptr;
  auto &entry = func->getEntryBlock();
  if (entry.size() > 1) {
    if (auto term = llvm::dyn_cast<llvm::ReturnInst>(entry.getTerminator())) {
      if (auto jump = llvm::dyn_cast<llvm::CallInst>(term->getPrevNode())) {
        if (jump->getCalledFunction() == jump_intrinsic) {
          jump_target = jump->getArgOperand(0);
        }
      }
    }
  }

  if (!jump_target) {
    return nullptr;
  }

  llvm::ConstantInt *addr = nullptr;
  if (auto call = llvm::dyn_cast<llvm::CallInst>(jump_target)) {
    if (call->getCalledFunction() == intrinsics->read_memory_64) {
      addr = llvm::dyn_cast<llvm::ConstantInt>(call->getArgOperand(1));
    }
  }

  if (auto load = llvm::dyn_cast<llvm::LoadInst>(jump_target)) {
    if (load->getPointerAddressSpace() == pmem_addr_space) {
      auto read_op = load->getPointerOperand();
      if (auto const_expr = llvm::dyn_cast<llvm::ConstantExpr>(read_op)) {
        auto inst = const_expr->getAsInstruction();
        if (auto int2ptr = llvm::dyn_cast<llvm::IntToPtrInst>(inst)) {
          addr = llvm::dyn_cast<llvm::ConstantInt>(int2ptr->getOperand(0));
        }
        inst->deleteValue();
      } else {
        addr = llvm::dyn_cast<llvm::ConstantInt>(read_op);
      }
    }
  }

  return addr ? executable.getStubTarget(addr->getLimitedValue()) : nullptr;
}

void RemillTranslationContext::FinalizeModule() {
  auto AACallBack = [](llvm::Pass &p, llvm::Function &f, llvm::AAResults &r) {
    if (auto asaa = p.getAnalysisIfAvailable<AddressSpaceAAWrapperPass>())
      r.addAAResult(asaa->getResult());
  };
  
  auto isels = FindISELs(module.get());
  RemoveIntrinsics(module.get());
  PrivatizeISELs(isels);
  
  llvm::legacy::PassManager phase_one;
  phase_one.add(llvm::createAlwaysInlinerLegacyPass());
  phase_one.add(createRemillArgumentRecoveryPass());
  phase_one.add(llvm::createPromoteMemoryToRegisterPass());
  phase_one.add(llvm::createReassociatePass());
  phase_one.add(llvm::createDeadCodeEliminationPass());
  phase_one.add(llvm::createCFGSimplificationPass());
  // phase_one.add(llvm::createVerifierPass());
  phase_one.run(*module);
  
  // Lower memory intrinsics into loads and stores
  // Runtime memory address space is 0
  // Program memory address space is given by pmem_addr_space
  LowerMemOps(module.get(), pmem_addr_space);
  ReplaceBarrier(module.get(), "__remill_barrier_load_load");
  ReplaceBarrier(module.get(), "__remill_barrier_load_store");
  ReplaceBarrier(module.get(), "__remill_barrier_store_load");
  ReplaceBarrier(module.get(), "__remill_barrier_store_store");
  ReplaceBarrier(module.get(), "__remill_barrier_atomic_begin");
  ReplaceBarrier(module.get(), "__remill_barrier_atomic_end");
  
  llvm::legacy::PassManager phase_two;
  phase_two.add(llvm::createTypeBasedAAWrapperPass());
  phase_two.add(llvm::createScopedNoAliasAAWrapperPass());
  phase_two.add(llvm::createBasicAAWrapperPass());
  phase_two.add(createAddressSpaceAliasAnalysis());
  phase_two.add(llvm::createExternalAAWrapperPass(AACallBack));
  phase_two.add(createRemillFixIntrinsicsPass());
  phase_two.add(llvm::createSROAPass());
  phase_two.add(llvm::createGVNPass());
  phase_two.add(llvm::createGlobalDCEPass());
  // phase_two.add(llvm::createVerifierPass());
  phase_two.run(*module);

  RemoveIntrinsics(module.get());

  llvm::legacy::PassManager phase_three;
  phase_three.add(llvm::createTypeBasedAAWrapperPass());
  phase_three.add(llvm::createScopedNoAliasAAWrapperPass());
  phase_three.add(llvm::createBasicAAWrapperPass());
  phase_three.add(createAddressSpaceAliasAnalysis());
  phase_three.add(llvm::createExternalAAWrapperPass(AACallBack));
  phase_three.add(llvm::createInstructionCombiningPass());
  phase_three.add(createRemillStackRecoveryPass());
  // phase_three.add(llvm::createPromoteMemoryToRegisterPass());
  // phase_three.add(llvm::createReassociatePass());
  phase_three.add(llvm::createDeadStoreEliminationPass());
  phase_three.add(llvm::createDeadCodeEliminationPass());
  phase_three.add(llvm::createCFGSimplificationPass());
  // phase_three.add(llvm::createInstructionCombiningPass());
  // phase_three.add(llvm::createVerifierPass());
  phase_three.run(*module);

  // Attempt to annotate remaining functions as stubs
  for (auto &func : *module) {
    if (auto stub_info = GetStubInfo(&func)) {
      func.setName(stub_info->name);
    }
  }
}

}  // namespace fcd
