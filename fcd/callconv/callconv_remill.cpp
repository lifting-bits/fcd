/*
 * Copyright (c) 2017 Trail of Bits, Inc.
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

#include <glog/logging.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "remill/Arch/Arch.h"
#include "remill/Arch/Name.h"
#include "remill/BC/Util.h"
#include "remill/BC/Version.h"
#include "remill/OS/OS.h"

#include "fcd/callconv/callconv_remill.h"

namespace fcd {

enum ValueKind {
  kInvalidKind = 0,
  kI8 = (1 << 0),
  kI16 = (1 << 1),
  kI32 = (1 << 2),
  kI64 = (1 << 3),
  kF32 = (1 << 4),
  kF64 = (1 << 5),
  kF80 = (1 << 6),

  kIntegralLeast32 = kI8 | kI16 | kI32,
  kIntegralLeast64 = kI8 | kI16 | kI32 | kI64,
};

struct ArgConstraint {
  const char *var_name;
  const int accepted_val_kinds;
};

namespace {

static const char *StackPointerName(const remill::Arch *arch) {
  static const char *sp_name = nullptr;
  if (sp_name) {
    return sp_name;
  }

  switch (arch->arch_name) {
    case remill::kArchAArch64LittleEndian:
      sp_name = "SP";
      break;

    case remill::kArchX86:
    case remill::kArchX86_AVX:
    case remill::kArchX86_AVX512:
      sp_name = "ESP";
      break;

    case remill::kArchAMD64:
    case remill::kArchAMD64_AVX:
    case remill::kArchAMD64_AVX512:
      sp_name = "RSP";
      break;
    default:
      LOG(FATAL) << "Can't get stack pointer name for architecture: "
                 << remill::GetArchName(arch->arch_name);
      return nullptr;
  }

  return sp_name;
}

static const ArgConstraint *ParamTable(const remill::Arch *arch,
                                       llvm::CallingConv::ID cc) {
  static const ArgConstraint kNoArgs[] = {
      {nullptr, kInvalidKind},
  };

  if (llvm::CallingConv::X86_64_SysV == cc) {
    static const ArgConstraint kAmd64SysVArgs[] = {
        {"RDI", kIntegralLeast64}, {"RSI", kIntegralLeast64},
        {"RDX", kIntegralLeast64}, {"RCX", kIntegralLeast64},
        {"R8", kIntegralLeast64},  {"R9", kIntegralLeast64},
        {"XMM0", kF32 | kF64},     {"XMM1", kF32 | kF64},
        {"XMM2", kF32 | kF64},     {"XMM3", kF32 | kF64},
        {"XMM4", kF32 | kF64},     {"XMM5", kF32 | kF64},
        {"XMM6", kF32 | kF64},     {"XMM7", kF32 | kF64},
        {"XMM8", kF32 | kF64},     {"XMM9", kF32 | kF64},
        {"XMM10", kF32 | kF64},    {"XMM11", kF32 | kF64},
        {"XMM12", kF32 | kF64},    {"XMM13", kF32 | kF64},
        {"XMM14", kF32 | kF64},    {"XMM15", kF32 | kF64},
        {nullptr, kInvalidKind},
    };
    return &(kAmd64SysVArgs[0]);

  } else if (llvm::CallingConv::Win64 == cc) {
    static const ArgConstraint kAmd64Win64Args[] = {
        {"RCX", kIntegralLeast64}, {"RDX", kIntegralLeast64},
        {"R8", kIntegralLeast64},  {"R9", kIntegralLeast64},
        {"XMM0", kF32 | kF64},     {"XMM1", kF32 | kF64},
        {"XMM2", kF32 | kF64},     {"XMM3", kF32 | kF64},
        {nullptr, kInvalidKind},
    };
    return &(kAmd64Win64Args[0]);

  } else if (llvm::CallingConv::X86_FastCall == cc) {
    static const ArgConstraint kX86FastCallArgs[] = {
        {"ECX", kIntegralLeast32},
        {"EDX", kIntegralLeast32},
        {nullptr, kInvalidKind},
    };
    return &(kX86FastCallArgs[0]);

  } else if (llvm::CallingConv::X86_ThisCall == cc) {
    static const ArgConstraint kX86ThisCallArgs[] = {
        {"ECX", kIntegralLeast32},
        {nullptr, kInvalidKind},
    };
    return &(kX86ThisCallArgs[0]);

  } else if (llvm::CallingConv::X86_StdCall == cc) {
    return &(kNoArgs[0]);  // stdcall takes all args on the stack.

  } else if (llvm::CallingConv::C == cc) {
    if (arch->IsX86()) {
      return &(kNoArgs[0]);  // cdecl takes all args on the stack.

    } else if (arch->IsAArch64()) {
      static const ArgConstraint kAArch64Args[] = {
          {"X0", kIntegralLeast64}, {"X1", kIntegralLeast64},
          {"X2", kIntegralLeast64}, {"X3", kIntegralLeast64},
          {"X4", kIntegralLeast64}, {"X5", kIntegralLeast64},
          {"X6", kIntegralLeast64}, {"X7", kIntegralLeast64},

          {"D0", kF32 | kF64},      {"D1", kF32 | kF64},
          {"D2", kF32 | kF64},      {"D3", kF32 | kF64},
          {"D4", kF32 | kF64},      {"D5", kF32 | kF64},
          {"D6", kF32 | kF64},      {"D7", kF32 | kF64},
          {"D8", kF32 | kF64},      {"D9", kF32 | kF64},
          {"D10", kF32 | kF64},     {"D11", kF32 | kF64},
          {"D12", kF32 | kF64},     {"D13", kF32 | kF64},
          {"D14", kF32 | kF64},     {"D15", kF32 | kF64},
          {"D16", kF32 | kF64},     {"D17", kF32 | kF64},
          {"D18", kF32 | kF64},     {"D19", kF32 | kF64},
          {"D20", kF32 | kF64},     {"D21", kF32 | kF64},
          {"D22", kF32 | kF64},     {"D23", kF32 | kF64},
          {"D24", kF32 | kF64},     {"D25", kF32 | kF64},
          {"D26", kF32 | kF64},     {"D27", kF32 | kF64},
          {"D28", kF32 | kF64},     {"D29", kF32 | kF64},
          {"D30", kF32 | kF64},     {"D31", kF32 | kF64},

          {nullptr, kInvalidKind},
      };
      return &(kAArch64Args[0]);
    }
  }

  LOG(FATAL) << "Unknown ABI/calling convention: " << cc;
  return &(kNoArgs[0]);
}

static const ArgConstraint *ReturnTable(const remill::Arch *arch,
                                        llvm::CallingConv::ID cc) {
  static const ArgConstraint kNoArgs[] = {
      {nullptr, kInvalidKind},
  };

  if (llvm::CallingConv::X86_64_SysV == cc || llvm::CallingConv::Win64 == cc) {
    static const ArgConstraint kAmd64SysVArgs[] = {
        {"RAX", kIntegralLeast64},
        {"XMM0", kF32 | kF64},
        {nullptr, kInvalidKind},
    };
    return &(kAmd64SysVArgs[0]);

  } else if (llvm::CallingConv::X86_StdCall == cc ||
             llvm::CallingConv::X86_FastCall == cc ||
             llvm::CallingConv::X86_ThisCall == cc) {
    static const ArgConstraint kX86FastCallArgs[] = {
        {"EAX", kIntegralLeast32},
        {"ST0", kF32 | kF64 | kF80},
        {nullptr, kInvalidKind},
    };
    return &(kX86FastCallArgs[0]);

  } else if (llvm::CallingConv::C == cc) {
    if (arch->IsX86()) {
      static const ArgConstraint kX86CDeclCallArgs[] = {
          {"EAX", kIntegralLeast32 | kF32},
          {nullptr, kInvalidKind},
      };
      return &(kX86CDeclCallArgs[0]);  // cdecl takes all args on the stack.

    } else if (arch->IsAArch64()) {
      static const ArgConstraint kAArch64Args[] = {
          {"X0", kIntegralLeast64},
          {"S0", kF32},
          {"D0", kF32 | kF64},
          {nullptr, kInvalidKind},
      };
      return &(kAArch64Args[0]);
    }
  }

  LOG(FATAL) << "Unknown ABI/calling convention: " << cc;
  return &(kNoArgs[0]);
}

static uint64_t DefaultUsedStackBytes(llvm::CallingConv::ID cc) {
  switch (cc) {
    case llvm::CallingConv::X86_64_SysV:
      return 8;  // Size of return address on the stack.

    case llvm::CallingConv::Win64:
      return 8 + 32;  // Return address + shadow space.

    case llvm::CallingConv::X86_FastCall:
    case llvm::CallingConv::X86_StdCall:
    case llvm::CallingConv::X86_ThisCall:
      return 4;  // Size of return address on the stack.

    default:
      return 0;
  }
}

static const char *IntReturnValVar(const remill::Arch *arch,
                                   llvm::CallingConv::ID cc) {
  if (llvm::CallingConv::X86_64_SysV == cc || llvm::CallingConv::Win64 == cc) {
    return "RAX";

  } else if (llvm::CallingConv::X86_StdCall == cc ||
             llvm::CallingConv::X86_FastCall == cc ||
             llvm::CallingConv::X86_ThisCall == cc) {
    return "EAX";

  } else if (llvm::CallingConv::C == cc) {
    if (arch->IsX86()) {
      return "EAX";  // cdecl.

    } else if (arch->IsAArch64()) {
      return "X0";
    }
  }

  LOG(FATAL) << "Unknown ABI/calling convention: " << cc;
  return nullptr;
}

static const char *FloatReturnValVar(const remill::Arch *arch,
                                     llvm::CallingConv::ID cc,
                                     llvm::Type *type) {
  if (llvm::CallingConv::X86_64_SysV == cc || llvm::CallingConv::Win64 == cc) {
    return "XMM0";

  } else if (llvm::CallingConv::X86_StdCall == cc ||
             llvm::CallingConv::X86_FastCall == cc ||
             llvm::CallingConv::X86_ThisCall == cc) {
    return "ST0";

  } else if (llvm::CallingConv::C == cc) {
    if (arch->IsX86()) {
      return "EAX";  // cdecl.

    } else if (arch->IsAArch64()) {
      if (type->isDoubleTy()) {
        return "D0";
      } else {
        CHECK(type->isFloatTy());
        return "S0";
      }
    }
  }

  LOG(FATAL) << "Cannot decide where to put return value of type "
             << remill::LLVMThingToString(type) << " for calling convention "
             << cc;

  return nullptr;
}

static const char *ReturnValVar(const remill::Arch *arch,
                                llvm::CallingConv::ID cc, llvm::Type *type) {
  if (type->isPointerTy() || type->isIntegerTy()) {
    return IntReturnValVar(arch, cc);
  } else if (type->isX86_FP80Ty()) {
    return "ST0";
  } else if (type->isFloatTy() || type->isDoubleTy()) {
    return FloatReturnValVar(arch, cc, type);
  } else {
    LOG(FATAL) << "Cannot decide where to put return value of type "
               << remill::LLVMThingToString(type) << " for calling convention "
               << cc;
    return nullptr;
  }
}

}  // namespace

CallingConvention::CallingConvention(llvm::CallingConv::ID cc_)
    : arch(remill::GetTargetArch()),
      cc(cc_),
      used_reg_bitmap(0),
      num_loaded_stack_bytes(DefaultUsedStackBytes(cc)),
      num_stored_stack_bytes(0),
      sp_name(StackPointerName(arch)),
      arg_table(ParamTable(arch, cc)),
      ret_table(ReturnTable(arch, cc)) {}

const std::vector<const char *> CallingConvention::RegsFromTable(
    const ArgConstraint *table) const {
  std::vector<const char *> result;
  for (unsigned i = 0; table[i].var_name != nullptr; ++i) {
    result.push_back(table[i].var_name);
  }
  return result;
}

const char *CallingConvention::ReturnRegForType(llvm::Type *type) const {
  return ReturnValVar(arch, cc, type);
}

}  // namespace fcd
