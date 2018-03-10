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

#ifndef FCD_CALLCONV_CALLCONV_REMILL_H_
#define FCD_CALLCONV_CALLCONV_REMILL_H_

#include "remill/Arch/Arch.h"

#include <vector>

namespace fcd {

struct ArgConstraint;

class CallingConvention {
 public:
  explicit CallingConvention(llvm::CallingConv::ID cc_);

  const std::vector<const char*> ParamRegs() const {
    return RegsFromTable(arg_table);
  }
  const std::vector<const char*> ReturnRegs() const {
    return RegsFromTable(ret_table);
  }
  const char *StackPointerVarName(void) const { return sp_name; }
  const char *ReturnRegForType(llvm::Type* type) const;

 private:
  const remill::Arch *arch;
  llvm::CallingConv::ID cc;
  uint64_t used_reg_bitmap;
  uint64_t num_loaded_stack_bytes;
  uint64_t num_stored_stack_bytes;
  const char *const sp_name;
  const ArgConstraint *arg_table;
  const ArgConstraint *ret_table;

  const std::vector<const char*> RegsFromTable(const ArgConstraint *table) const;

  CallingConvention(void) = delete;
};

}  // namespace fcd

#endif  // FCD_CALLCONV_CALLCONV_REMILL_H_