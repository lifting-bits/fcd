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

#ifndef FCD_COMPAT_ATTRIBUTES_H_
#define FCD_COMPAT_ATTRIBUTES_H_

#include <llvm/IR/Argument.h>

#include <vector>

#include "remill/BC/Compat/Attributes.h"
#include "remill/BC/Version.h"

static llvm::AttributeSet createAttrSet(llvm::LLVMContext &ctx, unsigned idx,
                                        const llvm::AttrBuilder &builder) {
#if LLVM_VERSION_NUMBER <= LLVM_VERSION(4, 0)
  return llvm::AttributeSet::get(ctx, idx, builder);
#else
  return llvm::AttributeSet::get(ctx, builder);
#endif
}

static llvm::AttributeSet createAttrSet(
    llvm::LLVMContext &ctx, unsigned idx,
    llvm::ArrayRef<llvm::Attribute::AttrKind> attrs) {
#if LLVM_VERSION_NUMBER <= LLVM_VERSION(4, 0)
  return llvm::AttributeSet::get(ctx, idx, attrs);
#elif LLVM_VERSION_NUMBER >= LLVM_VERSION(5, 0)
  std::vector<llvm::Attribute> tmp;
  for (auto attr : attrs) {
    tmp.push_back(llvm::Attribute::get(ctx, attr));
  }
  return llvm::AttributeSet::get(ctx, tmp);
#else
  return llvm::AttributeSet::get(ctx, attrs);
#endif
}

static void removeAttr(llvm::Argument &arg, llvm::Attribute::AttrKind attr) {
#if LLVM_VERSION_NUMBER >= LLVM_VERSION(4, 0)
  arg.removeAttr(attr);
#else
  arg.removeAttr(createAttrSet(arg.getContext(), arg.getArgNo() + 1, {attr}));
#endif
}

#endif  // FCD_COMPAT_ATTRIBUTES_H_
