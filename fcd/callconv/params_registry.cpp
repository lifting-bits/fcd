//
// params_registry.cpp
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#include <gflags/gflags.h>
#include <glog/logging.h>

#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Module.h>

#include "anyarch_anycc.h"
#include "anyarch_interactive.h"
#include "call_conv.h"
#include "command_line.h"
#include "executable.h"
#include "metadata.h"
#include "params_registry.h"
#include "pass_executable.h"

#include <iostream>

DEFINE_string(callconv, "auto",
              "Call convention to use when detecting function arguments.");

using namespace llvm;
using namespace std;

ModRefInfo CallInformation::getRegisterModRef(
    const TargetRegisterInfo& reg) const {
  // If it's a return value, then Mod;
  // if it's a parameter, then Ref;
  // otherwise, NoModRef, as far as the call information is concerned.
  // Two notable exceptions are the instruction pointer and the stack pointer,
  // which have to be handled out of here.

  cerr << "GECKO" << endl;

  underlying_type_t<ModRefInfo> result = MRI_NoModRef;
  auto retBegin = return_begin();
  for (auto iter = begin(); iter != end(); ++iter) {
    if (iter->type == ValueInformation::IntegerRegister &&
        &reg == iter->registerInfo) {
      result |= iter < retBegin ? MRI_Ref : MRI_Mod;
    }
  }

  return static_cast<ModRefInfo>(result);
}

ModRefInfo ParameterRegistryAAResults::getModRefInfo(
    ImmutableCallSite cs, const MemoryLocation& loc) {
  if (auto func = cs.getCalledFunction()) {
    auto iter = callInformation.find(func);
    if (iter != callInformation.end())
      if (const TargetRegisterInfo* info = targetInfo->registerInfo(*loc.Ptr)) {
        cerr << "SATAN" << endl;
        return iter->second.getRegisterModRef(*info);
      }
  }
  cerr << "BELZEBUB" << endl;
  return AAResultBase::getModRefInfo(cs, loc);
}

char ParameterRegistry::ID = 0;

CallInformation* ParameterRegistry::analyzeFunction(Function& func) {
  CallInformation& info = aaResults->callInformation[&func];
  if (info.getStage() == CallInformation::New) {
    for (CallingConvention* cc : ccChain) {
      DLOG(INFO) << "Analyzing function " << func.getName().str()
                 << " with calling convention " << cc->getName();

      info.setStage(CallInformation::Analyzing);
      if (cc->analyzeFunction(*this, info, func)) {
        info.setCallingConvention(cc);
        info.setStage(CallInformation::Completed);
        break;
      } else {
        info.setStage(CallInformation::New);
        info.clear();
      }
    }

    if (info.getStage() != CallInformation::Completed) {
      info.setStage(CallInformation::Failed);
    }
  }

  return info.getStage() == CallInformation::Completed ? &info : nullptr;
}

Executable* ParameterRegistry::getExecutable() {
  return getAnalysis<ExecutableWrapper>().getExecutable();
}

// Returns:
// - a complete entry when parameter inference already succeeded;
// - an empty entry when parameter inference is on the way;
// - nullptr when analysis failed.
// It is possible that analysis returns an empty set, but then returns nullptr.
const CallInformation* ParameterRegistry::getCallInfo(Function& func) {
  assert(!md::isPrototype(func));
  auto iter = aaResults->callInformation.find(&func);
  if (iter == aaResults->callInformation.end()) {
    return analyzing ? analyzeFunction(func) : nullptr;
  }

  return &iter->second;
}

const CallInformation* ParameterRegistry::getDefinitionCallInfo(
    Function& func) {
  assert(md::isPrototype(func));

  CallInformation& info = aaResults->callInformation[&func];
  if (info.getStage() == CallInformation::New) {
    for (CallingConvention* cc : ccChain) {
      DLOG(INFO) << "Analyzing function type of " << func.getName().str()
                 << " with calling convention " << cc->getName();

      if (cc->analyzeFunctionType(*this, info, *func.getFunctionType())) {
        info.setCallingConvention(cc);
        return &info;
      }
    }
  } else if (info.getStage() == CallInformation::Completed) {
    return &info;
  }

  return nullptr;
}

unique_ptr<CallInformation> ParameterRegistry::analyzeCallSite(
    CallSite callSite) {
  unique_ptr<CallInformation> info(new CallInformation);
  for (CallingConvention* cc : ccChain) {
    string calleeName = "(not a function)";
    if (auto callee = callSite.getCalledFunction())
      calleeName = callee->getName();

    DLOG(INFO) << "Analyzing call site for " << calleeName << " in "
               << callSite->getFunction()->getName().str()
               << " with calling convention " << cc->getName();

    info->setStage(CallInformation::Analyzing);
    if (cc->analyzeCallSite(*this, *info, callSite)) {
      info->setCallingConvention(cc);
      info->setStage(CallInformation::Completed);
      return info;
    } else {
      info->setStage(CallInformation::New);
      info->clear();
    }
  }

  info.reset();
  return info;
}

unique_ptr<MemorySSA> ParameterRegistry::createMemorySSA(Function& function) {
  auto& domTree = getAnalysis<DominatorTreeWrapperPass>(function).getDomTree();
  auto& aaResult = getAnalysis<AAResultsWrapperPass>(function).getAAResults();

  // XXX: don't explicitly depend on this other AA pass
  // This will be easier once we move over to the new pass infrastructure
  aaResult.addAAResult(*aaHack);

  return std::make_unique<MemorySSA>(function, &aaResult, &domTree);
}

MemorySSA* ParameterRegistry::getMemorySSA(Function& function) {
  unsigned version = md::getFunctionVersion(function);
  auto iter = mssas.find(&function);
  if (iter == mssas.end()) {
    auto mssa = createMemorySSA(function);
    iter = mssas.insert(make_pair(&function, make_pair(version, move(mssa))))
               .first;
  } else if (iter->second.first != version) {
    iter->second.first = version;
    iter->second.second = createMemorySSA(function);
  }
  return iter->second.second.get();
}

void ParameterRegistry::getAnalysisUsage(AnalysisUsage& au) const {
  au.addRequired<AAResultsWrapperPass>();

  au.addRequired<DominatorTreeWrapperPass>();
  au.addPreserved<DominatorTreeWrapperPass>();

  au.addRequired<TargetLibraryInfoWrapperPass>();
  au.addPreserved<TargetLibraryInfoWrapperPass>();

  au.addRequired<PostDominatorTreeWrapperPass>();
  au.addPreserved<PostDominatorTreeWrapperPass>();

  au.addRequired<ExecutableWrapper>();
  au.addPreserved<ExecutableWrapper>();

  au.addRequired<TargetLibraryInfoWrapperPass>();
  au.addPreserved<TargetLibraryInfoWrapperPass>();

  for (CallingConvention* cc : CallingConvention::getCallingConventions()) {
    cc->getAnalysisUsage(au);
  }

  ModulePass::getAnalysisUsage(au);
}

bool ParameterRegistry::doInitialization(Module& m) {
  if (!(targetInfo = TargetInfo::getTargetInfo(m))) {
    return false;
  }

  return ModulePass::doInitialization(m);
}

bool ParameterRegistry::runOnModule(Module& module) {
  aaHack.reset(new fcd::AddressSpaceAAResult);

  if (FLAGS_callconv != "auto") {
    for (auto cc : CallingConvention::getCallingConventions()) {
      if (FLAGS_callconv == cc->getName()) {
        ccChain.push_back(cc);
      }
    }
  } else if (Executable* executable = getExecutable()) {
    if (auto cc = CallingConvention::getMatchingCallingConvention(
            getTargetInfo(), *executable)) {
      ccChain.push_back(cc);
    }
  }

  CHECK(ccChain.size() > 0) << "No call convention specified or inferred.";

  ccChain.push_back(CallingConvention::getCallingConvention(
      CallingConvention_AnyArch_AnyCC::name));

  ccChain.push_back(CallingConvention::getCallingConvention(
      CallingConvention_AnyArch_Interactive::name));

  aaResults.reset(
      new ParameterRegistryAAResults(TargetInfo::getTargetInfo(module)));

  for (auto& fn : module) {
    if (!fn.empty()) analyzeFunction(fn);
  }

  return false;
}

INITIALIZE_PASS_BEGIN(ParameterRegistry, "paramreg",
                      "ModRef info for registers", false, true)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(CallGraphWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTreeWrapperPass)
INITIALIZE_PASS_END(ParameterRegistry, "paramreg", "ModRef info for registers",
                    false, true)

llvm::ModulePass* createParameterRegistryPass() {
  return new ParameterRegistry;
}
