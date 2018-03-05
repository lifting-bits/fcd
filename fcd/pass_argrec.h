//
// pass_argrec.h
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#ifndef fcd__pass_argrec_h
#define fcd__pass_argrec_h

#include <llvm/Analysis/Passes.h>
#include <llvm/IR/Module.h>

#include <unordered_map>
#include <unordered_set>

#include "fcd/callconv/params_registry.h"

namespace fcd {

class ArgumentRecovery : public llvm::ModulePass {
 private:
  std::unordered_map<llvm::Function*, llvm::Value*> registerPtr;
  std::unordered_set<llvm::Function*> functionsToErase;

  llvm::Value* getRegisterPtr(llvm::Function& fn);

  llvm::Function& createParameterizedFunction(llvm::Function& base,
                                              const CallInformation& ci);
  void fixCallSites(llvm::Function& base, llvm::Function& newTarget,
                    const CallInformation& ci);
  llvm::Value* createReturnValue(llvm::Function& function,
                                 const CallInformation& ci,
                                 llvm::Instruction* insertionPoint);
  void updateFunctionBody(llvm::Function& oldFunction,
                          llvm::Function& newTarget, const CallInformation& ci);
  bool recoverArguments(llvm::Function& fn);

 public:
  static char ID;

  ArgumentRecovery() : ModulePass(ID) {}

  static llvm::FunctionType* createFunctionType(
      const CallInformation& ci, llvm::Module& module,
      const std::string& returnTypeName);
  static llvm::FunctionType* createFunctionType(
      const CallInformation& ci, llvm::Module& module,
      const std::string& returnTypeName,
      llvm::SmallVectorImpl<std::string>& parameterNames);
  static llvm::CallInst* createCallSite(TargetInfo& targetInfo,
                                        const CallInformation& ci,
                                        llvm::Value& callee,
                                        llvm::Value& callerRegisters,
                                        llvm::Instruction& insertionPoint);

  void getAnalysisUsage(llvm::AnalysisUsage& au) const override;
  bool runOnModule(llvm::Module& module) override;
};

llvm::ModulePass* createArgumentRecoveryPass();
}  // namespace fcd

namespace llvm {
void initializeArgumentRecoveryPass(PassRegistry&);
}

#endif /* fcd__pass_argrec_h */
