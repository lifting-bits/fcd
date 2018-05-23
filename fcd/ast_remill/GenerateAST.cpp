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

#include <clang/AST/ASTContext.h>
#include <clang/Basic/Builtins.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Frontend/CompilerInstance.h>

#include <memory>

#include "fcd/ast_remill/GenerateAST.h"

namespace fcd {

char GenerateAST::ID = 0;

GenerateAST::GenerateAST(void) : ModulePass(GenerateAST::ID) {}

void GenerateAST::getAnalysisUsage(llvm::AnalysisUsage &usage) const {}

bool GenerateAST::runOnModule(llvm::Module &module) {
  clang::CompilerInstance ins;
  auto inv = std::make_shared<clang::CompilerInvocation>();

  const char *tmp[] = {""};
  ins.setDiagnostics(ins.createDiagnostics(new clang::DiagnosticOptions).get());
  clang::CompilerInvocation::CreateFromArgs(*inv, tmp, tmp,
                                            ins.getDiagnostics());

  inv->getTargetOpts().Triple = module.getTargetTriple();
  ins.setInvocation(inv);
  ins.setTarget(clang::TargetInfo::CreateTargetInfo(
      ins.getDiagnostics(), ins.getInvocation().TargetOpts));

  ins.createFileManager();
  ins.createSourceManager(ins.getFileManager());
  ins.createPreprocessor(clang::TU_Complete);
  ins.createASTContext();

  auto &ast = ins.getASTContext();

  ASTGenerator gen(ast);

  gen.visit();

  // auto top = ast.getTranslationUnitDecl();
  return true;
}

}  // namespace fcd

// using namespace llvm;
// using GenerateAST = fcd::GenerateAST;
// INITIALIZE_PASS(GenerateAST, "generate_ast",
//                 "Generate clang AST from LLVM IR", true, false)