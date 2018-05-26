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

#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/Analysis/CFG.h>

#include <clang/AST/ASTContext.h>
#include <clang/Basic/Builtins.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Frontend/CompilerInstance.h>

#include <memory>
#include <unordered_set>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/ast_remill/GenerateAST.h"

namespace fcd {

ASTGenerator::ASTGenerator(clang::ASTContext *ctx) : ast_ctx(ctx) {}

void ASTGenerator::visitCallInst(llvm::CallInst &inst) {
  DLOG(INFO) << "visitCallInst: " << remill::LLVMThingToString(&inst);
}

void ASTGenerator::visitAllocaInst(llvm::AllocaInst &inst) {
  DLOG(INFO) << "visitAllocaInst: " << remill::LLVMThingToString(&inst);
}

void ASTGenerator::visitLoadInst(llvm::LoadInst &inst) {
  DLOG(INFO) << "visitLoadInst: " << remill::LLVMThingToString(&inst);
}

void ASTGenerator::visitStoreInst(llvm::StoreInst &inst) {
  DLOG(INFO) << "visitStoreInst: " << remill::LLVMThingToString(&inst);
}

namespace {

static void StructureAcyclicRegion() {
  DLOG(INFO) << "Structuring acyclic region";
}

static void StructureCyclicRegion() {
  DLOG(INFO) << "Structuring cyclic region";
}

}  // namespace

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
  // auto top = ast.getTranslationUnitDecl();

  ASTGenerator gen(&ast);
  gen.visit(module);

  using CFGEdge = std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>;

  for (auto &func : module) {
    // Compute back edges using a DFS walk of the CFG
    llvm::SmallVector<CFGEdge, 10> back_edges;
    llvm::FindFunctionBackedges(func, back_edges);
    // Extract loop headers
    std::unordered_set<const llvm::BasicBlock *> loop_headers;
    for (auto edge : back_edges) {
      loop_headers.insert(edge.second);
    }
    // Walk the CFG in post-order and structurize regions
    auto po_walk = llvm::make_range(llvm::po_begin(&func), llvm::po_end(&func));
    for (auto block : po_walk) {
      if (loop_headers.count(block) > 0) {
        StructureCyclicRegion();
      } else {
        StructureAcyclicRegion();
      }
    }
  }

  return true;
}

llvm::ModulePass *createGenerateASTPass(void) { return new GenerateAST; }

}  // namespace fcd

// using namespace llvm;
// using GenerateAST = fcd::GenerateAST;
// INITIALIZE_PASS(GenerateAST, "generate_ast",
//                 "Generate clang AST from LLVM IR", true, false)