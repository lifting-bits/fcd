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

#include <clang/Basic/Builtins.h>
#include <clang/Basic/TargetInfo.h>
#include "clang/Lex/Preprocessor.h"

#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/ast_remill/GenerateAST.h"

namespace fcd {

ASTGenerator::ASTGenerator(clang::CompilerInstance &ins)
    : cc_ins(&ins), decl_ctx(ins.getASTContext().getTranslationUnitDecl()) {}

clang::QualType ASTGenerator::GetClangQualType(llvm::Type *type) {
  return cc_ins->getASTContext().VoidPtrTy;
}

void ASTGenerator::visitCallInst(llvm::CallInst &inst) {
  DLOG(INFO) << "visitCallInst: " << remill::LLVMThingToString(&inst);
}

void ASTGenerator::visitAllocaInst(llvm::AllocaInst &inst) {
  DLOG(INFO) << "visitAllocaInst: " << remill::LLVMThingToString(&inst);
  // Create an identifier
  auto &itable = cc_ins->getPreprocessor().getIdentifierTable();
  std::stringstream name;
  // Make a name
  if (inst.hasName()) {
    name << inst.getName().str();
  } else {
    name << "var"
         << std::distance(decl_ctx->decls_begin(), decl_ctx->decls_end());
  }
  // Declare a variable
  auto var = clang::VarDecl::Create(
      cc_ins->getASTContext(), decl_ctx, clang::SourceLocation(),
      clang::SourceLocation(), &itable.get(name.str()),
      GetClangQualType(inst.getType()), nullptr, clang::SC_None);
  // Add it to the current DeclContext
  decl_ctx->addDecl(var);
}

void ASTGenerator::visitLoadInst(llvm::LoadInst &inst) {
  DLOG(INFO) << "visitLoadInst: " << remill::LLVMThingToString(&inst);
}

void ASTGenerator::visitStoreInst(llvm::StoreInst &inst) {
  DLOG(INFO) << "visitStoreInst: " << remill::LLVMThingToString(&inst);
}

// void ASTGenerator::visitInstruction(llvm::Instruction &inst) {
//   DLOG(INFO) << "visitInstruction: " << remill::LLVMThingToString(&inst);
// }

namespace {

static void CFGSlice(
    llvm::BasicBlock *source, llvm::BasicBlock *sink,
    std::unordered_map<llvm::BasicBlock *, llvm::BasicBlock *> &result) {}
}

void GenerateAST::GetOrCreateAST(llvm::BasicBlock *block) {
  auto name = block->hasName() ? block->getName().str() : "<no_name>";
  DLOG(INFO) << "Generating AST for block " << name;
  ast_gen->visit(block);
}

void GenerateAST::StructureAcyclicRegion(llvm::Region *region) {
  DLOG(INFO) << "Structuring acyclic region " << region->getNameStr();
}

void GenerateAST::StructureCyclicRegion(llvm::Region *region) {
  DLOG(INFO) << "Structuring cyclic region " << region->getNameStr();
}

char GenerateAST::ID = 0;

GenerateAST::GenerateAST(void) : ModulePass(GenerateAST::ID) {}

void GenerateAST::getAnalysisUsage(llvm::AnalysisUsage &usage) const {
  usage.addRequired<llvm::DominatorTreeWrapperPass>();
  usage.addRequired<llvm::RegionInfoPass>();
}

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

  ast_gen = std::make_unique<ASTGenerator>(ins);

  using CFGEdge = std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>;

  for (auto &func : module) {
    if (func.isDeclaration()) {
      continue;
    }
    DLOG(INFO) << "Generating AST for function " << func.getName().str();
    // Compute back edges using a DFS walk of the CFG
    llvm::SmallVector<CFGEdge, 10> back_edges;
    llvm::FindFunctionBackedges(func, back_edges);
    // Extract loop headers
    std::unordered_set<const llvm::BasicBlock *> loop_headers;
    for (auto edge : back_edges) {
      loop_headers.insert(edge.second);
    }
    // Walk the CFG in post-order and structurize regions
    auto regions = &getAnalysis<llvm::RegionInfoPass>(func).getRegionInfo();
    auto po_walk = llvm::make_range(llvm::po_begin(&func), llvm::po_end(&func));
    for (auto block : po_walk) {
      GetOrCreateAST(block);
      // Check if `block` is the head of a region
      auto region = regions->getRegionFor(block);
      if (block == region->getEntry()) {
        // Check if `region` contains a cycle
        if (loop_headers.count(block) > 0) {
          StructureCyclicRegion(region);
        } else {
          StructureAcyclicRegion(region);
        }
      }
    }
  }

  // ins.getASTContext().getTranslationUnitDecl()->dump();
  // ins.getASTContext().getTranslationUnitDecl()->print(llvm::outs());

  return true;
}

llvm::ModulePass *createGenerateASTPass(void) { return new GenerateAST; }

}  // namespace fcd

// using namespace llvm;
// using GenerateAST = fcd::GenerateAST;
// INITIALIZE_PASS(GenerateAST, "generate_ast",
//                 "Generate clang AST from LLVM IR", true, false)