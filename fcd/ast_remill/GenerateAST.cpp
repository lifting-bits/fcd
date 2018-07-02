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

#include <llvm/ADT/DepthFirstIterator.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/Analysis/CFG.h>

#include <clang/Basic/TargetInfo.h>

#include <unordered_set>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/ast_remill/GenerateAST.h"

namespace fcd {

namespace {

using CFGEdge = std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>;

static void CFGSlice(llvm::BasicBlock *source, llvm::BasicBlock *sink,
                     std::vector<CFGEdge> &result) {
  // Clear the output container
  result.clear();

  for (auto it = llvm::df_begin(source); it != llvm::df_end(source); ++it) {
    auto path_len = it.getPathLength();
    if (path_len > 1) {
      df_edges.push_back({it.getPath(path_len - 2), *it});
      for (auto succ : llvm::successors(*it)) {
        if (it.nodeVisited(succ)) {
          df_edges.push_back({*it, succ});
        }
      }
    }
  }

  auto LLVMThingToString = [](const llvm::Value *cval) {
    auto val = const_cast<llvm::Value *>(cval);
    return remill::LLVMThingToString(val);
  };

  for (auto edge : df_edges) {
    DLOG(INFO) << "FROM: " << LLVMThingToString(&*edge.first->begin());
    DLOG(INFO) << "TO: " << LLVMThingToString(&*edge.second->begin());
  }
}

}  // namespace

void GenerateAST::StructureAcyclicRegion(llvm::Region *region) {
  DLOG(INFO) << "Structuring acyclic region " << region->getNameStr();
  std::vector<CFGEdge> slice;
  CFGSlice(region->getEntry(), region->getExit(), slice);
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

  for (auto &var : module.globals()) {
    ast_gen->VisitGlobalVar(var);
  }

  for (auto &func : module.functions()) {
    ast_gen->VisitFunctionDecl(func);
  }

  for (auto &func : module.functions()) {
    if (!func.isDeclaration()) {
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
      auto po_walk =
          llvm::make_range(llvm::po_begin(&func), llvm::po_end(&func));
      for (auto block : po_walk) {
        ast_gen->VisitBasicBlock(*block);
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
      ast_gen->VisitFunctionDefn(func);
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