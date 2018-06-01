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

#include <clang/AST/Expr.h>
#include <clang/Basic/Builtins.h>
#include <clang/Basic/TargetInfo.h>
#include "clang/Lex/Preprocessor.h"

#include <sstream>
#include <unordered_set>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/ast_remill/GenerateAST.h"

namespace fcd {

namespace {

static clang::QualType GetClangQualType(clang::ASTContext &ctx,
                                        llvm::Type *type) {
  clang::QualType result;

  switch (type->getTypeID()) {
    case llvm::Type::VoidTyID:
      result = ctx.VoidTy;
      break;

    case llvm::Type::HalfTyID:
      result = ctx.HalfTy;
      break;

    case llvm::Type::FloatTyID:
      result = ctx.FloatTy;
      break;

    case llvm::Type::DoubleTyID:
      result = ctx.DoubleTy;
      break;

    case llvm::Type::IntegerTyID: {
      auto size = type->getIntegerBitWidth();
      CHECK(size > 0) << "Integer bit width has to be greater than 0";
      if (size == 1) {
        result = ctx.BoolTy;
      } else if (size <= 8) {
        result = ctx.UnsignedCharTy;
      } else if (size <= 16) {
        result = ctx.UnsignedShortTy;
      } else if (size <= 32) {
        result = ctx.UnsignedLongTy;
      } else if (size <= 64) {
        result = ctx.UnsignedLongLongTy;
      } else if (size <= 128) {
        result = ctx.UnsignedInt128Ty;
      } else {
        LOG(WARNING)
            << "Integer bit width greater than 128; Returning UnsignedInt128Ty";
        result = ctx.UnsignedInt128Ty;
      }
    } break;

    case llvm::Type::PointerTyID: {
      auto ptr = llvm::cast<llvm::PointerType>(type);
      result = ctx.getPointerType(GetClangQualType(ctx, ptr->getElementType()));
    } break;

    default:
      LOG(FATAL) << "Unknown LLVM Type";
      break;
  }

  return result;
}

static clang::VarDecl *CreateClangVarDecl(clang::ASTContext &ast_ctx,
                                          clang::DeclContext *decl_ctx,
                                          llvm::Type *type, std::string name) {
  return clang::VarDecl::Create(
      ast_ctx, decl_ctx, clang::SourceLocation(), clang::SourceLocation(),
      &ast_ctx.Idents.get(name), GetClangQualType(ast_ctx, type), nullptr,
      clang::SC_None);
}

}  // namespace

ASTGenerator::ASTGenerator(clang::CompilerInstance &ins) : cc_ins(&ins) {}

void ASTGenerator::VisitGlobalVar(llvm::GlobalVariable &gvar) {
  DLOG(INFO) << "VisitGlobalVar: " << remill::LLVMThingToString(&gvar);
  auto &ast_ctx = cc_ins->getASTContext();
  auto decl_ctx = ast_ctx.getTranslationUnitDecl();
  
  auto var = CreateClangVarDecl(ast_ctx, decl_ctx, gvar.getType(),
                                gvar.getName().str());
  
  decl_ctx->addDecl(var);
  decls[&gvar] = var;
}

void ASTGenerator::VisitFunctionDecl(llvm::Function &func) {
  DLOG(INFO) << "VisitFunctionDecl: " << remill::LLVMThingToString(&func);
  auto &ast_ctx = cc_ins->getASTContext();
  auto decl_ctx = ast_ctx.getTranslationUnitDecl();
  auto name = func.getName().str();

  auto cfunc = clang::FunctionDecl::Create(
      ast_ctx, decl_ctx, clang::SourceLocation(), clang::SourceLocation(),
      clang::DeclarationName(&ast_ctx.Idents.get(name)),
      GetClangQualType(ast_ctx, func.getType()), nullptr, clang::SC_None,
      false);

  decl_ctx->addDecl(cfunc);
  decls[&func] = cfunc;
}

void ASTGenerator::visitCallInst(llvm::CallInst &inst) {
  DLOG(INFO) << "visitCallInst: " << remill::LLVMThingToString(&inst);
}

void ASTGenerator::visitAllocaInst(llvm::AllocaInst &inst) {
  DLOG(INFO) << "visitAllocaInst: " << remill::LLVMThingToString(&inst);
  auto &ast_ctx = cc_ins->getASTContext();
  auto decl_ctx =
      llvm::dyn_cast<clang::FunctionDecl>(decls[inst.getParent()->getParent()]);
  CHECK(decl_ctx != nullptr) << "Undeclared function";
  // Make a name
  std::stringstream name;
  if (inst.hasName()) {
    name << inst.getName().str();
  } else {
    name << "var"
         << std::distance(decl_ctx->decls_begin(), decl_ctx->decls_end());
    inst.setName(name.str());
  }
  // Declare a variable
  auto var = CreateClangVarDecl(ast_ctx, decl_ctx, inst.getAllocatedType(),
                                name.str());
  // Add it to the current DeclContext
  decl_ctx->addDecl(var);
  // Add mapping
  decls[&inst] = var;
}

void ASTGenerator::visitLoadInst(llvm::LoadInst &inst) {
  DLOG(INFO) << "visitLoadInst: " << remill::LLVMThingToString(&inst);
  auto &ast_ctx = cc_ins->getASTContext();
  auto decl_ctx =
      llvm::dyn_cast<clang::FunctionDecl>(decls[inst.getParent()->getParent()]);
  CHECK(decl_ctx != nullptr) << "Undeclared function";
  auto ptr = inst.getPointerOperand();
  if (llvm::isa<llvm::AllocaInst>(ptr) || llvm::isa<llvm::GlobalObject>(ptr)) {
    DLOG(INFO) << "Loading from a variable";
    if (auto var = llvm::dyn_cast<clang::VarDecl>(decls[ptr])) {
      auto ref = clang::DeclRefExpr::Create(
          ast_ctx, var->getQualifierLoc(), clang::SourceLocation(), var, false,
          var->getLocation(), var->getType(), clang::VK_LValue);
      stmts[&inst] = ref;
      ref->dump();
    }
  } else if (llvm::isa<llvm::GetElementPtrInst>(ptr)) {
    DLOG(INFO) << "Loading from an aggregate";
  }
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
}  // namespace

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

  for (auto &var : module.globals()) {
    ast_gen->VisitGlobalVar(var);
  }
  
  using CFGEdge = std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>;

  for (auto &func : module.functions()) {
    ast_gen->VisitFunctionDecl(func);
    if (!func.isDeclaration()) {
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
      auto po_walk =
          llvm::make_range(llvm::po_begin(&func), llvm::po_end(&func));
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