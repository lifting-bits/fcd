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

#include <unordered_set>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/ast_remill/GenerateAST.h"

namespace fcd {

namespace {

static clang::IdentifierInfo *CreateIdentifier(clang::ASTContext &ctx,
                                               std::string name) {
  std::string str = "";
  for (auto chr : name) {
    str.push_back(std::isalnum(chr) ? chr : '_');
  }
  return &ctx.Idents.get(str);
}

static clang::QualType GetQualType(clang::ASTContext &ctx, llvm::Type *type,
                                   bool constant = false) {
  // DLOG(INFO) << "GetQualType: " << (constant ? "constant " : "")
  //            << remill::LLVMThingToString(type);

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

    case llvm::Type::FunctionTyID: {
      auto func = llvm::cast<llvm::FunctionType>(type);
      auto ret = GetQualType(ctx, func->getReturnType(), constant);
      std::vector<clang::QualType> params;
      for (auto param : func->params()) {
        params.push_back(GetQualType(ctx, param, constant));
      }
      auto epi = clang::FunctionProtoType::ExtProtoInfo();
      epi.Variadic = func->isVarArg();
      result = ctx.getFunctionType(ret, params, epi);
    } break;

    case llvm::Type::PointerTyID: {
      auto ptr = llvm::cast<llvm::PointerType>(type);
      result =
          ctx.getPointerType(GetQualType(ctx, ptr->getElementType(), constant));
    } break;

    case llvm::Type::ArrayTyID: {
      auto arr = llvm::cast<llvm::ArrayType>(type);
      auto elm = GetQualType(ctx, arr->getElementType(), constant);
      if (constant) {
        result = ctx.getConstantArrayType(
            elm, llvm::APInt(32, arr->getNumElements()),
            clang::ArrayType::ArraySizeModifier::Normal, 0);
      } else {
        LOG(FATAL) << "Unknown LLVM ArrayType";
      }
    } break;

    default:
      LOG(FATAL) << "Unknown LLVM Type";
      break;
  }

  return result;
}

static clang::Expr *CreateLiteralExpr(clang::ASTContext &ast_ctx,
                                      clang::DeclContext *decl_ctx,
                                      llvm::Constant *constant) {
  auto type = GetQualType(ast_ctx, constant->getType(), /*constant=*/true);

  clang::Expr *result = nullptr;
  if (auto integer = llvm::dyn_cast<llvm::ConstantInt>(constant)) {
    result = clang::IntegerLiteral::Create(ast_ctx, integer->getValue(), type,
                                           clang::SourceLocation());
  } else if (auto floating = llvm::dyn_cast<llvm::ConstantFP>(constant)) {
    result = clang::FloatingLiteral::Create(ast_ctx, floating->getValueAPF(),
                                            /*isexact=*/true, type,
                                            clang::SourceLocation());
  } else if (auto array = llvm::dyn_cast<llvm::ConstantDataArray>(constant)) {
    CHECK(array->isString()) << "ConstantArray is not a string";
    result = clang::StringLiteral::Create(
        ast_ctx, array->getAsString(), clang::StringLiteral::StringKind::Ascii,
        /*Pascal=*/false, type, clang::SourceLocation());
  }

  return result;
}

static clang::VarDecl *CreateVarDecl(clang::ASTContext &ast_ctx,
                                     clang::DeclContext *decl_ctx,
                                     llvm::Type *type, std::string name,
                                     bool constant = false) {
  DLOG(INFO) << "Creating VarDecl for " << name;
  return clang::VarDecl::Create(
      ast_ctx, decl_ctx, clang::SourceLocation(), clang::SourceLocation(),
      CreateIdentifier(ast_ctx, name), GetQualType(ast_ctx, type, constant),
      nullptr, clang::SC_None);
}

}  // namespace

ASTGenerator::ASTGenerator(clang::CompilerInstance &ins)
    : cc_ins(&ins), ast_ctx(cc_ins->getASTContext()) {}

void ASTGenerator::VisitGlobalVar(llvm::GlobalVariable &gvar) {
  DLOG(INFO) << "VisitGlobalVar: " << remill::LLVMThingToString(&gvar);
  auto &var = decls[&gvar];
  if (!var) {
    auto name = gvar.getName().str();
    auto decl_ctx = ast_ctx.getTranslationUnitDecl();
    auto type = llvm::cast<llvm::PointerType>(gvar.getType())->getElementType();

    var = CreateVarDecl(ast_ctx, decl_ctx, type, name, /*constant=*/true);

    if (gvar.hasInitializer()) {
      auto tmp = llvm::cast<clang::VarDecl>(var);
      tmp->setInit(CreateLiteralExpr(ast_ctx, decl_ctx, gvar.getInitializer()));
    }

    decl_ctx->addDecl(var);
  }
}

void ASTGenerator::VisitFunctionDecl(llvm::Function &func) {
  auto name = func.getName().str();
  DLOG(INFO) << "VisitFunctionDecl: " << name;
  auto &decl = decls[&func];
  if (!decl) {
    DLOG(INFO) << "Creating FunctionDecl for " << name;
    auto decl_ctx = ast_ctx.getTranslationUnitDecl();
    auto type = llvm::cast<llvm::PointerType>(func.getType())->getElementType();

    decl = clang::FunctionDecl::Create(
        ast_ctx, decl_ctx, clang::SourceLocation(), clang::SourceLocation(),
        clang::DeclarationName(CreateIdentifier(ast_ctx, name)),
        GetQualType(ast_ctx, type), nullptr, clang::SC_None, false);

    if (!func.arg_empty()) {
      auto func_ctx = llvm::cast<clang::FunctionDecl>(decl);
      std::vector<clang::ParmVarDecl *> params;
      for (auto &arg : func.args()) {
        auto arg_name = arg.hasName() ? arg.getName().str()
                                      : "arg" + std::to_string(arg.getArgNo());

        DLOG(INFO) << "Creating ParmVarDecl for " << arg_name;

        auto param = clang::ParmVarDecl::Create(
            ast_ctx, func_ctx, clang::SourceLocation(), clang::SourceLocation(),
            CreateIdentifier(ast_ctx, arg_name),
            GetQualType(ast_ctx, arg.getType()), nullptr, clang::SC_None,
            nullptr);
        decls[&arg] = param;
        params.push_back(param);
      }

      func_ctx->setParams(params);
    }

    decl_ctx->addDecl(decl);
  }
}

void ASTGenerator::VisitFunctionDefn(llvm::Function &func) {
  auto name = func.getName().str();
  DLOG(INFO) << "VisitFunctionDefn: " << name;
  auto &compound = stmts[&func];
  if (!compound) {
    std::vector<clang::Stmt *> compounds;
    for (auto &block : func) {
      auto stmt = stmts[&block];
      CHECK(stmt) << "CompoundStmt for block doesn't exist";
      compounds.push_back(stmt);
    }

    compound = new (ast_ctx) clang::CompoundStmt(
        ast_ctx, compounds, clang::SourceLocation(), clang::SourceLocation());

    if (auto decl = llvm::dyn_cast<clang::FunctionDecl>(decls[&func])) {
      decl->setBody(compound);
    } else {
      LOG(FATAL) << "FunctionDecl for function doesn't exist";
    }
  }
}

void ASTGenerator::VisitBasicBlock(llvm::BasicBlock &block) {
  auto name = block.hasName() ? block.getName().str() : "<no_name>";
  DLOG(INFO) << "VisitBasicBlock: " << name;
  auto &compound = stmts[&block];
  if (!compound) {
    DLOG(INFO) << "Creating CompoundStmt for " << name;
    visit(block);
    std::vector<clang::Stmt *> block_stmts;
    for (auto &inst : block) {
      block_stmts.push_back(stmts[&inst]);
    }
    compound = new (ast_ctx) clang::CompoundStmt(
        ast_ctx, block_stmts, clang::SourceLocation(), clang::SourceLocation());
  }
}

void ASTGenerator::visitCallInst(llvm::CallInst &inst) {
  DLOG(INFO) << "visitCallInst: " << remill::LLVMThingToString(&inst);
}

void ASTGenerator::visitAllocaInst(llvm::AllocaInst &inst) {
  DLOG(INFO) << "visitAllocaInst: " << remill::LLVMThingToString(&inst);
  auto &var = decls[&inst];
  if (!var) {
    auto decl_ctx = llvm::dyn_cast<clang::FunctionDecl>(
        decls[inst.getParent()->getParent()]);
    CHECK(decl_ctx != nullptr) << "Undeclared function";
    // Make a name
    auto name =
        inst.hasName()
            ? inst.getName().str()
            : "var" + std::to_string(std::distance(decl_ctx->decls_begin(),
                                                   decl_ctx->decls_end()));
    // Declare a variable
    var = CreateVarDecl(ast_ctx, decl_ctx, inst.getAllocatedType(), name);
    // Add it to the current DeclContext
    decl_ctx->addDecl(var);
  }
}

void ASTGenerator::visitLoadInst(llvm::LoadInst &inst) {
  DLOG(INFO) << "visitLoadInst: " << remill::LLVMThingToString(&inst);
  auto &ref = stmts[&inst];
  if (!ref) {
    auto decl_ctx = llvm::dyn_cast<clang::FunctionDecl>(
        decls[inst.getParent()->getParent()]);
    CHECK(decl_ctx != nullptr) << "Undeclared function";
    auto ptr = inst.getPointerOperand();
    if (llvm::isa<llvm::AllocaInst>(ptr) ||
        llvm::isa<llvm::GlobalObject>(ptr)) {
      DLOG(INFO) << "Loading from a variable";
      if (auto var = llvm::dyn_cast<clang::VarDecl>(decls[ptr])) {
        ref = clang::DeclRefExpr::Create(
            ast_ctx, var->getQualifierLoc(), clang::SourceLocation(), var,
            false, var->getLocation(), var->getType(), clang::VK_LValue);
      }
    } else if (llvm::isa<llvm::GetElementPtrInst>(ptr)) {
      DLOG(INFO) << "Loading from an aggregate";
    }
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

  ins.getASTContext().getTranslationUnitDecl()->dump();
  ins.getASTContext().getTranslationUnitDecl()->print(llvm::outs());

  return true;
}

llvm::ModulePass *createGenerateASTPass(void) { return new GenerateAST; }

}  // namespace fcd

// using namespace llvm;
// using GenerateAST = fcd::GenerateAST;
// INITIALIZE_PASS(GenerateAST, "generate_ast",
//                 "Generate clang AST from LLVM IR", true, false)