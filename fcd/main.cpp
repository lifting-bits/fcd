//
// main.cpp
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#include <gflags/gflags.h>
#include <glog/logging.h>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/Signals.h>
#include <llvm/Support/SourceMgr.h>

#include <clang/Basic/TargetInfo.h>

#include <list>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "remill/BC/Util.h"

#include "fcd/ast_remill/CondBasedRefine.h"
#include "fcd/ast_remill/DeadStmtElim.h"
#include "fcd/ast_remill/GenerateAST.h"
#include "fcd/ast_remill/IRToASTVisitor.h"
#include "fcd/ast_remill/LoopRefine.h"
#include "fcd/ast_remill/NestedCondProp.h"
#include "fcd/ast_remill/NestedScopeCombiner.h"
#include "fcd/ast_remill/Z3CondSimplify.h"

#include "fcd/ast/ast_passes.h"
#include "fcd/codegen/translation_context_remill.h"
#include "fcd/compat/AliasAnalysis.h"
#include "fcd/compat/AnalysisPasses.h"
#include "fcd/compat/IPO.h"
#include "fcd/compat/Scalar.h"
#include "fcd/header_decls.h"
#include "fcd/metadata.h"
#include "fcd/passes.h"
#include "fcd/python/python_context.h"

DEFINE_bool(partial, false,
            "Only decompile functions specified with -other-entry");
DEFINE_bool(exclusive, false, "More restrictive version of -partial");
DEFINE_bool(module_in, false, "Input file is a LLVM module");
DEFINE_bool(module_out, false, "Output LLVM module");
DEFINE_bool(optimize, true, "Optimize lifted LLVM module");
DEFINE_string(
    other_entry, "",
    "Add entry points from virtual addresses; Requires hexadecimal format");
DEFINE_string(opt, "",
              "Insert LLVM IR passes; Allows passes from LLVM's opt or .py "
              "files; Requires default pass pipeline");
DEFINE_string(pipeline, "default", "Customize opt pass pipeline");
DEFINE_string(headers, "", "Header files to parse for function declarations");
DEFINE_string(frameworks, "",
              "Apple framework dirs to be used for declarations");
DEFINE_string(includes, "", "Directories to search headers in");

DECLARE_string(arch);
DECLARE_string(os);

namespace {

static PythonContext* sPythonContext;
static llvm::LLVMContext* sLLVMContext;

static std::vector<std::string> sIncludePaths;
static std::vector<std::string> sHeaderFiles;
static std::vector<std::string> sAppleFrameworks;
static std::vector<std::string> sUserPassPipeline;
static std::vector<std::string> sUserPasses;
static std::vector<uint64_t> sUserEntryPoints;

template <typename T>
static std::vector<T> ParseListFlag(std::string flag, char sep) {
  std::vector<T> res;
  std::istringstream f(flag);
  std::string s;
  while (std::getline(f, s, sep)) {
    T tmp;
    std::istringstream(s) >> std::hex >> tmp >> std::dec;
    res.push_back(tmp);
  }
  return res;
}

template <typename T>
std::string errorOf(const llvm::ErrorOr<T>& error) {
  return error.getError().message();
}

static void InitOptPasses(void) {
  auto& pr = *llvm::PassRegistry::getPassRegistry();
  initializeCore(pr);
  initializeVectorization(pr);
  initializeIPO(pr);
  initializeAnalysis(pr);
  initializeTransformUtils(pr);
  initializeInstCombine(pr);
  initializeScalarOpts(pr);
}

static std::vector<llvm::Pass*> CreatePassesFromList(
    const std::vector<std::string>& pass_names) {
  std::vector<llvm::Pass*> result;

  auto TrimString = [](std::string& str) {
    llvm::StringRef ref(str);
    str = ref.trim().str();
    return str;
  };

  auto pr = llvm::PassRegistry::getPassRegistry();
  for (auto name : pass_names) {
    TrimString(name);
    if (!name.empty()) {
      auto ext = llvm::sys::path::extension(name);
      if (ext == ".py" || ext == ".pyc" || ext == ".pyo") {
        if (auto passOrError = sPythonContext->createPass(name)) {
          result.push_back(passOrError.get());
        } else {
          LOG(WARNING) << "Failed to load pass: " << name << ": "
                       << errorOf(passOrError);
        }
      } else if (auto pi = pr->getPassInfo(name)) {
        result.push_back(pi->createPass());
      } else {
        LOG(WARNING) << "Failed to identify pass: " << name;
      }
    }
  }

  return result;
}

static std::unique_ptr<Executable> ParseExecutable(llvm::MemoryBuffer& buffer) {
  auto start = reinterpret_cast<const uint8_t*>(buffer.getBufferStart());
  auto end = reinterpret_cast<const uint8_t*>(buffer.getBufferEnd());
  auto exe = Executable::parse(start, end);
  CHECK(exe) << "Failed to parse executable: " << errorOf(exe);
  return std::move(exe.get());
}

static bool isFullDisassembly() { return !FLAGS_partial && !FLAGS_exclusive; }

static bool isPartialDisassembly() { return FLAGS_partial && !FLAGS_exclusive; }

static bool isExclusiveDisassembly() { return FLAGS_exclusive; }

static std::unique_ptr<llvm::Module> LiftExecutable(Executable& executable) {
  fcd::RemillTranslationContext RTC(*sLLVMContext, executable);
  // Load headers here, since this is the earliest point where we have an
  // executable and a module.
  auto& module = RTC.GetModule();
  auto cdecls = HeaderDeclarations::create(module, sIncludePaths, sHeaderFiles,
                                           sAppleFrameworks, llvm::errs());

  CHECK(cdecls) << "Header file parsing error";

  EntryPointRepository EPR;
  EPR.addProvider(executable);
  EPR.addProvider(*cdecls);

  md::addIncludedFiles(RTC.GetModule(), cdecls->getIncludedFiles());

  std::list<uint64_t> entry_points;
  if (isFullDisassembly()) {
    for (uint64_t addr : EPR.getVisibleEntryPoints()) {
      CHECK(EPR.getInfo(addr))
          << "No symbol info for address " << std::hex << addr << std::dec;
      entry_points.push_back(addr);
    }
  }

  for (uint64_t addr : sUserEntryPoints) {
    CHECK(EPR.getInfo(addr))
        << "User entry point address outside of executable";
    entry_points.push_back(addr);
  }

  CHECK(!entry_points.empty()) << "No entry points found";

  auto IterCondition = [](size_t cur_size, size_t prev_size) {
    // Only decode addresses already in entry_points.
    // Do not add new ones.
    if (isExclusiveDisassembly()) return false;
    // Decode addresses in entry_points and add
    // entry point addresses discovered in them.
    if (isPartialDisassembly()) return prev_size == 0;
    // Decode until there's nothing new to decode.
    return cur_size > prev_size;
  };

  size_t prev_size = 0;
  while (IterCondition(entry_points.size(), prev_size)) {
    prev_size = entry_points.size();
    std::list<uint64_t> new_entry_points;
    for (auto ep_addr : entry_points) {
      for (auto inst_addr : RTC.DecodeFunction(ep_addr)) {
        auto& inst = RTC.GetInstMap().find(inst_addr)->second;
        if (inst.category == remill::Instruction::kCategoryDirectFunctionCall)
          new_entry_points.push_back(inst.branch_taken_pc);
      }
    }
    entry_points.splice(entry_points.begin(), new_entry_points);
    entry_points.sort();
    entry_points.unique();
  }

  for (auto addr : entry_points) {
    auto symbol_info = EPR.getInfo(addr);
    if (auto func = RTC.DeclareFunction(symbol_info->virtualAddress)) {
      if (symbol_info->name.size() > 0) {
        func->setName(symbol_info->name);
      }
    }
  }

  for (auto addr : entry_points) {
    auto func_addr = EPR.getInfo(addr)->virtualAddress;
    auto func = RTC.DefineFunction(func_addr);
    CHECK(func) << "Could not lift function at address " << std::hex
                << func_addr << std::dec;
    if (auto cfunc = cdecls->prototypeForAddress(func_addr)) {
      md::setFinalPrototype(*func, *cfunc);
    }
  }

  RTC.FinalizeModule();

  for (auto& func : RTC.GetModule()) {
    if (!md::isPrototype(func)) {
      if (auto cfunc = cdecls->prototypeForImportName(func.getName())) {
        if (&func != cfunc) {
          md::setIsStub(func);
          md::setFinalPrototype(func, *cfunc);
        }
      }
    }
  }

  // CHECK(llvm::verifyModule(RTC.GetModule()))
  //     << "Lifted IR module is broken.";

  return RTC.TakeModule();
}

static bool RunPassPipeline(llvm::Module& module,
                            std::vector<llvm::Pass*> passes,
                            Executable* executable = nullptr) {
  LOG(INFO) << "Optimizing LLVM IR";

  auto AACallBack = [](llvm::Pass& p, llvm::Function& f, llvm::AAResults& r) {
    if (auto asaa =
            p.getAnalysisIfAvailable<fcd::AddressSpaceAAWrapperPass>()) {
      r.addAAResult(asaa->getResult());
    }
  };

  // Phase 3: make into functions with arguments, run codegen.
  llvm::legacy::PassManager pm;
  pm.add(llvm::createTypeBasedAAWrapperPass());
  pm.add(llvm::createScopedNoAliasAAWrapperPass());
  pm.add(llvm::createBasicAAWrapperPass());
  pm.add(fcd::createAddressSpaceAliasAnalysis());
  pm.add(llvm::createExternalAAWrapperPass(AACallBack));
  for (llvm::Pass* pass : passes) {
    pm.add(pass);
  }
  pm.run(module);

  return true;
}

static void InitCompilerInstance(llvm::Module& module,
                                 clang::CompilerInstance& ins) {
  auto inv = std::make_shared<clang::CompilerInvocation>();

  const char* tmp[] = {""};
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
}

static bool GeneratePseudocode(llvm::Module& module,
                               llvm::raw_ostream& output) {
  LOG(INFO) << "Generating pseudo-C output";

  // Run that module through the output pass
  // UnwrapReturns happens after value propagation because value propagation
  // doesn't know that calls
  // are generally not safe to reorder.
  // AstBackEnd* backend = createAstBackEnd();
  // backend->addPass(new AstRemoveUndef);
  // backend->addPass(new AstConsecutiveCombiner);
  // backend->addPass(new AstNestedCombiner);
  // backend->addPass(new AstConsecutiveCombiner);
  // backend->addPass(new AstSimplifyExpressions);
  // backend->addPass(new AstMergeCongruentVariables);
  // backend->addPass(new AstConsecutiveCombiner);
  // backend->addPass(new AstNestedCombiner);
  // backend->addPass(new AstConsecutiveCombiner);
  // backend->addPass(new AstPrint(output, md::getIncludedFiles(module)));
  // backend->runOnModule(module);

  clang::CompilerInstance ins;
  InitCompilerInstance(module, ins);

  fcd::IRToASTVisitor gen(ins);

  llvm::legacy::PassManager ast;
  ast.add(fcd::createGenerateASTPass(ins, gen));
  ast.add(fcd::createDeadStmtElimPass(ins, gen));
  // ast.add(fcd::createZ3CondSimplifyPass(ins, gen));
  // ast.add(fcd::createDeadStmtElimPass(ins, gen));
  ast.run(module);

  llvm::legacy::PassManager cbr;
  cbr.add(fcd::createNestedCondPropPass(ins, gen));
  cbr.add(fcd::createNestedScopeCombinerPass(ins, gen));
  cbr.add(fcd::createCondBasedRefinePass(ins, gen));
  while(cbr.run(module));

  llvm::legacy::PassManager loop;
  loop.add(fcd::createNestedCondPropPass(ins, gen));
  loop.add(fcd::createNestedScopeCombinerPass(ins, gen));
  loop.add(fcd::createLoopRefinePass(ins, gen));
  // while(loop.run(module));

  llvm::legacy::PassManager fin;
  fin.add(fcd::createZ3CondSimplifyPass(ins, gen));
  fin.add(fcd::createNestedCondPropPass(ins, gen));
  fin.add(fcd::createNestedScopeCombinerPass(ins, gen));
  // fin.run(module);

  // ins.getASTContext().getTranslationUnitDecl()->dump();
  ins.getASTContext().getTranslationUnitDecl()->print(llvm::outs());

  return true;
}

static bool InitOptPassPipeline(std::vector<llvm::Pass*>& passes) {
  InitOptPasses();
  // Default passes
  std::vector<std::string> pass_names = {
      "globaldce",
      // "sroa",
      // "intnarrowing", // fcd
      // "signext", // fcd
      // "instcombine",
      // "intops", // fcd
      // "simplifyconditions", // fcd
      // // <-- custom passes go here with the default pass pipeline
      // "instcombine",
      // "gvn",
      // "simplifycfg",
      // "instcombine"
      // "gvn",
      // "recoverstackframe", // fcd
      // "dse",
      // "sccp",
      // "simplifycfg",
      // "eliminatecasts", // fcd
      // "instcombine",
      // "memssadle", // fcd
      // "dse",
      // "instcombine",
      // "sroa",
      // "instcombine",
      // "globaldce",
      // "simplifycfg",
  };

  if (FLAGS_pipeline == "default") {
    if (!sUserPasses.empty()) {
      auto it = ++std::find(pass_names.begin(), pass_names.end(),
                            "simplifyconditions");
      pass_names.insert(it, sUserPasses.begin(), sUserPasses.end());
    }
    passes = CreatePassesFromList(pass_names);
  } else {
    passes = CreatePassesFromList(sUserPassPipeline);
  }
  return !passes.empty();
}

}  // namespace

int main(int argc, char** argv) {
  std::stringstream ss("");

  google::InitGoogleLogging(argv[0]);
  google::InstallFailureSignalHandler();
  google::SetUsageMessage(ss.str());
  google::ParseCommandLineFlags(&argc, &argv, true);

  sIncludePaths = ParseListFlag<std::string>(FLAGS_includes, ',');
  sHeaderFiles = ParseListFlag<std::string>(FLAGS_headers, ',');
  sAppleFrameworks = ParseListFlag<std::string>(FLAGS_frameworks, ',');
  sUserPassPipeline = ParseListFlag<std::string>(FLAGS_pipeline, ',');
  sUserPasses = ParseListFlag<std::string>(FLAGS_opt, ',');
  sUserEntryPoints = ParseListFlag<uint64_t>(FLAGS_other_entry, ',');

  CHECK(FLAGS_pipeline == "default" || FLAGS_opt.empty())
      << "Inserting LLVM IR passes only allowed when using default pipeline.";

  CHECK(!FLAGS_os.empty()) << "Must specify an operating system name to --os.";

  CHECK(!FLAGS_arch.empty())
      << "Must specify a machine code architecture name to --arch.";

  CHECK(argc > 1) << "Must specify and input file.";

  std::string inputFile = argv[1];

  llvm::LLVMContext llvm;
  PythonContext python(argv[0]);

  sLLVMContext = &llvm;
  sPythonContext = &python;

  // step 0: before even attempting anything, prepare optimization passes
  // (the user won't be happy if we work for 5 minutes only to discover
  // that the optimization passes don't load)
  std::vector<llvm::Pass*> opt_passes;
  CHECK(InitOptPassPipeline(opt_passes))
      << "Failed to initialize pass pipeline";

  std::unique_ptr<Executable> executable;
  std::unique_ptr<llvm::Module> module;

  // step 1: create annotated module from executable (or load it from .ll)
  if (FLAGS_module_in) {
    LOG(INFO) << "Parsing IR from " << inputFile;
    module.reset(remill::LoadModuleFromFile(&llvm, inputFile, false));
  } else {
    LOG(INFO) << "Parsing executable " << inputFile;

    auto buffer = llvm::MemoryBuffer::getFile(inputFile, -1, false);
    CHECK(buffer) << "Failed to open input file: " << errorOf(buffer);

    executable = ParseExecutable(*buffer.get());
    module = LiftExecutable(*executable);
  }

  // step 2: optimize the llvm ir module
  if (FLAGS_optimize) {
    CHECK(RunPassPipeline(*module, opt_passes, executable.get()))
        << "Error while running pass pipeline";
  }

  // step 3 (final step): emit output IR or C pseudocode
  if (FLAGS_module_out) {
    module->print(llvm::outs(), nullptr);
  } else {
    CHECK(GeneratePseudocode(*module, llvm::outs()))
        << "Error while generating C pseudocode";
  }

  google::ShutDownCommandLineFlags();
  google::ShutdownGoogleLogging();

  return 0;
}
