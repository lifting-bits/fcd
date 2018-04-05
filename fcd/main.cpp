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

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/Process.h>
#include <llvm/Support/Signals.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "fcd/ast/ast_passes.h"
#include "fcd/callconv/params_registry.h"
#include "fcd/codegen/translation_context_remill.h"
#include "fcd/command_line.h"
#include "fcd/errors.h"
#include "fcd/executables/executable.h"
#include "fcd/header_decls.h"
#include "fcd/main.h"
#include "fcd/metadata.h"
#include "fcd/passes.h"
#include "fcd/python/python_context.h"

#ifdef FCD_DEBUG
[[gnu::used]] llvm::raw_ostream& llvm_errs() { return llvm::errs(); }
#endif

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
DEFINE_string(pipeline, "default",
              "Customize opt pass pipeline; \"\" opens $EDITOR;");
DEFINE_string(headers, "", "Header files to parse for function declarations");
DEFINE_string(frameworks, "",
              "Apple framework dirs to be used for declarations");
DEFINE_string(includes, "", "Directories to search headers in");

DECLARE_string(arch);
DECLARE_string(os);

namespace {
template <typename T>
std::vector<T> parseListFlag(std::string flag, char sep) {
  std::vector<T> res;
  std::istringstream f(flag);
  std::string s;
  while (getline(f, s, sep)) {
    T tmp;
    std::istringstream(s) >> std::hex >> tmp >> std::dec;
    res.push_back(tmp);
  }
  return res;
}

auto headerSearchPath = parseListFlag<std::string>(FLAGS_includes, ',');
auto headers = parseListFlag<std::string>(FLAGS_headers, ',');
auto frameworks = parseListFlag<std::string>(FLAGS_frameworks, ',');
auto customPassPipeline = parseListFlag<std::string>(FLAGS_pipeline, ',');
auto additionalPasses = parseListFlag<std::string>(FLAGS_opt, ',');
auto additionalEntryPoints = parseListFlag<uint64_t>(FLAGS_other_entry, ',');

template <typename T>
std::string errorOf(const llvm::ErrorOr<T>& error) {
  return error.getError().message();
}

template <typename TAction>
size_t forEachCall(llvm::Function* callee, unsigned stringArgumentIndex,
                   TAction&& action) {
  size_t count = 0;
  for (llvm::Use& use : callee->uses()) {
    if (auto call = llvm::dyn_cast<llvm::CallInst>(use.getUser())) {
      std::unique_ptr<llvm::Instruction> eraseIfNecessary;
      llvm::Value* operand = call->getOperand(stringArgumentIndex);
      if (auto constant = llvm::dyn_cast<llvm::ConstantExpr>(operand)) {
        eraseIfNecessary.reset(constant->getAsInstruction());
        operand = eraseIfNecessary.get();
      }

      if (auto gep = llvm::dyn_cast<llvm::GetElementPtrInst>(operand))
        if (auto global =
                llvm::dyn_cast<llvm::GlobalVariable>(gep->getOperand(0)))
          if (auto dataArray = llvm::dyn_cast<llvm::ConstantDataArray>(
                  global->getInitializer())) {
            action(dataArray->getAsString().str());
            count++;
          }
    }
  }
  return count;
}

class Main {
  int argc;
  char** argv;

  llvm::LLVMContext llvm;
  PythonContext python;
  std::vector<llvm::Pass*> optimizeAndTransformPasses;

  static void aliasAnalysisHooks(llvm::Pass& pass, llvm::Function& fn,
                                 llvm::AAResults& aar) {
    if (auto prgmem =
            pass.getAnalysisIfAvailable<fcd::AddressSpaceAAWrapperPass>()) {
      aar.addAAResult(prgmem->getResult());
    }
    if (auto params = pass.getAnalysisIfAvailable<ParameterRegistry>()) {
      aar.addAAResult(params->getAAResult());
    }
  }
  std::vector<llvm::Pass*> createPassesFromList(
      const std::vector<std::string>& passNames) {
    std::vector<llvm::Pass*> result;
    llvm::PassRegistry* pr = llvm::PassRegistry::getPassRegistry();
    for (auto passName : passNames) {
      auto begin = passName.begin();
      while (begin != passName.end()) {
        if (isspace(*begin)) {
          ++begin;
        } else {
          break;
        }
      }
      passName.erase(passName.begin(), begin);

      auto rbegin = passName.rbegin();
      while (rbegin != passName.rend()) {
        if (isspace(*rbegin)) {
          ++rbegin;
        } else {
          break;
        }
      }
      passName.erase(rbegin.base(), passName.end());

      if (passName.size() > 0 && passName[0] != '#') {
        auto ext = llvm::sys::path::extension(passName);
        if (ext == ".py" || ext == ".pyc" || ext == ".pyo") {
          if (auto passOrError = python.createPass(passName)) {
            result.push_back(passOrError.get());
          } else {
            std::cerr << getProgramName() << ": couldn't load " << passName
                      << ": " << errorOf(passOrError) << std::endl;
            return std::vector<llvm::Pass*>();
          }
        } else if (const llvm::PassInfo* pi = pr->getPassInfo(passName)) {
          result.push_back(pi->createPass());
        } else {
          std::cerr << getProgramName() << ": couldn't identify pass "
                    << passName << std::endl;
          return std::vector<llvm::Pass*>();
        }
      }
    }

    if (result.size() == 0) {
      llvm::errs() << getProgramName() << ": empty pass list\n";
    }
    return result;
  }

  std::vector<llvm::Pass*> interactivelyEditPassPipeline(
      const std::string& editor, const std::vector<std::string>& basePasses) {
    int fd;
    llvm::SmallVector<char, 100> path;
    if (auto errorCode = llvm::sys::fs::createTemporaryFile("fcd-pass-pipeline",
                                                            "txt", fd, path)) {
      llvm::errs() << getProgramName()
                   << ": can't open temporary file for editing: "
                   << errorCode.message() << "\n";
      return std::vector<llvm::Pass*>();
    }

    llvm::raw_fd_ostream passListOs(fd, true);
    passListOs << "# Enter the name of the LLVM or fcd passes that you want to "
                  "run on the module.\n";
    passListOs << "# Files starting with a # symbol are ignored.\n";
    passListOs << "# Names ending with .py are assumed to be Python scripts "
                  "implementing passes.\n";
    for (const std::string& passName : basePasses) {
      passListOs << passName << '\n';
    }
    passListOs.flush();

    // shell escape temporary path
    std::string escapedPath;
    escapedPath.reserve(path.size());
    for (char c : path) {
      if (c == '\'' || c == '\\') {
        escapedPath.push_back('\\');
      }
      escapedPath.push_back(c);
    }

    std::string editCommand;
    llvm::raw_string_ostream(editCommand)
        << editor << " '" << escapedPath << "'";
    if (int errorCode = system(editCommand.c_str())) {
      llvm::errs()
          << getProgramName()
          << ": interactive pass pipeline: editor returned status code "
          << errorCode << '\n';
      return std::vector<llvm::Pass*>();
    }

    std::ifstream passListIs(path.data());
    assert(static_cast<bool>(passListIs));

    std::string inputLine;
    std::vector<std::string> lines;
    while (getline(passListIs, inputLine)) {
      lines.push_back(inputLine);
      inputLine.clear();
    }
    if (inputLine.size() != 0) {
      lines.push_back(inputLine);
    }

    return createPassesFromList(lines);
  }

 public:
  Main(int argc, char** argv) : argc(argc), argv(argv), python(argv[0]) {
    (void)argc;
    (void)this->argc;
  }

  std::string getProgramName() { return llvm::sys::path::stem(argv[0]); }
  llvm::LLVMContext& getContext() { return llvm; }

  llvm::ErrorOr<std::unique_ptr<Executable>> parseExecutable(
      llvm::MemoryBuffer& executableCode) {
    auto start =
        reinterpret_cast<const uint8_t*>(executableCode.getBufferStart());
    auto end = reinterpret_cast<const uint8_t*>(executableCode.getBufferEnd());
    return Executable::parse(start, end);
  }

  std::unique_ptr<llvm::Module> generateAnnotatedModule(
      Executable& executable) {
    fcd::RemillTranslationContext RTC(llvm, executable);
    // Load headers here, since this is the earliest point where we have an
    // executable and a module.
    auto& module = RTC.GetModule();
    auto cdecls = HeaderDeclarations::create(module, headerSearchPath, headers,
                                             frameworks, llvm::errs());

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

    for (uint64_t addr : additionalEntryPoints) {
      CHECK(EPR.getInfo(addr))
          << "Additional entry address points outside of executable";
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

    // Perform early optimizations to make the module suitable for analysis
    // legacy::PassManager phaseOne;
    // phaseOne.add(llvm::createExternalAAWrapperPass(&Main::aliasAnalysisHooks));
    // phaseOne.add(llvm::createDeadCodeEliminationPass());
    // phaseOne.add(llvm::createInstructionCombiningPass());
    // phaseOne.add(createRegisterPointerPromotionPass());
    // phaseOne.add(llvm::createGVNPass());
    // phaseOne.add(llvm::createDeadStoreEliminationPass());
    // phaseOne.add(llvm::createInstructionCombiningPass());
    // phaseOne.add(llvm::createGlobalDCEPass());

    RTC.FinalizeModule();
    // phaseOne.run(RTC.GetModule());
    // RTC.FinalizeModule();

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

  bool optimizeAndTransformModule(llvm::Module& module,
                                  llvm::raw_ostream& errorOutput,
                                  Executable* executable = nullptr) {
    llvm::PrettyStackTraceString optimize("Optimizing LLVM IR");

    // Phase 3: make into functions with arguments, run codegen.
    llvm::legacy::PassManager pm;
    pm.add(llvm::createTypeBasedAAWrapperPass());
    pm.add(llvm::createScopedNoAliasAAWrapperPass());
    pm.add(llvm::createBasicAAWrapperPass());
    pm.add(fcd::createAddressSpaceAliasAnalysis());
    pm.add(new ExecutableWrapper(executable));
    pm.add(createParameterRegistryPass());
    pm.add(llvm::createExternalAAWrapperPass(&Main::aliasAnalysisHooks));
    for (llvm::Pass* pass : optimizeAndTransformPasses) {
      pm.add(pass);
    }
    pm.run(module);

#ifdef FCD_DEBUG
    // if (llvm::verifyModule(module, &errorOutput)) {
    //   // errors!
    //   return false;
    // }
#endif
    return true;
  }

  bool generateEquivalentPseudocode(llvm::Module& module,
                                    llvm::raw_ostream& output) {
    llvm::PrettyStackTraceString pseudocode("Generating pseudo-C output");

    // Run that module through the output pass
    // UnwrapReturns happens after value propagation because value propagation
    // doesn't know that calls
    // are generally not safe to reorder.
    AstBackEnd* backend = createAstBackEnd();
    backend->addPass(new AstRemoveUndef);
    backend->addPass(new AstConsecutiveCombiner);
    backend->addPass(new AstNestedCombiner);
    backend->addPass(new AstConsecutiveCombiner);
    backend->addPass(new AstSimplifyExpressions);
    backend->addPass(new AstMergeCongruentVariables);
    backend->addPass(new AstConsecutiveCombiner);
    backend->addPass(new AstNestedCombiner);
    backend->addPass(new AstConsecutiveCombiner);
    backend->addPass(new AstPrint(output, md::getIncludedFiles(module)));
    backend->runOnModule(module);
    return true;
  }

  static void initializePasses() {
    auto& pr = *llvm::PassRegistry::getPassRegistry();
    initializeCore(pr);
    initializeVectorization(pr);
    initializeIPO(pr);
    initializeAnalysis(pr);
    initializeTransformUtils(pr);
    initializeInstCombine(pr);
    initializeScalarOpts(pr);

    initializeParameterRegistryPass(pr);
    initializeArgumentRecoveryPass(pr);
  }

  bool prepareOptimizationPasses() {
    // Default passes
    std::vector<std::string> passNames = {
        "globaldce",
        "fixindirects",  // fcd
        "argrec",        // fcd
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
        // "instcombine",
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
      if (additionalPasses.size() > 0) {
        auto extensionPoint =
            find(passNames.begin(), passNames.end(), "simplifyconditions") + 1;
        passNames.insert(extensionPoint, additionalPasses.begin(),
                         additionalPasses.end());
      }
      optimizeAndTransformPasses = createPassesFromList(passNames);
    } else if (FLAGS_pipeline == "") {
      if (auto editor = getenv("EDITOR")) {
        optimizeAndTransformPasses =
            interactivelyEditPassPipeline(editor, passNames);
      } else {
        llvm::errs() << getProgramName()
                     << ": environment has no EDITOR variable; "
                        "pass pipeline can't be edited "
                        "interactively\n";
        return false;
      }
    } else {
      optimizeAndTransformPasses = createPassesFromList(customPassPipeline);
      if (optimizeAndTransformPasses.size() == 0) {
        llvm::errs() << getProgramName() << ": empty custom pass list\n";
      }
    }
    return optimizeAndTransformPasses.size() > 0;
  }
};
}  // namespace

bool isFullDisassembly() { return !FLAGS_partial && !FLAGS_exclusive; }

bool isPartialDisassembly() { return FLAGS_partial && !FLAGS_exclusive; }

bool isExclusiveDisassembly() { return FLAGS_exclusive; }

bool isEntryPoint(uint64_t vaddr) {
  return any_of(additionalEntryPoints.begin(), additionalEntryPoints.end(),
                [&](uint64_t entryPoint) { return vaddr == entryPoint; });
}

int main(int argc, char** argv) {
  std::stringstream ss("");

  llvm::EnablePrettyStackTrace();
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  google::InitGoogleLogging(argv[0]);
  google::SetUsageMessage(ss.str());
  google::ParseCommandLineFlags(&argc, &argv, true);

  headerSearchPath = parseListFlag<std::string>(FLAGS_includes, ',');
  headers = parseListFlag<std::string>(FLAGS_headers, ',');
  frameworks = parseListFlag<std::string>(FLAGS_frameworks, ',');
  customPassPipeline = parseListFlag<std::string>(FLAGS_pipeline, ',');
  additionalPasses = parseListFlag<std::string>(FLAGS_opt, ',');
  additionalEntryPoints = parseListFlag<uint64_t>(FLAGS_other_entry, ',');

  CHECK(FLAGS_pipeline == "default" || FLAGS_opt.empty())
      << "Inserting LLVM IR passes only allowed when using default pipeline.";

  CHECK(!FLAGS_os.empty()) << "Must specify an operating system name to --os.";

  CHECK(!FLAGS_arch.empty())
      << "Must specify a machine code architecture name to --arch.";

  CHECK(argc > 1) << "Must specify and input file.";

  std::string inputFile = argv[1];

  Main::initializePasses();

  Main mainObj(argc, argv);
  std::string program = mainObj.getProgramName();

  // step 0: before even attempting anything, prepare optimization passes
  // (the user won't be happy if we work for 5 minutes only to discover that the
  // optimization passes don't load)
  if (!mainObj.prepareOptimizationPasses()) {
    return 1;
  }

  std::unique_ptr<Executable> executable;
  std::unique_ptr<llvm::Module> module;

  // step one: create annotated module from executable (or load it from .ll)
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> bufferOrError(nullptr);
  if (FLAGS_module_in) {
    llvm::PrettyStackTraceFormat parsingIR("Parsing IR from \"%s\"",
                                           inputFile.c_str());

    llvm::SMDiagnostic errors;
    module = parseIRFile(inputFile, errors, mainObj.getContext());
    if (!module) {
      errors.print(argv[0], llvm::errs());
      return 1;
    }
  } else {
    llvm::PrettyStackTraceFormat parsingIR("Parsing executable \"%s\"",
                                           inputFile.c_str());

    bufferOrError = llvm::MemoryBuffer::getFile(inputFile, -1, false);
    if (!bufferOrError) {
      std::cerr << program << ": can't open " << inputFile << ": "
                << errorOf(bufferOrError) << std::endl;
      return 1;
    }

    auto executableOrError = mainObj.parseExecutable(*bufferOrError.get());
    if (!executableOrError) {
      std::cerr << program << ": couldn't parse " << inputFile << ": "
                << errorOf(executableOrError) << std::endl;
      return 1;
    }

    executable = move(executableOrError.get());
    module = mainObj.generateAnnotatedModule(*executable);
  }

  // Make sure that the module is legal
  size_t errorCount = 0;
  if (llvm::Function* assertionFailure =
          module->getFunction("x86_assertion_failure")) {
    errorCount +=
        forEachCall(assertionFailure, 0, [](const std::string& message) {
          std::cerr << "translation assertion failure: " << message
                    << std::endl;
        });
  }

  if (errorCount > 0) {
    std::cerr << "incorrect or missing translations; cannot decompile"
              << std::endl;
    return 1;
  }

  if (FLAGS_optimize) {
    if (!mainObj.optimizeAndTransformModule(*module, llvm::errs(),
                                            executable.get())) {
      return 1;
    }
  }

  if (FLAGS_module_out) {
    module->print(llvm::outs(), nullptr);
    google::ShutDownCommandLineFlags();
    google::ShutdownGoogleLogging();
    return 0;
  }

  // step three (final step): emit pseudocode
  if (!mainObj.generateEquivalentPseudocode(*module, llvm::outs())) {
    return 1;
  }

  google::ShutDownCommandLineFlags();
  google::ShutdownGoogleLogging();

  return 0;
}
