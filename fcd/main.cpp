//
// main.cpp
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#include "main.h"
#include "ast_passes.h"
#include "command_line.h"
#include "errors.h"
#include "executable.h"
#include "header_decls.h"
#include "metadata.h"
#include "params_registry.h"
#include "passes.h"
#include "python_context.h"
#include "translation_context.h"

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
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#ifdef FCD_DEBUG
[[gnu::used]] llvm::raw_ostream& llvm_errs() { return llvm::errs(); }
#endif

DEFINE_bool(partial, false,
            "Only decompile functions specified with -other-entry");
DEFINE_bool(exclusive, false, "More restrictive version of -partial");
DEFINE_bool(module_in, false, "Input file is a LLVM module");
DEFINE_bool(module_out, false, "Output LLVM module");
DEFINE_bool(optimize, true, "Optimize ");
DEFINE_string(other_entry, "", "Add entry points from virtual addresses");
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
    std::istringstream(s) >> tmp;
    res.push_back(tmp);
  }
  return res;
}

auto headerSearchPath = parseListFlag<std::string>(FLAGS_includes, ',');
auto headers = parseListFlag<std::string>(FLAGS_headers, ',');
auto frameworks = parseListFlag<std::string>(FLAGS_frameworks, ',');
auto customPassPipeline = parseListFlag<std::string>(FLAGS_pipeline, ',');
auto additionalPasses = parseListFlag<std::string>(FLAGS_opt, ',');
auto additionalEntryPoints = parseListFlag<unsigned long long>(FLAGS_other_entry, ',');

// cl::opt<std::string> inputFile(cl::Positional, cl::desc("<input program>"),
//                           cl::Required, whitelist());
// cl::list<unsigned long long> additionalEntryPoints(
//     "other-entry",
//     cl::desc(
//         "Add entry point from virtual address (can be used multiple times)"),
//     cl::CommaSeparated, whitelist());
// cl::list<bool> partialDisassembly(
//     "partial",
//     cl::desc("Only decompile functions specified with --other-entry"),
//     whitelist());
// cl::list<bool> inputIsModule("module-in",
//                              cl::desc("Input file is a LLVM module"),
//                              whitelist());
// cl::list<bool> outputIsModule("module-out", cl::desc("Output LLVM module"),
//                               whitelist());

// cl::list<std::string> additionalPasses(
//     "opt",
//     cl::desc("Insert LLVM optimization pass; a pass name ending in .py is "
//              "interpreted as a Python script. Requires default pass
//              pipeline."),
//     whitelist());
// cl::opt<std::string> customPassPipeline(
//     "opt-pipeline", cl::desc("Customize pass pipeline. Empty string lets you
//     "
//                              "order passes through $EDITOR; otherwise, must
//                              be "
//                              "a whitespace-separated list of passes."),
//     cl::init("default"), whitelist());
// cl::list<std::string> headers(
//     "header", cl::desc("Path of a header file to parse for function "
//                        "declarations. Can be specified multiple times"),
//     whitelist());
// cl::list<std::string> frameworks(
//     "framework", cl::desc("Path of an Apple framework that fcd should use for
//     "
//                           "declarations. Can be specified multiple times"),
//     whitelist());
// cl::list<std::string> headerSearchPath(
//     "I", cl::desc("Additional directory to search headers in. Can be
//     specified "
//                   "multiple times"),
//     whitelist());

// template <int (*)()>  // templated to ensure multiple instatiation of the
// static
//                       // variables
//                       inline int optCount(const cl::list<bool>& list) {
//   static int count = 0;
//   static bool counted = false;
//   if (!counted) {
//     for (bool opt : list) {
//       count += opt ? 1 : -1;
//     }
//     counted = true;
//   }
//   return count;
// }

// inline int partialOptCount() {
//   return optCount<partialOptCount>(partialDisassembly);
// }

// inline int moduleInCount() { return optCount<moduleInCount>(inputIsModule); }

// inline int moduleOutCount() { return
// optCount<moduleOutCount>(outputIsModule); }

// void pruneOptionList(StringMap<cl::Option*>& list) {
//   for (auto& pair : list) {
//     if (!whitelist::isWhitelisted(*pair.second)) {
//       pair.second->setHiddenFlag(cl::ReallyHidden);
//     }
//   }
// }

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

bool refillEntryPoints(const TranslationContext& transl,
                       const EntryPointRepository& entryPoints,
                       std::map<uint64_t, SymbolInfo>& toVisit,
                       size_t iterations) {
  if (isExclusiveDisassembly() || (isPartialDisassembly() && iterations > 1)) {
    return false;
  }

  for (uint64_t entryPoint : transl.getDiscoveredEntryPoints()) {
    if (auto symbolInfo = entryPoints.getInfo(entryPoint)) {
      toVisit.insert({entryPoint, *symbolInfo});
    }
  }
  return !toVisit.empty();
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
            pass.getAnalysisIfAvailable<ProgramMemoryAAWrapperPass>()) {
      aar.addAAResult(prgmem->getResult());
    }
    if (auto params = pass.getAnalysisIfAvailable<ParameterRegistry>()) {
      aar.addAAResult(params->getAAResult());
    }
  }

  static llvm::legacy::PassManager createBasePassManager() {
    llvm::legacy::PassManager pm;
    pm.add(llvm::createTypeBasedAAWrapperPass());
    pm.add(llvm::createScopedNoAliasAAWrapperPass());
    pm.add(llvm::createBasicAAWrapperPass());
    pm.add(createProgramMemoryAliasAnalysis());
    return pm;
  }

  std::vector<llvm::Pass*> createPassesFromList(
      const std::vector<std::string>& passNames) {
    std::vector<llvm::Pass*> result;
    llvm::PassRegistry* pr = llvm::PassRegistry::getPassRegistry();
    for (std::string passName : passNames) {
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

  // std::vector<llvm::Pass*> readPassPipelineFromString(const std::string&
  // argString) {
  //   std::stringstream ss(argString, ios::in);
  //   std::vector<std::string> passes;
  //   while (ss) {
  //     passes.emplace_back();
  //     std::string& passName = passes.back();
  //     ss >> passName;
  //     if (passName.size() == 0 || passName[0] == '#') {
  //       passes.pop_back();
  //     }
  //   }
  //   auto result = createPassesFromList(passes);
  //   if (result.size() == 0) {
  //     llvm::errs() << getProgramName() << ": empty custom pass list\n";
  //   }
  //   return result;
  // }

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

  llvm::ErrorOr<std::unique_ptr<llvm::Module>> generateAnnotatedModule(
      Executable& executable, const std::string& moduleName = "fcd-out") {
    x86_config config64 = {x86_isa64, 8, X86_REG_RIP, X86_REG_RSP, X86_REG_RBP};
    TranslationContext transl(llvm, executable, config64, moduleName);

    // Load headers here, since this is the earliest point where we have an
    // executable and a module.
    auto cDecls = HeaderDeclarations::create(
        transl.get(), headerSearchPath.begin(), headerSearchPath.end(),
        headers.begin(), headers.end(), frameworks.begin(), frameworks.end(),
        llvm::errs());
    if (!cDecls) {
      return make_error_code(FcdError::Main_HeaderParsingError);
    }

    EntryPointRepository entryPoints;
    entryPoints.addProvider(executable);
    entryPoints.addProvider(*cDecls);

    md::addIncludedFiles(transl.get(), cDecls->getIncludedFiles());

    std::map<uint64_t, SymbolInfo> toVisit;
    if (isFullDisassembly()) {
      for (uint64_t address : entryPoints.getVisibleEntryPoints()) {
        auto symbolInfo = entryPoints.getInfo(address);
        assert(symbolInfo != nullptr);
        toVisit.insert({symbolInfo->virtualAddress, *symbolInfo});
      }
    }

    for (uint64_t address : std::unordered_set<uint64_t>(
             additionalEntryPoints.begin(), additionalEntryPoints.end())) {
      if (auto symbolInfo = entryPoints.getInfo(address)) {
        toVisit.insert({symbolInfo->virtualAddress, *symbolInfo});
      } else {
        return make_error_code(FcdError::Main_EntryPointOutOfMappedMemory);
      }
    }

    if (toVisit.size() == 0) {
      return make_error_code(FcdError::Main_NoEntryPoint);
    }

    size_t iterations = 0;
    do {
      while (toVisit.size() > 0) {
        auto iter = toVisit.begin();
        auto functionInfo = iter->second;
        toVisit.erase(iter);

        if (functionInfo.name.size() > 0) {
          transl.setFunctionName(functionInfo.virtualAddress,
                                 functionInfo.name);
        }

        if (llvm::Function* fn =
                transl.createFunction(functionInfo.virtualAddress)) {
          if (llvm::Function* cFunction =
                  cDecls->prototypeForAddress(functionInfo.virtualAddress)) {
            md::setFinalPrototype(*fn, *cFunction);
          }
        } else {
          // Couldn't decompile, abort
          return make_error_code(FcdError::Main_DecompilationError);
        }
      }
      iterations++;
    } while (refillEntryPoints(transl, entryPoints, toVisit, iterations));

    // Perform early optimizations to make the module suitable for analysis
    auto module = transl.take();
    llvm::legacy::PassManager phaseOne = createBasePassManager();
    phaseOne.add(llvm::createExternalAAWrapperPass(&Main::aliasAnalysisHooks));
    phaseOne.add(llvm::createDeadCodeEliminationPass());
    phaseOne.add(llvm::createInstructionCombiningPass());
    phaseOne.add(createRegisterPointerPromotionPass());
    phaseOne.add(llvm::createGVNPass());
    phaseOne.add(llvm::createDeadStoreEliminationPass());
    phaseOne.add(llvm::createInstructionCombiningPass());
    phaseOne.add(llvm::createGlobalDCEPass());
    phaseOne.run(*module);

    // Annotate stubs before returning module
    llvm::Function* jumpIntrin = module->getFunction("x86_jump_intrin");
    std::vector<llvm::Function*> functions;
    for (llvm::Function& fn : module->getFunctionList()) {
      if (md::isPrototype(fn)) {
        continue;
      }

      llvm::BasicBlock& entry = fn.getEntryBlock();
      auto terminator = entry.getTerminator();
      if (llvm::isa<llvm::UnreachableInst>(terminator)) {
        if (auto prev =
                llvm::dyn_cast<llvm::CallInst>(terminator->getPrevNode()))
          if (prev->getCalledFunction() == jumpIntrin)
            if (auto load = llvm::dyn_cast<llvm::LoadInst>(prev->getOperand(2)))
              if (auto constantExpr = llvm::dyn_cast<llvm::ConstantExpr>(
                      load->getPointerOperand())) {
                std::unique_ptr<llvm::Instruction> inst(
                    constantExpr->getAsInstruction());
                if (auto int2ptr =
                        llvm::dyn_cast<llvm::IntToPtrInst>(inst.get())) {
                  auto value =
                      llvm::cast<llvm::ConstantInt>(int2ptr->getOperand(0));
                  if (const StubInfo* stubTarget =
                          executable.getStubTarget(value->getLimitedValue())) {
                    if (llvm::Function* cFunction =
                            cDecls->prototypeForImportName(stubTarget->name)) {
                      md::setIsStub(fn);
                      md::setFinalPrototype(fn, *cFunction);
                    }

                    // If we identified no function from the header file, this
                    // gives the import its real
                    // name. Otherwise, it'll prefix the name with some number.
                    fn.setName(stubTarget->name);
                  }
                }
              }
      }
    }
    return move(module);
  }

  bool optimizeAndTransformModule(llvm::Module& module,
                                  llvm::raw_ostream& errorOutput,
                                  Executable* executable = nullptr) {
    llvm::PrettyStackTraceString optimize("Optimizing LLVM IR");

    // Phase 3: make into functions with arguments, run codegen.
    auto passManager = createBasePassManager();
    passManager.add(new ExecutableWrapper(executable));
    passManager.add(createParameterRegistryPass());
    passManager.add(createExternalAAWrapperPass(&Main::aliasAnalysisHooks));
    for (llvm::Pass* pass : optimizeAndTransformPasses) {
      passManager.add(pass);
    }
    passManager.run(module);

#ifdef FCD_DEBUG
    if (verifyModule(module, &errorOutput)) {
      // errors!
      return false;
    }
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
        "globaldce", "fixindirects", "argrec", "sroa", "intnarrowing",
        "signext", "instcombine", "intops", "simplifyconditions",
        // <-- custom passes go here with the default pass pipeline
        "instcombine", "gvn", "simplifycfg", "instcombine", "gvn",
        "recoverstackframe", "dse", "sccp", "simplifycfg", "eliminatecasts",
        "instcombine", "memssadle", "dse", "instcombine", "sroa", "instcombine",
        "globaldce", "simplifycfg",
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
};

bool isFullDisassembly() { return !FLAGS_partial && !FLAGS_exclusive; }

bool isPartialDisassembly() { return FLAGS_partial && !FLAGS_exclusive; }

bool isExclusiveDisassembly() { return FLAGS_exclusive; }

bool isEntryPoint(uint64_t vaddr) {
  return any_of(additionalEntryPoints.begin(), additionalEntryPoints.end(),
                [&](uint64_t entryPoint) { return vaddr == entryPoint; });
}

int main(int argc, char** argv) {
  std::stringstream ss("");
  google::InitGoogleLogging(argv[0]);
  google::SetUsageMessage(ss.str());
  google::ParseCommandLineFlags(&argc, &argv, true);

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
    std::string moduleName = llvm::sys::path::stem(inputFile);
    auto moduleOrError =
        mainObj.generateAnnotatedModule(*executable, moduleName);
    if (!moduleOrError) {
      std::cerr << program << ": couldn't build LLVM module out of "
                << inputFile << ": " << errorOf(moduleOrError) << std::endl;
      return 1;
    }

    module = move(moduleOrError.get());
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
