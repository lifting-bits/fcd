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

#include "fcd/codegen/translation_context_remill.h"

using namespace llvm;
using namespace std;

#ifdef FCD_DEBUG
[[gnu::used]] raw_ostream& llvm_errs() { return errs(); }
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
vector<T> parseListFlag(string flag, char sep) {
  vector<T> res;
  istringstream f(flag);
  string s;
  while (getline(f, s, sep)) {
    T tmp;
    istringstream(s) >> std::hex >> tmp >> std::dec;
    res.push_back(tmp);
  }
  return res;
}

vector<string> headerSearchPath;
vector<string> headers;
vector<string> frameworks;
vector<string> customPassPipeline;
vector<string> additionalPasses;
vector<uint64_t> additionalEntryPoints;

template <typename T>
string errorOf(const ErrorOr<T>& error) {
  return error.getError().message();
}

template <typename TAction>
size_t forEachCall(Function* callee, unsigned stringArgumentIndex,
                   TAction&& action) {
  size_t count = 0;
  for (Use& use : callee->uses()) {
    if (auto call = dyn_cast<CallInst>(use.getUser())) {
      unique_ptr<Instruction> eraseIfNecessary;
      Value* operand = call->getOperand(stringArgumentIndex);
      if (auto constant = dyn_cast<ConstantExpr>(operand)) {
        eraseIfNecessary.reset(constant->getAsInstruction());
        operand = eraseIfNecessary.get();
      }

      if (auto gep = dyn_cast<GetElementPtrInst>(operand))
        if (auto global = dyn_cast<GlobalVariable>(gep->getOperand(0)))
          if (auto dataArray =
                  dyn_cast<ConstantDataArray>(global->getInitializer())) {
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

  LLVMContext llvm;
  PythonContext python;
  vector<Pass*> optimizeAndTransformPasses;

  static void aliasAnalysisHooks(Pass& pass, Function& fn, AAResults& aar) {
    if (auto prgmem =
            pass.getAnalysisIfAvailable<fcd::AddressSpaceAAWrapperPass>()) {
      aar.addAAResult(prgmem->getResult());
    }
    if (auto params = pass.getAnalysisIfAvailable<ParameterRegistry>()) {
      aar.addAAResult(params->getAAResult());
    }
  }

  vector<Pass*> createPassesFromList(const vector<string>& passNames) {
    vector<Pass*> result;
    PassRegistry* pr = PassRegistry::getPassRegistry();
    for (string passName : passNames) {
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
        auto ext = sys::path::extension(passName);
        if (ext == ".py" || ext == ".pyc" || ext == ".pyo") {
          if (auto passOrError = python.createPass(passName)) {
            result.push_back(passOrError.get());
          } else {
            cerr << getProgramName() << ": couldn't load " << passName << ": "
                 << errorOf(passOrError) << endl;
            return vector<Pass*>();
          }
        } else if (const PassInfo* pi = pr->getPassInfo(passName)) {
          result.push_back(pi->createPass());
        } else {
          cerr << getProgramName() << ": couldn't identify pass " << passName
               << endl;
          return vector<Pass*>();
        }
      }
    }

    if (result.size() == 0) {
      errs() << getProgramName() << ": empty pass list\n";
    }
    return result;
  }

  vector<Pass*> interactivelyEditPassPipeline(
      const string& editor, const vector<string>& basePasses) {
    int fd;
    SmallVector<char, 100> path;
    if (auto errorCode = sys::fs::createTemporaryFile("fcd-pass-pipeline",
                                                      "txt", fd, path)) {
      errs() << getProgramName() << ": can't open temporary file for editing: "
             << errorCode.message() << "\n";
      return vector<Pass*>();
    }

    raw_fd_ostream passListOs(fd, true);
    passListOs << "# Enter the name of the LLVM or fcd passes that you want to "
                  "run on the module.\n";
    passListOs << "# Files starting with a # symbol are ignored.\n";
    passListOs << "# Names ending with .py are assumed to be Python scripts "
                  "implementing passes.\n";
    for (const string& passName : basePasses) {
      passListOs << passName << '\n';
    }
    passListOs.flush();

    // shell escape temporary path
    string escapedPath;
    escapedPath.reserve(path.size());
    for (char c : path) {
      if (c == '\'' || c == '\\') {
        escapedPath.push_back('\\');
      }
      escapedPath.push_back(c);
    }

    string editCommand;
    raw_string_ostream(editCommand) << editor << " '" << escapedPath << "'";
    if (int errorCode = system(editCommand.c_str())) {
      errs() << getProgramName()
             << ": interactive pass pipeline: editor returned status code "
             << errorCode << '\n';
      return vector<Pass*>();
    }

    ifstream passListIs(path.data());
    assert(static_cast<bool>(passListIs));

    string inputLine;
    vector<string> lines;
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

  string getProgramName() { return sys::path::stem(argv[0]); }
  LLVMContext& getContext() { return llvm; }

  ErrorOr<unique_ptr<Executable>> parseExecutable(
      MemoryBuffer& executableCode) {
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
                                             frameworks, errs());

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

  bool optimizeAndTransformModule(Module& module, raw_ostream& errorOutput,
                                  Executable* executable = nullptr) {
    PrettyStackTraceString optimize("Optimizing LLVM IR");

    // Phase 3: make into functions with arguments, run codegen.
    legacy::PassManager pm;
    pm.add(llvm::createTypeBasedAAWrapperPass());
    pm.add(llvm::createScopedNoAliasAAWrapperPass());
    pm.add(llvm::createBasicAAWrapperPass());
    pm.add(fcd::createAddressSpaceAliasAnalysis());
    pm.add(new ExecutableWrapper(executable));
    pm.add(createParameterRegistryPass());
    pm.add(llvm::createExternalAAWrapperPass(&Main::aliasAnalysisHooks));
    for (Pass* pass : optimizeAndTransformPasses) {
      pm.add(pass);
    }
    pm.run(module);

#ifdef FCD_DEBUG
    if (llvm::verifyModule(module, &errorOutput)) {
      // errors!
      return false;
    }
#endif
    return true;
  }

  bool generateEquivalentPseudocode(Module& module, raw_ostream& output) {
    PrettyStackTraceString pseudocode("Generating pseudo-C output");

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
    auto& pr = *PassRegistry::getPassRegistry();
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
    vector<string> passNames = {
        "globaldce",
        "fixindirects", // fcd
        "argrec", // fcd
        "sroa",
        "intnarrowing", // fcd
        "signext", // fcd
        "instcombine",
        "intops", // fcd
        "simplifyconditions", // fcd
        // <-- custom passes go here with the default pass pipeline
        "instcombine",
        "gvn",
        "simplifycfg",
        "instcombine",
        "gvn",
        "recoverstackframe", // fcd
        "dse",
        "sccp",
        "simplifycfg",
        "eliminatecasts", // fcd
        "instcombine",
        "memssadle", // fcd
        "dse",
        "instcombine",
        "sroa",
        "instcombine",
        "globaldce",
        "simplifycfg",
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
        errs() << getProgramName()
               << ": environment has no EDITOR variable; "
                  "pass pipeline can't be edited "
                  "interactively\n";
        return false;
      }
    } else {
      optimizeAndTransformPasses = createPassesFromList(customPassPipeline);
      if (optimizeAndTransformPasses.size() == 0) {
        errs() << getProgramName() << ": empty custom pass list\n";
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
  stringstream ss("");

  EnablePrettyStackTrace();
  sys::PrintStackTraceOnErrorSignal(argv[0]);

  google::InitGoogleLogging(argv[0]);
  google::SetUsageMessage(ss.str());
  google::ParseCommandLineFlags(&argc, &argv, true);

  headerSearchPath = parseListFlag<string>(FLAGS_includes, ',');
  headers = parseListFlag<string>(FLAGS_headers, ',');
  frameworks = parseListFlag<string>(FLAGS_frameworks, ',');
  customPassPipeline = parseListFlag<string>(FLAGS_pipeline, ',');
  additionalPasses = parseListFlag<string>(FLAGS_opt, ',');
  additionalEntryPoints = parseListFlag<uint64_t>(FLAGS_other_entry, ',');

  CHECK(FLAGS_pipeline == "default" || FLAGS_opt.empty())
      << "Inserting LLVM IR passes only allowed when using default pipeline.";

  CHECK(!FLAGS_os.empty()) << "Must specify an operating system name to --os.";

  CHECK(!FLAGS_arch.empty())
      << "Must specify a machine code architecture name to --arch.";

  CHECK(argc > 1) << "Must specify and input file.";

  string inputFile = argv[1];

  Main::initializePasses();

  Main mainObj(argc, argv);
  string program = mainObj.getProgramName();

  // step 0: before even attempting anything, prepare optimization passes
  // (the user won't be happy if we work for 5 minutes only to discover that the
  // optimization passes don't load)
  if (!mainObj.prepareOptimizationPasses()) {
    return 1;
  }

  unique_ptr<Executable> executable;
  unique_ptr<Module> module;

  // step one: create annotated module from executable (or load it from .ll)
  ErrorOr<unique_ptr<MemoryBuffer>> bufferOrError(nullptr);
  if (FLAGS_module_in) {
    PrettyStackTraceFormat parsingIR("Parsing IR from \"%s\"",
                                     inputFile.c_str());

    SMDiagnostic errors;
    module = parseIRFile(inputFile, errors, mainObj.getContext());
    if (!module) {
      errors.print(argv[0], errs());
      return 1;
    }
  } else {
    PrettyStackTraceFormat parsingIR("Parsing executable \"%s\"",
                                     inputFile.c_str());

    bufferOrError = MemoryBuffer::getFile(inputFile, -1, false);
    if (!bufferOrError) {
      cerr << program << ": can't open " << inputFile << ": "
           << errorOf(bufferOrError) << endl;
      return 1;
    }

    auto executableOrError = mainObj.parseExecutable(*bufferOrError.get());
    if (!executableOrError) {
      cerr << program << ": couldn't parse " << inputFile << ": "
           << errorOf(executableOrError) << endl;
      return 1;
    }

    executable = move(executableOrError.get());
    module = mainObj.generateAnnotatedModule(*executable);
  }

  // Make sure that the module is legal
  size_t errorCount = 0;
  if (Function* assertionFailure =
          module->getFunction("x86_assertion_failure")) {
    errorCount += forEachCall(assertionFailure, 0, [](const string& message) {
      cerr << "translation assertion failure: " << message << endl;
    });
  }

  if (errorCount > 0) {
    cerr << "incorrect or missing translations; cannot decompile" << endl;
    return 1;
  }

  if (FLAGS_optimize) {
    if (!mainObj.optimizeAndTransformModule(*module, errs(),
                                            executable.get())) {
      return 1;
    }
  }

  if (FLAGS_module_out) {
    module->print(outs(), nullptr);
    google::ShutDownCommandLineFlags();
    google::ShutdownGoogleLogging();
    return 0;
  }

  // step three (final step): emit pseudocode
  if (!mainObj.generateEquivalentPseudocode(*module, outs())) {
    return 1;
  }

  google::ShutDownCommandLineFlags();
  google::ShutdownGoogleLogging();

  return 0;
}
