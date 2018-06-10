//
// executable.cpp
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#include "command_line.h"
#include "executable.h"
#include "executable_errors.h"
#include "elf_executable.h"
#include "flat_binary.h"
#include "python_executable.h"

#include "remill/BC/Version.h"

#include <ctype.h>
#include <gflags/gflags.h>

using namespace llvm;
using namespace std;

namespace
{
	// http://stackoverflow.com/a/2886589/251153
	// "all you have to do"... understatement of the week!
	struct ci_char_traits : public char_traits<char>
	{
		static bool eq(char c1, char c2) { return toupper(c1) == toupper(c2); }
		static bool ne(char c1, char c2) { return toupper(c1) != toupper(c2); }
		static bool lt(char c1, char c2) { return toupper(c1) <  toupper(c2); }
		
		static int compare(const char* s1, const char* s2, size_t n)
		{
			while (n != 0)
			{
				--n;
				if (toupper(*s1) < toupper(*s2))
				{
					return -1;
				}
				if (toupper(*s1) > toupper(*s2))
				{
					return 1;
				}
				++s1;
				++s2;
			}
			return 0;
		}
		
		static const char* find(const char* s, int n, char a)
		{
			while (n > 0 && !eq(*s, a))
			{
				--n;
				++s;
			}
			return s;
		}
	};
	
	typedef basic_string<char, ci_char_traits> ci_string;
	
	class AutoExecutableFactory : public ExecutableFactory
	{
		static const char elf_magic[4];
		
	public:
		AutoExecutableFactory()
		: ExecutableFactory("auto", "autodetect")
		{
		}
		
		virtual llvm::ErrorOr<std::unique_ptr<Executable>> parse(const uint8_t* begin, const uint8_t* end) override
		{
			assert(end >= begin);
			uintptr_t size = static_cast<uintptr_t>(end - begin);
			if (size >= sizeof elf_magic && memcmp(begin, elf_magic, sizeof elf_magic) == 0)
			{
				return ElfExecutableFactory().parse(begin, end);
			}
			
			return make_error_code(ExecutableParsingError::Generic_UnknownFormat);
		}
	};
	
	const char AutoExecutableFactory::elf_magic[4] = {0x7f, 'E', 'L', 'F'};
	
	AutoExecutableFactory autoFactory;
	ElfExecutableFactory elfFactory;
	FlatBinaryExecutableFactory flatBinaryFactory;
	PythonExecutableFactory pythonScriptExecutableFactory;
    ExecutableFactory* const factories[] = {
        &autoFactory,
        &elfFactory,
        &flatBinaryFactory,
        &pythonScriptExecutableFactory,
    };
    
    ExecutableFactory* getFactory(llvm::StringRef formatStr) {
        if (formatStr.endswith(".py")) {
            // Yikes! Mutable global state
            pythonScriptExecutableFactory.setScriptPath(formatStr);
            return &pythonScriptExecutableFactory;
        }
        for (auto f: factories) {
            if (formatStr == f->getParameterValue()) {
                return f;
            }
        }
        return nullptr;
    }
    
    bool validateFormat(char const *, std::string const & formatStr) {
        return getFactory(formatStr) != nullptr;
    }

    DEFINE_string(format, "auto", "Executable format");
    DEFINE_validator(format, &validateFormat);
}

string Executable::getTargetTriple() const
{
	string triple = doGetTargetTriple();
	auto firstDash = triple.find('-');
	if (firstDash != string::npos)
	{
		auto secondDash = triple.find('-', firstDash + 1);
		if (secondDash != string::npos)
		{
			return triple;
		}
	}
	assert(false);
	return "unknown-unknown-unknown";
}

vector<uint64_t> Executable::getVisibleEntryPoints() const
{
	vector<uint64_t> result;
	for (const auto& pair : symbols)
	{
		result.push_back(pair.second.virtualAddress);
	}
	sort(result.begin(), result.end());
	return result;
}

const SymbolInfo* Executable::getInfo(uint64_t address) const
{
	auto iter = symbols.find(address);
	if (iter != symbols.end())
	{
		return &iter->second;
	}
	else if (map(address) != nullptr)
	{
		SymbolInfo& info = symbols[address];
		info.virtualAddress = address;
		return &info;
	}
	return nullptr;
}

const StubInfo* Executable::getStubTarget(uint64_t address) const
{
	auto iter = stubTargets.find(address);
	if (iter != stubTargets.end())
	{
		return &iter->second;
	}
	
	string libraryName;
	string targetName;
	switch (doGetStubTarget(address, libraryName, targetName))
	{
		case ResolvedInFlatNamespace:
		{
			StubInfo& stub = stubTargets[address];
			stub.sharedObject = nullptr;
			stub.name = move(targetName);
			return &stub;
		}
		case ResolvedInTwoLevelNamespace:
		{
			auto libIter = libraries.insert(libraryName).first;
			StubInfo& stub = stubTargets[address];
			stub.sharedObject = &*libIter;
			stub.name = move(targetName);
			return &stub;
		}
		case Unresolved:
			return nullptr;
		default:
			llvm_unreachable("Unknown stub target resolution type!");
	}
}

ErrorOr<unique_ptr<Executable>> Executable::parse(const uint8_t* begin, const uint8_t* end)
{
	return getFactory(FLAGS_format)->parse(begin, end);
}
