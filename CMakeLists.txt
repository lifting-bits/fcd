# Copyright (c) 2017 Trail of Bits, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

project(fcd)
cmake_minimum_required (VERSION 3.2)

set(FCD_SOURCE_DIR "${PROJECT_SOURCE_DIR}")

# add fallback clang detection 
list(APPEND CMAKE_PREFIX_PATH "${PROJECT_SOURCE_DIR}/cmake")

# C++ standard
# todo(msurovic): remove this once AST code is refactored
set(CMAKE_CXX_STANDARD 14)

# warnings and compiler settings
# todo(msurovic): enable once AST code is refactored
# set(PROJECT_CXXWARNINGS "-Wall -Wextra -Werror -Wconversion -pedantic -Wno-unused-parameter -Wno-c++98-compat -Wno-unreachable-code-return -Wno-nested-anon-types -Wno-extended-offsetof -Wno-gnu-anonymous-struct -Wno-gnu-designator -Wno-variadic-macros -Wno-gnu-zero-variadic-macro-arguments -Wno-gnu-statement-expression -Wno-return-type-c-linkage -Wno-c99-extensions -Wno-ignored-attributes -Wno-unused-local-typedef")

set(PROJECT_CXXFLAGS "${PROJECT_CXXFLAGS} -Wno-unknown-warning-option ${PROJECT_CXXWARNINGS} -fPIC -fno-omit-frame-pointer -fvisibility-inlines-hidden -fno-exceptions -fno-asynchronous-unwind-tables -fno-rtti")

#
# clang libs
#
find_package(Clang)
if(NOT ${Clang_FOUND})
  find_package(ClangFallback REQUIRED)
endif()

set(CLANG_LIBS libclang clangIndex clangCodeGen)
list(APPEND PROJECT_LIBRARIES ${CLANG_LIBS})

# hack: rpath magic for libclang.so
# todo(msurovic): remove this if libClang is replaced by libTooling
set(CMAKE_SKIP_BUILD_RPATH false)
set(CMAKE_BUILD_WITH_INSTALL_RPATH true) 
set(CMAKE_INSTALL_RPATH "${LIBRARY_REPOSITORY_ROOT}/llvm/lib")

#
# python libs
#
find_package(PythonLibs 2.7 REQUIRED)
list(APPEND PROJECT_INCLUDEDIRECTORIES ${PYTHON_INCLUDE_PATH})
list(APPEND PROJECT_LIBRARIES ${PYTHON_LIBRARIES})

#
# target settings
#
set(FCD fcd-${REMILL_LLVM_VERSION})

#
# old fcd stuff
#
# todo(msurovic): remove this once python bindings and header parsing are refactored
set(subdirs fcd/ fcd/ast fcd/ast_remill fcd/callconv fcd/clang fcd/codegen fcd/executables fcd/symbols)
set(pythonbindingsfile "${CMAKE_CURRENT_BINARY_DIR}/bindings.cpp")
set(subdirs ${subdirs} fcd/python)
list(APPEND PROJECT_INCLUDEDIRECTORIES ${subdirs})
add_custom_command(OUTPUT ${pythonbindingsfile} COMMAND "${CMAKE_C_COMPILER}" -E ${LLVM_DEFINITIONS} -isystem ${LLVM_INCLUDE_DIRS} "${LLVM_INCLUDE_DIRS}/llvm-c/Core.h" | python "${PROJECT_SOURCE_DIR}/fcd/python/bindings.py" > ${pythonbindingsfile})
execute_process(COMMAND "${CMAKE_CXX_COMPILER}" -E -x c++ -v /dev/null OUTPUT_VARIABLE dummy ERROR_VARIABLE defaultHeaderSearchPathList)
string(REGEX REPLACE ".*#include <...> search starts here:\n(.+)\nEnd of search list.*" "\\1" defaultHeaderSearchPathList "${defaultHeaderSearchPathList}")
string(REGEX REPLACE " (/[^\n]+)" "\"\\1\"," defaultHeaderSearchPathList "${defaultHeaderSearchPathList}")
set(defaultFrameworkSearchPathList)
configure_file(fcd/systemIncludePath.cpp.tmpl "${CMAKE_CURRENT_BINARY_DIR}/systemIncludePath.cpp")
file(GLOB_RECURSE sources fcd/*.cpp)
list(APPEND sources ${pythonbindingsfile})
list(APPEND sources "${CMAKE_CURRENT_BINARY_DIR}/systemIncludePath.cpp")

#
# add fcd target
#
# todo(msurovic): enable once AST code is refactored
add_executable(${FCD} ${sources}
    # mcsema/Arch/ABI.cpp
    # mcsema/Arch/Arch.cpp

    # mcsema/CFG/CFG.cpp

    # mcsema/BC/Callback.cpp
    # mcsema/BC/External.cpp
    # mcsema/BC/Function.cpp
    # mcsema/BC/Instruction.cpp
    # mcsema/BC/Legacy.cpp
    # mcsema/BC/Lift.cpp
    # mcsema/BC/Optimize.cpp
    # mcsema/BC/Segment.cpp
    # mcsema/BC/Util.cpp

    # tools/mcsema_lift/Lift.cpp
)

# this is needed for the includes with absolutes paths to work correctly
list(APPEND PROJECT_INCLUDEDIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(${FCD} PRIVATE remill ${PROJECT_LIBRARIES})
target_include_directories(${FCD} SYSTEM PUBLIC ${PROJECT_INCLUDEDIRECTORIES})
target_compile_definitions(${FCD} PUBLIC ${PROJECT_DEFINITIONS})
set_target_properties(${FCD} PROPERTIES COMPILE_FLAGS ${PROJECT_CXXFLAGS})