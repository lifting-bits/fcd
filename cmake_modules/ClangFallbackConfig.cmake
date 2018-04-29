cmake_minimum_required(VERSION 3.9)

function(FindClang)
  if(DEFINED ENV{TRAILOFBITS_LIBRARIES})
    set(LLVM_LIBRARY_ROOT "$ENV{TRAILOFBITS_LIBRARIES}/llvm")
  endif()
  if(NOT "${LLVM_LIBRARY_ROOT}" STREQUAL "")
    set(llvm_library_location_hint "${LLVM_LIBRARY_ROOT}/lib")
    set(llvm_include_location_hint "${LLVM_LIBRARY_ROOT}/include")
  endif()
  find_library(clang_library_location
    NAMES "libclang.so"
    HINTS "/usr/local/lib" "/usr/lib" "${llvm_library_location_hint}"
  )
  find_path(clang_include_location
    NAMES "llvm/Config/llvm-config.h"
    HINTS "/usr/local/include" "/usr/include" "${llvm_include_location_hint}"
  )
  if("${clang_library_location}" STREQUAL "clang_library_location-NOTFOUND" OR
     "${clang_include_location}" STREQUAL "clang_include_location-NOTFOUND")
    message(SEND_ERROR "The Clang library could not be found")
  else()
    message(STATUS "Clang library found: ${clang_library_location}, ${clang_include_location}")
    add_library("clang" STATIC IMPORTED)
    set_property(TARGET "clang" PROPERTY IMPORTED_LOCATION "${clang_library_location}")
    set_property(TARGET "clang" PROPERTY INTERFACE_INCLUDE_DIRECTORIES "${clang_include_location}")
  endif()
endfunction()

if(NOT TARGET "clang")
  FindClang()
endif()