cmake_minimum_required(VERSION 3.10)

project(CBreakCompiler)

include(../Common.cmake)

#Set up llvm
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

#Compile CBreak Compiler Library
add_library(CBreakLibrary
    src/Parser.cpp
    src/SourceTokenizer.cpp
    src/ModuleCompiler.cpp
    src/int/ModuleCompilerInternal.cpp
    src/int/ModuleConfiguration.cpp
    src/Language.cpp
    src/int/EvaluationContext.cpp
    src/int/TypeSystem.cpp
    src/CompiledOutput.cpp
    src/JIT.cpp)

target_include_directories(CBreakLibrary PUBLIC src)

#Compile CBreak Compiler Executable
add_executable(CBreakCompiler
    src/main.cpp)

llvm_map_components_to_libnames(llvm_libs core executionengine support nativecodegen orcjit)

target_link_libraries(CBreakCompiler
    CBreakLibrary
    ${llvm_libs})

