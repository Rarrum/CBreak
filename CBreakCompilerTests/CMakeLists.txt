cmake_minimum_required(VERSION 3.10)

project(CBreakCompilerTests)

include(../Common.cmake)

#Set up llvm
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

#Tests
find_package(CxxTest)

if(CXXTEST_FOUND)
    include_directories(${CXXTEST_INCLUDE_DIR})
    enable_testing()

    CXXTEST_ADD_TEST(CBreakCompilerTests GeneratedTests.cc
        ${CMAKE_CURRENT_SOURCE_DIR}/src/SourceTokenizerTests.h
        ${CMAKE_CURRENT_SOURCE_DIR}/src/CompilerTests.h
        ${CMAKE_CURRENT_SOURCE_DIR}/src/ModuleTests.h)

    llvm_map_components_to_libnames(llvm_libs core executionengine support nativecodegen orcjit)
    target_link_libraries(CBreakCompilerTests CBreakLibrary ${llvm_libs})
endif()

