// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include <cxxtest/TestSuite.h>
#include <string>
#include <iostream>

#include "Parser.h"
#include "ModuleCompiler.h"
#include "JIT.h"

using namespace CBreakCompiler;

namespace
{
    CompiledModule CompileTestModule(llvm::LLVMContext &llvmContext, const std::string &moduleCode, ModuleImporter *importer)
    {
        try
        {
            std::vector<ParsedModuleFragment> fragments;
            fragments.emplace_back(ParseSource(moduleCode, "test"));
            return GenerateModule(llvmContext, fragments, importer);
        }
        catch (const ParseFailedException& e)
        {
            std::cerr<<"At "<<e.Location.Location.Line<<":"<<e.Location.Location.Column<<": "<<e.Message<<std::endl;
            std::cerr<<"Module:\n"<<moduleCode<<std::endl;
            throw;
        }
        catch (const CodeGenerationFailedException& e)
        {
            std::cerr<<"At "<<e.Location.Location.Line<<":"<<e.Location.Location.Column<<": "<<e.Message<<std::endl;
            std::cerr<<"Module:\n"<<moduleCode<<std::endl;
            throw;
        }
    }

    int RunAndReturnCode(llvm::LLVMContext &llvmContext, const std::string &program, ModuleImporter *importer)
    {
        try
        {
            CompiledModule module = CompileTestModule(llvmContext, program, importer);
            return RunJIT(std::move(module), importer, "::main", false);
        }
        catch (const JITFailedException& e)
        {
            std::cerr<<e.Message<<std::endl;
            std::cerr<<"Program:\n"<<program<<std::endl;
            throw;
        }
    }
}

class TestModuleImporter: public ModuleImporter
{
public:
    virtual CompiledModule* LoadModule(const std::string &moduleName) override
    {
        for (CompiledModule *m : AvailableModules)
        {
            if (m->Identifier.Name == moduleName)
                return m;
        }

        return nullptr;
    }

    std::vector<CompiledModule*> AvailableModules;
};

class ModuleTests: public CxxTest::TestSuite
{
public:
    void testSimpleImportMethod()
    {
        llvm::LLVMContext llvmContext;
        TestModuleImporter importer;
        CompiledModule m1 = CompileTestModule(llvmContext, "#configuration { #ModuleName \"OtherLib\"; } #export (#int32) MulTwo (#int32 value) { #return value * 2; }", &importer);
        importer.AvailableModules.emplace_back(&m1);
        int ret = RunAndReturnCode(llvmContext, "#configuration { #Import \"OtherLib\"; } #export (#int32) main() { #return ::MulTwo(3); }", &importer);
        TS_ASSERT_EQUALS(ret, 6);
    }
};
