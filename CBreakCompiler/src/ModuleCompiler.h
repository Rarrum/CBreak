// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#pragma once

#include <vector>

#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>

#include "Parser.h"

namespace CBreakCompiler
{
    struct CodeGenerationFailedException
    {
        std::string Message;
        SourceLocation Location;
        SourceLocation OtherLocation;
    };

    const uint32_t CurrentCBreakMajorVersion = 0;
    const uint32_t CurrentCBreakMinorVersion = 1;

    struct CBreakVersionNumber
    {
        uint32_t Major = CurrentCBreakMajorVersion;
        uint32_t Minor = CurrentCBreakMinorVersion;

        inline bool operator<(const CBreakVersionNumber &other)
        {
            return Major < other.Major ? true : (Major > other.Major ? false : Minor < other.Minor);
        }
    };

    struct ModuleIdentifier
    {
        std::string Name;
        uint32_t Major = 0;
        uint32_t Minor = 0;
        uint32_t Build = 0;
        std::string Alterant;
    };

    struct CompiledModule
    {
        CBreakVersionNumber LanguageVersion;
        ModuleIdentifier Identifier;
        std::unique_ptr<llvm::Module> LLVMModule;

        std::vector<ModuleIdentifier> ImportedModules;
        std::vector<FunctionInformation> ExportedGlobalFunctions; //declarations only - code should be stripped

        //TODO: list of exported classes and their exported members
        //TODO: list of exported global variables
    };

    //used when the compiler encounters an #import for a particular module
    class ModuleImporter
    {
    public:
        inline virtual ~ModuleImporter() {}

        virtual CompiledModule* LoadModule(const std::string &moduleName) = 0;
    };

    class DefaultModuleImporter: public ModuleImporter
    {
    public:
        virtual CompiledModule* LoadModule(const std::string &moduleName) override;

        //Files in this path will be searched for a matching module, assuming the module name matches the file name.  If a module is present on multiple paths, the last one is used.
        std::vector<std::string> SearchPaths;

        //These specific files (which is assumed to match the module name) will be considered when loading modules.  This takes priority over SearchPaths.
        std::vector<std::string> ModuleFilePaths;
    };

    struct GenerationOptions
    {
        //useful when debugging compiler issues, since llvm's verifier tends to segfault on a lot of stuff that's incorrect
        bool DisableLLVMVerifier = false;
    };

    //may be thrown instead of CodeGenerationFailedException if we did successfully create the IR for the module at least
    struct CodeValidationException: public CodeGenerationFailedException
    {
        CompiledModule PartialModule;
    };

    //Combines module fragments and generates llvm ir representing the module.
    //May throw: CodeGenerationFailedException
    CompiledModule GenerateModule(llvm::LLVMContext &llvmContext, const std::vector<ParsedModuleFragment> &parsedFragments, ModuleImporter *moduleImporter, const GenerationOptions &options = GenerationOptions());
}
