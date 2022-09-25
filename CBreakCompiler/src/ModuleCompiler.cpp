// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "ModuleCompiler.h"
#include "int/ModuleCompilerInternal.h"
#include <unordered_set>

namespace
{
    using namespace CBreakCompiler;

    class LLVMErrorHandlerWrapper
    {
    public:
        LLVMErrorHandlerWrapper()
        {
            llvm::install_fatal_error_handler(WrapFatalError, this);
        }

        ~LLVMErrorHandlerWrapper()
        {
            llvm::remove_fatal_error_handler();
        }

    private:
        static void WrapFatalError(void *user_data, const std::string &reason, bool gen_crash_diag)
        {
            throw CodeGenerationFailedException{.Message = reason};
        }
    };
}

namespace CBreakCompiler
{
    using namespace CBreakCompilerInternal;

    CompiledModule* DefaultModuleImporter::LoadModule(const std::string &moduleName)
    {
        //TODO: Some mechanism for having the language-provided built-in modules, without needing to go to disk.  This makes embedding the compiler easier.

        //TODO: search paths
        return nullptr;
    }

    CompiledModule GenerateModule(llvm::LLVMContext &llvmContext, const std::vector<ParsedModuleFragment> &parsedFragments, ModuleImporter *moduleImporter, const GenerationOptions &options)
    {
        if (parsedFragments.empty())
            throw CodeGenerationFailedException{.Message = "No fragments specified."};

        //set up compiler components
        CompiledModule moduleToReturn;
        LLVMErrorHandlerWrapper wrapLLVMErrors;
        moduleToReturn.LLVMModule = std::make_unique<llvm::Module>("(not set)", llvmContext);
        try
        {
            ModuleCompilerInternal compiler(moduleToReturn, *moduleToReturn.LLVMModule, options);
            compiler.Config.CreateFromFragments(compiler, parsedFragments);
            compiler.Types.CreateBuiltInTypes();

            moduleToReturn.Identifier = compiler.Config.Identifier;

            //The first thing we do is process our imports, since there there may be types we need to reference there.
            //TODO: handle module version stuff here too.. maybe #Import also specifies version?
            std::unordered_set<std::string> alreadyImportedModules;
            for (const ModuleIdentifier &importId : compiler.Config.ModuleImports)
            {
                if (!moduleImporter)
                    throw CodeGenerationFailedException{.Message = "Module #Import is not available."};

                if (alreadyImportedModules.find(importId.Name) != alreadyImportedModules.end())
                    throw CodeGenerationFailedException{.Message = "Module '" + importId.Name + "' has more than one #Import statements."};
                alreadyImportedModules.emplace(importId.Name);

                CompiledModule *importedModule = moduleImporter->LoadModule(importId.Name);
                if (!moduleImporter)
                    throw CodeGenerationFailedException{.Message = "Module '" + importId.Name + "' not found for #Import."};

                moduleToReturn.ImportedModules.emplace_back(importedModule->Identifier);

                //imported global functions
                for (const FunctionInformation &funcDecl : importedModule->ExportedGlobalFunctions)
                    compiler.AddExternalFunctionDeclaration(funcDecl);

                //TODO: everything else
            }

            //We want to emit types for our classes first.  However those types may depend on nested types being defined.
            //Since we don't require forward declarations, we'll keep trying to emit what types we can resolve in a loop until we get them all.
            //If we get stuck making no progress, that means any types left are misdefined.
            std::list<std::tuple<const ParsedModuleFragment*, const ClassInformation*>> unresolvedClasses;
            for (const ParsedModuleFragment &pmf : parsedFragments)
            {
                for (const ClassInformation &ci : pmf.GlobalClasses)
                    unresolvedClasses.emplace_back(&pmf, &ci);
            }

            while (!unresolvedClasses.empty())
            {
                size_t initialCount = unresolvedClasses.size();

                for (auto uci = unresolvedClasses.begin(); uci != unresolvedClasses.end(); )
                {
                    auto [pmf, ci] = *uci;
                    if (compiler.AddClassDeclaration(*ci, false))
                        uci = unresolvedClasses.erase(uci);
                    else
                        ++uci;
                }

                if (initialCount == unresolvedClasses.size())
                {
                    auto [pmf, ci] = unresolvedClasses.front();
                    if (compiler.AddClassDeclaration(*ci, true))
                        throw CodeGenerationFailedException{.Message = "(internal) Expected AddClassDeclaration to throw."};
                }
            }

            //global variable declarations
            for (const ParsedModuleFragment &pmf : parsedFragments)
            {
                for (const VariableInformation &vi : pmf.GlobalVariables)
                    compiler.AddGlobalVariable(vi);
            }

            //function declarations
            for (const ParsedModuleFragment &pmf : parsedFragments)
            {
                for (const FunctionInformation &fi : pmf.GlobalFunctions)
                    compiler.AddFunctionDeclaration(fi, nullptr);
            }

            for (const ParsedModuleFragment &pmf : parsedFragments)
            {
                for (const ClassInformation &ci : pmf.GlobalClasses)
                {
                    for (const FunctionInformation &fi : ci.Functions)
                        compiler.AddFunctionDeclaration(fi, &ci);
                }
            }

            //global function bodies
            for (const ParsedModuleFragment &pmf : parsedFragments)
            {
                for (const FunctionInformation &fi : pmf.GlobalFunctions)
                {
                    FunctionDetails &fd = compiler.GetGlobalFunction(fi.Name, fi.StartLocation);
                    compiler.AddFunctionBody(fd, fi, nullptr);
                }
            }

            //class function bodies
            for (const ParsedModuleFragment &pmf : parsedFragments)
            {
                for (const ClassInformation &ci : pmf.GlobalClasses)
                {
                    TypeDetails &classType = compiler.Types.GetTypeDetails(ci.Name, ci.StartLocation);

                    for (const FunctionInformation &fi : ci.Functions)
                    {
                        FunctionDetails &fd = *classType.Members->Functions.TryFindName(fi.Name);
                        if (fd.Parameters.empty() || fd.Parameters[0].Name != "#this")
                            throw CodeGenerationFailedException{.Message = "(internal) #this implicit function argument missing.", .Location = fi.StartLocation};
                        else if (fd.Parameters[0].Type->PointerDepth != 1)
                            throw CodeGenerationFailedException{.Message = "(internal) #this implicit function argument is not a single pointer.", .Location = fi.StartLocation};

                        EvaluationResult classTarget{.Type = fd.Parameters[0].Type, .IRValue = fd.Parameters[0].IRFunctionParameter, .IsConst = true, .NeedsDereferenceForValue = false};
                        classTarget.SanityCheckType();

                        compiler.AddFunctionBody(fd, fi, &classTarget);
                    }
                }
            }
        }
        catch (CodeValidationException &cve)
        {
            cve.PartialModule = std::move(moduleToReturn);
            throw std::move(cve);
        }

        //validate that llvm is happy with the module
        if (!options.DisableLLVMVerifier)
        {
            std::string validationErrors;
            llvm::raw_string_ostream validationErrorsStream(validationErrors);
            if (llvm::verifyModule(*moduleToReturn.LLVMModule, &validationErrorsStream))
            {
                validationErrorsStream.flush();
                CodeValidationException cve;
                cve.Message = "(internal) llvm module verification failed:\r\n" + validationErrors;
                cve.PartialModule = std::move(moduleToReturn);
                throw std::move(cve);
            }
        }

        return moduleToReturn;
    }
}
