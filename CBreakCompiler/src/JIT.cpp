// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "JIT.h"

#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <unordered_set>

namespace
{
    using namespace CBreakCompiler;

    void AddModuleToJit(llvm::orc::LLJIT &jit, std::unordered_set<std::string> &alreadyImportedModules, ModuleImporter *moduleImporter, CompiledModule &&module)
    {
        for (const ModuleIdentifier &importId : module.ImportedModules)
        {
            if (!moduleImporter)
                throw JITFailedException{.Message = "The module provided has imports, but no module importer is available."};

            if (alreadyImportedModules.find(importId.Name) != alreadyImportedModules.end())
                continue;

            CompiledModule *importedModule = moduleImporter->LoadModule(importId.Name);
            if (!moduleImporter)
                throw JITFailedException{.Message = "Module '" + importId.Name + "' not found for #Import."};

            AddModuleToJit(jit, alreadyImportedModules, moduleImporter, std::move(*importedModule));
        }

        llvm::Error err = jit.addIRModule(llvm::orc::ThreadSafeModule(std::move(module.LLVMModule), std::make_unique<llvm::LLVMContext>()));
        if (err)
            throw JITFailedException{.Message = "addIRModule failed."};
    }
}

namespace CBreakCompiler
{
    int32_t RunJIT(CompiledModule &&module, ModuleImporter *moduleImporter, const std::string &functionName, bool isCExport)
    {
        //Set up jit stuff
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();

        llvm::Expected<std::unique_ptr<llvm::orc::LLJIT>> jit = llvm::orc::LLJITBuilder().create();
        if (!jit)
            throw JITFailedException{.Message = "LLJITBuilder unavailable."};

        //Load up all of the modules this one imports, and feed them all to the jit beast
        std::unordered_set<std::string> alreadyImportedModules;
        AddModuleToJit(*jit.get(), alreadyImportedModules, moduleImporter, std::move(module));

        //Find the function they asked for and run it
        llvm::Expected<llvm::JITEvaluatedSymbol> jitFunc = jit.get()->lookup(functionName);
        if (!jitFunc)
            throw JITFailedException{.Message = "Function '" + functionName + "' not found."};

        if (isCExport)
        {
            auto *callableFunc = (int32_t(*)())jitFunc.get().getAddress();
            return callableFunc();
        }
        else
        {
            auto *callableFunc = (void(*)(int32_t*))jitFunc.get().getAddress();
            int32_t returnValue = -1;
            callableFunc(&returnValue);
            return returnValue;
        }
    }
}
