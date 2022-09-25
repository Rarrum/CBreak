// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "ModuleCompilerInternal.h"

namespace CBreakCompilerInternal
{
    using namespace CBreakCompiler;

    void ModuleConfiguration::CreateFromFragments(ModuleCompilerInternal &compiler, const std::vector<ParsedModuleFragment> &parsedFragments)
    {
        if (finalized)
            throw CodeGenerationFailedException{.Message = "(internal) Attempt to modify finalized module configuration"};

        for (const ParsedModuleFragment &pmf : parsedFragments)
        {
            for (const ModuleConfigurationValue &mcv : pmf.Configurations)
                ApplyConfigurationValue(mcv);
        }

        FinalizeConfig(compiler);

        std::string moduleFileForLLVM;
        for (const ParsedModuleFragment &pmf : parsedFragments)
        {
            if (!pmf.FileName.empty())
            {
                if (!moduleFileForLLVM.empty())
                    moduleFileForLLVM += ";";

                moduleFileForLLVM += pmf.FileName;
            }
        }

        if (moduleFileForLLVM.empty())
            moduleFileForLLVM = Identifier.Name;

        compiler.LLVMModule.setSourceFileName(moduleFileForLLVM);
        compiler.LLVMModule.setModuleIdentifier(Identifier.Name);
        finalized = true;
    }

    void ModuleConfiguration::ApplyConfigurationValue(const ModuleConfigurationValue &mcv)
    {
        if (mcv.Name == "#ModuleName")
        {
            if (!mcv.Value.IsString || mcv.Value.String.empty() || !((mcv.Value.String[0] >= 'a' && mcv.Value.String[0] <= 'z') || (mcv.Value.String[0] >= 'A' && mcv.Value.String[0] <= 'Z')))
                throw CodeGenerationFailedException{.Message = "Invalid #ModuleName value.", .Location = mcv.StartLocation};
            else if (!Identifier.Name.empty())
                throw CodeGenerationFailedException{.Message = "#ModuleName may only be specified once.", .Location = mcv.StartLocation};
            else
                Identifier.Name = mcv.Value.String;
        }
        else if (mcv.Name == "#Import")
        {
            if (!mcv.Value.IsString || mcv.Value.String.empty())
                throw CodeGenerationFailedException{.Message = "Invalid #Import value.", .Location = mcv.StartLocation};

            //TODO: handle module version stuff here too.. maybe #Import also specifies version?
            ModuleImports.emplace_back(ModuleIdentifier{.Name = mcv.Value.String});
        }
        else if (mcv.Name == "#DefaultIntBits")
        {
            int newBits = (int)mcv.Value.UnsignedNumber;
            if (!mcv.Value.IsInteger())
                throw CodeGenerationFailedException{.Message = "Invalid #DefaultIntBits value.", .Location = mcv.StartLocation};
            else if (DefaultIntBits != 0)
                throw CodeGenerationFailedException{.Message = "#DefaultIntBits may only be specified once.", .Location = mcv.StartLocation};
            else
                DefaultIntBits = newBits;
        }
        else if (mcv.Name == "#DefaultFloatBits")
        {
            int newBits = (int)mcv.Value.UnsignedNumber;
            if (!mcv.Value.IsInteger())
                throw CodeGenerationFailedException{.Message = "Invalid #DefaultFloatBits value.", .Location = mcv.StartLocation};
            else if (DefaultFloatBits != 0)
                throw CodeGenerationFailedException{.Message = "#DefaultFloatBits may only be specified once.", .Location = mcv.StartLocation};
            else
                DefaultFloatBits = newBits;
        }
        else
            throw CodeGenerationFailedException{.Message = "Unrecognized configuration name '" + mcv.Name + "'.", .Location = mcv.StartLocation};
    }

    void ModuleConfiguration::FinalizeConfig(ModuleCompilerInternal &compiler)
    {
        //assign defaults as needed and verify values
        if (Identifier.Name.empty())
            Identifier.Name = "Default";

        if (DefaultIntBits == 0)
            DefaultIntBits = 64;

        if (DefaultFloatBits == 0)
            DefaultFloatBits = 64;

        if (compiler.Types.TryGetTypeDetails("#int" + std::to_string(DefaultIntBits)))
            throw CodeGenerationFailedException{.Message = "Invalid #DefaultIntBits value in module configuration."};

        if (compiler.Types.TryGetTypeDetails("float" + std::to_string(DefaultFloatBits)))
            throw CodeGenerationFailedException{.Message = "Invalid #DefaultFloatBits value in module configuration."};
    }
}
