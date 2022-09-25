// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#pragma once

#include "ModuleCompiler.h"
#include <vector>

namespace CBreakCompiler
{
    enum class OutputFormat
    {
        IRText,
        AsmText,
        Bitcode,
        NativeObject
    };

    struct IRProcessingFailedException
    {
        std::string Message;
    };

    //Converts llvm ir into another form.
    //May throw: IRProcessingFailedException
    std::vector<char> GenerateOutput(const CompiledModule &module, OutputFormat outputFormat);
}
