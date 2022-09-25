// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#pragma once

#include "ModuleCompiler.h"

namespace CBreakCompiler
{
    struct JITFailedException
    {
        std::string Message;
    };

    //Runs the specified function from compiled a compiled module using JIT.
    //WARNING: This obliterates the module.. it will become invalid after this call.. as will everything returned from moduleImporter!!!!  TODO: Figure out how to make this not happen...
    //The function must return a 32-bit integer and take no arguments. (TODO: make this restriction not a thing)
    //May throw JITFailedException
    int32_t RunJIT(CompiledModule &&module, ModuleImporter *moduleImporter, const std::string &functionName, bool isCExport = false);
}
