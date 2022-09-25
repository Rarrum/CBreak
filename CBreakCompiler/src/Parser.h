// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#pragma once

#include "Language.h"
#include <string>
#include <exception>
#include <vector>
#include <memory>

namespace CBreakCompiler
{
    struct ParseFailedException
    {
        std::string Message;
        SourceLocation Location;
        SourceLocation OtherLocation;
    };

    struct ParsedModuleFragment
    {
        std::string FileName; //for diagnostic purposes
        std::vector<VariableInformation> GlobalVariables;
        std::vector<FunctionInformation> GlobalFunctions;
        std::vector<ClassInformation> GlobalClasses;
        std::vector<ModuleConfigurationValue> Configurations;
    };

    //Parses a source file which represents a module fragment until a language-specific representation.
    //May throw: ParseFailedException
    ParsedModuleFragment ParseSource(std::string_view sourceText, std::string_view fileNameForDiagnostics = std::string_view());
}
