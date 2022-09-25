// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "Language.h"

namespace CBreakCompiler
{
    VariableInformation VariableInformation::CopyWithoutCode() const
    {
        VariableInformation copy;
        copy.Type = Type;
        copy.Name = Name;
        copy.IsConst = IsConst;
        copy.Qualifiers = Qualifiers;
        copy.StartLocation = StartLocation;

        return copy;
    }

    FunctionInformation FunctionInformation::CopyWithoutCode() const
    {
        FunctionInformation copy;
        copy.Name = Name;
        copy.Qualifiers = Qualifiers;
        copy.StartLocation = StartLocation;

        copy.Returns.reserve(Returns.size());
        for (auto &r : Returns)
            copy.Returns.emplace_back(r.CopyWithoutCode());

        copy.Parameters.reserve(Parameters.size());
        for (auto &r : Parameters)
            copy.Parameters.emplace_back(r.CopyWithoutCode());

        return copy;
    }
}
