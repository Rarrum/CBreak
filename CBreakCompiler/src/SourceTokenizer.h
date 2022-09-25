// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#pragma once

#include "Parser.h"

namespace CBreakCompiler
{
    enum class TokenType
    {
        Invalid,
        Term,
        Symbol,
        NumericLiteral,
        StringLiteral
    };

    struct SourceToken
    {
        std::string_view String = std::string_view();
        TokenType Type = TokenType::Invalid;
        bool WhitespaceBefore = false;

        StringLocation StartLocation;
        StringLocation EndLocation;
    };

    class SourceTokenizer
    {
    public:
        SourceTokenizer(std::string_view source, const StringLocation &initialSourceLocation);

        inline const SourceToken& NextToken() const { return nextToken; }
        void ConsumeToken();
        SourceTokenizer ConsumePairedRange(std::string_view beginToken, std::string_view endToken);

    private:
        std::string_view source;
        std::string_view::iterator iterNextSourceChar;
        StringLocation currentLocation;
        SourceToken nextToken;

        SourceToken ParseNextToken();
    };
}
