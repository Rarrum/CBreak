// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "SourceTokenizer.h"
#include <algorithm>

namespace
{
    using namespace CBreakCompiler;

    inline bool IsTermStartCharacter(char c)
    {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c =='#';
    }

    inline bool IsTermContinuationCharacter(char c)
    {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
    }

    void ValidateToken(const SourceToken &token)
    {
        if (token.Type == TokenType::NumericLiteral)
        {
            size_t periodCount = std::count_if(token.String.begin(), token.String.end(), [](char c) { return c == '.'; });
            if (periodCount > 1)
                throw ParseFailedException{.Message = "Numeric literal may only have a single '.' character.", .Location = SourceLocation("", token.StartLocation)};

            if (token.String.back() == '.')
                throw ParseFailedException{.Message = "Numeric literal may not end in a '.' character.", .Location = SourceLocation("", token.StartLocation)};
        }
    }

    //TODO: delete this and use ctor after all the major compilers support C++20
    std::string_view MakeStringView(const std::string_view::iterator &a, const std::string_view::iterator &b)
    {
        if (a == b)
            return std::string_view();

        return std::string_view{&*a, (size_t)(b - a)};
    }
}

namespace CBreakCompiler
{
    SourceTokenizer::SourceTokenizer(std::string_view source, const StringLocation &initialSourceLocation):
        source(source), currentLocation(initialSourceLocation)
    {
        iterNextSourceChar = source.begin();
        nextToken = ParseNextToken();
    }

    SourceToken SourceTokenizer::ParseNextToken()
    {
        SourceToken token
        {
            .StartLocation = currentLocation,
            .EndLocation = currentLocation
        };

        bool startedTerm = false;
        bool startedSingleLineComment = false;
        int multiLineCommentDepth = 0;
        bool startedString = false;
        bool startedNumber = false;
        bool startedSymbol = false;
        bool skipNextChar = false;
        bool stopAfterNext = false;
        std::string_view::iterator iterTokenStart = iterNextSourceChar;

        for (; iterNextSourceChar != source.end(); ++iterNextSourceChar)
        {
            char c = *iterNextSourceChar;

            bool isWhiteSpace = (c == ' ' || c == '\t' || c == '\r' || c == '\n');
            bool isLineBreak = (c == '\n');

            if (!isWhiteSpace && ((uint8_t)c < 32 || (uint8_t)c > 127))
                throw ParseFailedException{.Message = "Unsupported character in source: " + std::to_string((uint8_t)c), .Location = SourceLocation("", currentLocation)};

            if (skipNextChar)
            {
                skipNextChar = false;
            }
            else if (stopAfterNext)
            {
                break;
            }
            else if (startedTerm)
            {
                if (isWhiteSpace)
                    break;

                if (!IsTermContinuationCharacter(c))
                    break;
            }
            else if (startedNumber)
            {
                if (isWhiteSpace)
                    break;

                if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '.'))
                    break;
            }
            else if (startedString)
            {
                if (isLineBreak)
                    throw ParseFailedException{.Message = "Unexpected linebreak in string.", .Location = SourceLocation("", currentLocation)};

                if (c == '"')
                    stopAfterNext = true;
            }
            else if (startedSingleLineComment)
            {
                if (isLineBreak)
                    startedSingleLineComment = false;
            }
            else if (multiLineCommentDepth > 0)
            {
                if (c == '/' && source.end() - iterNextSourceChar >= 2 && *(iterNextSourceChar + 1) == '*')
                {
                    ++multiLineCommentDepth;
                }
                else if (c == '*' && source.end() - iterNextSourceChar >= 2 && *(iterNextSourceChar + 1) == '/')
                {
                    skipNextChar = true;
                    --multiLineCommentDepth;
                }
            }
            else if (startedSymbol)
            {
                break;
            }
            else if (!isWhiteSpace)
            {
                token.StartLocation = currentLocation;
                iterTokenStart = iterNextSourceChar;

                if (IsTermStartCharacter(c))
                {
                    token.Type = TokenType::Term;
                    startedTerm = true;
                }
                else if (c >= '0' && c <= '9')
                {
                    token.Type = TokenType::NumericLiteral;
                    startedNumber = true;
                }
                else if (c == '"')
                {
                    token.Type = TokenType::StringLiteral;
                    startedString = true;
                }
                else if (c == '/' && source.end() - iterNextSourceChar >= 2 && (*(iterNextSourceChar + 1) == '/' || *(iterNextSourceChar + 1) == '*'))
                {
                    if (*(iterNextSourceChar + 1) == '/')
                        startedSingleLineComment = true;
                    else
                        multiLineCommentDepth = 1;
                }
                else if (c == '-')
                {
                    //this could be a symbol, or it could be the prefix for a number literal
                    if ((iterNextSourceChar + 1) != source.end() && *(iterNextSourceChar + 1) >= '0' && *(iterNextSourceChar + 1) <= '9')
                    {
                        if (iterNextSourceChar != source.begin() && IsTermContinuationCharacter(*(iterNextSourceChar - 1))) //if an identifier came before it without any whitespace between
                        {
                            token.Type = TokenType::Symbol;
                            startedSymbol = true;
                        }
                        else
                        {
                            token.Type = TokenType::NumericLiteral;
                            startedNumber = true;
                        }
                    }
                    else
                    {
                        token.Type = TokenType::Symbol;
                        startedSymbol = true;
                    }
                }
                else
                {
                    token.Type = TokenType::Symbol;
                    startedSymbol = true;
                }
            }
            else if (isWhiteSpace)
            {
                token.WhitespaceBefore = true;
            }

            ++currentLocation.Column;
            if (c == '\n')
            {
                ++currentLocation.Line;
                currentLocation.Column = 1;
            }
        }

        token.EndLocation = currentLocation;
        token.String = MakeStringView(iterTokenStart, iterNextSourceChar);

        if (token.Type == TokenType::StringLiteral && token.String.size() >= 2) //trim quotes off
        {
            token.String.remove_prefix(1);
            token.String.remove_suffix(1);
        }

        if (token.Type == TokenType::Invalid)
            token.String = std::string_view();
        else if (token.String.empty())
            token.Type = TokenType::Invalid;
        else
            ValidateToken(token);

        if (token.String == "#true" || token.String == "#false")
            token.Type = TokenType::NumericLiteral;

        return token;
    }

    void SourceTokenizer::ConsumeToken()
    {
        nextToken = ParseNextToken();
    }

    SourceTokenizer SourceTokenizer::ConsumePairedRange(std::string_view beginToken, std::string_view endToken)
    {
        if (nextToken.String != beginToken)
            throw ParseFailedException{.Message = "Expected: " + std::string{beginToken}, .Location = SourceLocation("", currentLocation)};

        std::string_view::iterator iterRangeStart = iterNextSourceChar;
        StringLocation rangeStartLocation = currentLocation;

        ConsumeToken();

        int depth = 1;

        for (; nextToken.Type != TokenType::Invalid; ConsumeToken())
        {
            if (nextToken.String == beginToken)
                ++depth;
            else if (nextToken.String == endToken)
            {
                --depth;

                if (depth == 0)
                {
                    std::string_view::iterator iterRangeEnd = iterNextSourceChar - endToken.size();
                    ConsumeToken();
                    return SourceTokenizer{MakeStringView(iterRangeStart, iterRangeEnd), rangeStartLocation};
                }
            }
        }

        throw ParseFailedException{.Message = "Expected: " + std::string{endToken}, .Location = SourceLocation("", currentLocation)};
    }
}
