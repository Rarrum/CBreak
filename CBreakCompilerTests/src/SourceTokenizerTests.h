// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include <cxxtest/TestSuite.h>
#include <string>

#include "SourceTokenizer.h"
using namespace CBreakCompiler;

namespace
{
    SourceToken ObserveThenConsumeToken(SourceTokenizer &source)
    {
        SourceToken token = source.NextToken();
        source.ConsumeToken();
        return token;
    }
}

class SourceTokenizerTests: public CxxTest::TestSuite
{
public:
    void testEmpty()
    {
        SourceTokenizer source{std::string_view(), StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }

    void testSimpleTerms()
    {
        SourceTokenizer source{"cats _are_ great", StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "cats");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "_are_");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "great");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }

    void testSimpleSymbols()
    {
        std::string symbols = "}{)(*&^%$@!~`[]|\\+=?/<,>.-";
        SourceTokenizer source{symbols, StringLocation()};

        for (char c : symbols)
            TS_ASSERT(ObserveThenConsumeToken(source).String == std::string{c});
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }

    void testSimpleKeywords()
    {
        std::string symbols = "a #b # c";
        SourceTokenizer source{symbols, StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "a");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "#b");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "#");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "c");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }

    void testSimpleComments()
    {
        SourceTokenizer source{"//c1\nmeow/*c2*/woof", StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "meow");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "woof");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }

    void testNestedComments()
    {
        SourceTokenizer source{"meow/*//*//*aa/*bb*/cc*//**/woof", StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "meow");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "woof");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }

    void testSimpleStrings()
    {
        SourceTokenizer source{"\"s1\" \"s2\"", StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "s1");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "s2");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }

    void testSimpleNumbers()
    {
        SourceTokenizer source{"123 45.67 0x08 08h", StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "123");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "45.67");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "0x08");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "08h");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }

    void testSimpleScope()
    {
        SourceTokenizer source{"a { b c } d", StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "a");
        SourceTokenizer scopedSource = source.ConsumePairedRange("{", "}");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "d");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());

        TS_ASSERT(ObserveThenConsumeToken(scopedSource).String == "b");
        TS_ASSERT(ObserveThenConsumeToken(scopedSource).String == "c");
        TS_ASSERT(ObserveThenConsumeToken(scopedSource).String.empty());
    }

    void testNestedScope()
    {
        SourceTokenizer source{"{ a { { c } d } }", StringLocation()};

        SourceTokenizer outerScope = source.ConsumePairedRange("{", "}");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());

        TS_ASSERT(ObserveThenConsumeToken(outerScope).String == "a");
        SourceTokenizer middleScope = outerScope.ConsumePairedRange("{", "}");

        SourceTokenizer innerScope = middleScope.ConsumePairedRange("{", "}");
        TS_ASSERT(ObserveThenConsumeToken(middleScope).String == "d");
        TS_ASSERT(ObserveThenConsumeToken(middleScope).String.empty());

        TS_ASSERT(ObserveThenConsumeToken(innerScope).String == "c");
        TS_ASSERT(ObserveThenConsumeToken(innerScope).String.empty());
    }

    void testScopeIncomplete()
    {
        SourceTokenizer source{"a { b ", StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "a");

        TS_ASSERT_THROWS(source.ConsumePairedRange("{", "}"), ParseFailedException);
    }

    void testSimpleFunction()
    {
        std::string symbols = "  #void Foo(#int32 c)\n  {\n    #return;\n  } ";
        SourceTokenizer source{symbols, StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "#void");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "Foo");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "(");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "#int32");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "c");
        TS_ASSERT(ObserveThenConsumeToken(source).String == ")");
        SourceTokenizer scopedSource = source.ConsumePairedRange("{", "}");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());

        TS_ASSERT(ObserveThenConsumeToken(scopedSource).String == "#return");
        TS_ASSERT(ObserveThenConsumeToken(scopedSource).String == ";");
        TS_ASSERT(ObserveThenConsumeToken(scopedSource).String.empty());
    }

    void testNegativeNumberSubtractionOperator()
    {
        SourceTokenizer source{"-1, a - 1, b -1, c - -1, d-1, e--1", StringLocation()};

        TS_ASSERT(ObserveThenConsumeToken(source).String == "-1");
        TS_ASSERT(ObserveThenConsumeToken(source).String == ",");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "a");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "-");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "1");
        TS_ASSERT(ObserveThenConsumeToken(source).String == ",");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "b");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "-1");
        TS_ASSERT(ObserveThenConsumeToken(source).String == ",");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "c");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "-");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "-1");
        TS_ASSERT(ObserveThenConsumeToken(source).String == ",");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "d");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "-");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "1");
        TS_ASSERT(ObserveThenConsumeToken(source).String == ",");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "e");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "-");
        TS_ASSERT(ObserveThenConsumeToken(source).String == "-1");
        TS_ASSERT(ObserveThenConsumeToken(source).String.empty());
    }
};
