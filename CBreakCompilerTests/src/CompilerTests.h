// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include <cxxtest/TestSuite.h>
#include <string>
#include <iostream>

#include "Parser.h"
#include "ModuleCompiler.h"
#include "JIT.h"

using namespace CBreakCompiler;

namespace
{
    int RunAndReturnCode(const std::string &program, const std::string &function = "::main", bool isCExport = false)
    {
        std::unique_ptr<llvm::LLVMContext> llvmContext = std::make_unique<llvm::LLVMContext>();
        try
        {
            std::vector<ParsedModuleFragment> fragments;
            fragments.emplace_back(ParseSource(program, "test"));
            CompiledModule module = GenerateModule(*llvmContext, fragments, nullptr);
            return RunJIT(std::move(module), nullptr, function, isCExport);
        }
        catch (const ParseFailedException& e)
        {
            std::cerr<<"At "<<e.Location.Location.Line<<":"<<e.Location.Location.Column<<": "<<e.Message<<std::endl;
            std::cerr<<"Program:\n"<<program<<std::endl;
            throw;
        }
        catch (const CodeGenerationFailedException& e)
        {
            std::cerr<<"At "<<e.Location.Location.Line<<":"<<e.Location.Location.Column<<": "<<e.Message<<std::endl;
            std::cerr<<"Program:\n"<<program<<std::endl;
            throw;
        }
        catch (const JITFailedException& e)
        {
            std::cerr<<e.Message<<std::endl;
            std::cerr<<"Program:\n"<<program<<std::endl;
            throw;
        }
    }
}

class CompilerTests: public CxxTest::TestSuite
{
public:
    void testSimple()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #return 1 + 1; }");
        TS_ASSERT_EQUALS(ret, 2);
    }

    void testParenthesis()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #return (1 + ((2 + 1) - (1 + 0)) * 4); }");
        TS_ASSERT_EQUALS(ret, 9);
    }

    void testSimpleFunctions()
    {
        int ret = RunAndReturnCode("(#int32) mul2(#int32 a){#return a*2;}\n#export (#int32) main() { #return :mul2(::add(2,3)); }\n(#int32) add(#int32 a, #int32 b){#return a+b;}");
        TS_ASSERT_EQUALS(ret, 10);
    }

    void testFunctionNamedReturnAssignment()
    {
        int ret = RunAndReturnCode("(#int32 ret) mul2(#int32 a) { ret=a*2; #return; }\n#export (#int32) main() { #var #int32 loc; (loc):mul2(13); #return loc+1; }");
        TS_ASSERT_EQUALS(ret, 27);
    }

    void testFunctionNamedReturnDirect()
    {
        int ret = RunAndReturnCode("(#int32 ret) mul2(#int32 a) { #return a*2; }\n#export (#int32) main() { #var #int32 loc; (loc)::mul2(13); #return loc+1; }");
        TS_ASSERT_EQUALS(ret, 27);
    }

    void testFunctionTwoReturn()
    {
        int ret = RunAndReturnCode("(#int32 retone, #int32 rettwo) func(#int32 a){retone = a*2; rettwo = retone*2; #return;}\n#export (#int32) main() { #var #int32 loc1; #var #int32 loc2; (loc1, loc2):func(3); #return loc1+loc2; }");
        TS_ASSERT_EQUALS(ret, 18);
    }

    void testSimpleNamespace()
    {
        int ret = RunAndReturnCode("#export (#int32) main(){#return N::f()+::alt();}\n#namespace N{ (#int32) f(){#return 3;} } (#int32) alt(){#return 1;}");
        TS_ASSERT_EQUALS(ret, 4);
    }

    void testLocalVariables()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 7; #var #int32 b; #var #int32 c = ::add(a, b); #return c; }\n(#int32) add(#int32 a, #int32 b){#return a+b;}");
        TS_ASSERT_EQUALS(ret, 7);
    }

    void testGlobalVariables()
    {
        int ret = RunAndReturnCode("#var #int32 ga;#export (#int32) main() { ::gb = 3; #var #int32 la = ::ga; #return la+::gb; }#var #int32 gb;");
        TS_ASSERT_EQUALS(ret, 3);
    }

    void testCExport()
    {
        int ret = RunAndReturnCode("#cexport(\"MainForC\") (#int32) RealMain() { #return 1 + 1; }", "MainForC", true);
        TS_ASSERT_EQUALS(ret, 2);
    }

    void testAssignment()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int32 la; ::glob = 7; la = ::glob + 1; la = la * 2; #return la; }#var #int32 glob;");
        TS_ASSERT_EQUALS(ret, 16);
    }

    void testClassDataAccessFromOutside()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var ::C c; c.V1 = 1; c.V2 = 2; #return c.V1 + c.V2; } #class C{#var #int32 V1;#var #int32 V2;}");
        TS_ASSERT_EQUALS(ret, 3);
    }

    void testClassFuncFromOutside()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var ::C c; #var #int32 l1 = c.funcy(3); l1 = c.funcy(l1); #return l1; } #class C{(#int32) funcy(#int32 n){#return n*2;}}");
        TS_ASSERT_EQUALS(ret, 12);
    }

    void testClassDataAccessFromInside()
    {
        int ret = RunAndReturnCode("#export (#int32) main(){#var N::C c; c.V1 = 3; #return c.funcy();}\n#namespace N{#class C{#var #int32 V1; (#int32) funcy(){.V2 = 5; #return .V1 + #this.V2;} #var #int32 V2;}}");
        TS_ASSERT_EQUALS(ret, 8);
    }

    void testClassFuncFromInside()
    {
        int ret = RunAndReturnCode("#export (#int32) main(){#var N::C c; #return c.f1();}\n#namespace N{#class C{#var #int32 V1; (#int32) f1(){.V1 = 3; #return .f2(.V1);} (#int32) f2(#int32 a){#return .V1 + a;}}}");
        TS_ASSERT_EQUALS(ret, 6);
    }

    void testClassNesting()
    {
        std::string program = 
"#export (#int32) main()"
"{"
"    #var :C2 outter1;"
"    #var :C2 outter2;"
"    outter1.Nest2.LeafA = 1;"
"    outter1.Nest2.LeafB = 2;"
"    outter2.Nest2.LeafA = outter1.Nest2.LeafA + 2;"
"    outter2.Nest2.LeafB = outter1.Nest2.LeafB + 2;"
"    #return outter1.Nest2.LeafA + outter2.add();"
"}"
"#class C1"
"{"
"    #var #int32 LeafA;"
"    #var #int32 LeafB;"
"}"
"#class C2"
"{"
"    #var ::C1 Nest1;"
"    #var :C1 Nest2;"
"    (#int32) add()"
"    {"
"        #return .Nest1.LeafA + .Nest2.LeafA + #this.Nest1.LeafB + #this.Nest2.LeafB;"
"    }"
"}";
        int ret = RunAndReturnCode(program);
        TS_ASSERT_EQUALS(ret, 8);
    }

    void testClassFieldInitialization()
    {
        std::string program = 
"#export (#int32) main()"
"{"
"    #var :C2 outter;"
"    #return outter.add();"
"}"
"#class C1"
"{"
"    #var #int32 LeafA = 2;"
"    #var #int32 LeafB = 7;"
"}"
"#class C2"
"{"
"    #var ::C1 Nest1;"
"    #var ::C1 Nest2;"
"    #var #int32 Middle = 3;"
"    (#int32) add()"
"    {"
"        #return .Nest1.LeafA + .Nest2.LeafA + .Nest1.LeafB + .Nest2.LeafB + .Middle;"
"    }"
"}";
        int ret = RunAndReturnCode(program);
        TS_ASSERT_EQUALS(ret, 21);
    }

    void testClassMultiReturnFunc()
    {
        std::string program = 
"#class C1"
"{"
"    #var #int32 Accumulation;"
"    (#int32 retone, #int32 rettwo) MultiFunc (#int32 a)"
"    {"
"        .Accumulation = .Accumulation + a;"
"        retone = .Accumulation;"
"        rettwo = retone * 2;"
"        #return;"
"    }"
"}"
"#class C2"
"{"
"    #var #int32 X;"
"    #var #int32 Y;"
"}"
"#export (#int32) main()"
"{"
"    #var :C1 c1;"
"    #var :C2 c2;"
"    (c2.X, c2.Y) c1.MultiFunc (3);"
"    (c2.X, c2.Y) c1.MultiFunc (3);"
"    #return c2.X + c2.Y;"
"}";
        int ret = RunAndReturnCode(program);
        TS_ASSERT_EQUALS(ret, 18);
    }

    // We don't support classas as global variables yet
    //void testClassGlobal()
    //{
    //    int ret = RunAndReturnCode("#export #int32 main() { #return glob.V1; } #class C{#var #int32 V1 = 3;} #var ::C glob;");
    //    TS_ASSERT_EQUALS(ret, 3);
    //}

    //TODO: passing a class instance as an arg?

    void testConfigDefaultInt()
    {
        int ret = RunAndReturnCode("#configuration{#DefaultIntBits 32;}#export (#int) main() { ::glob = 3; #var #int32 la = 1; #var #int lb = 2; #return la+lb+::glob; }#var #int glob;");
        TS_ASSERT_EQUALS(ret, 6);
    }

    void testSimpleIntegerStorage()
    {
        int ret = RunAndReturnCode("#export (#uint32) main() { #var #uint8 u8 = 255; #var #int8 i8p = 127; #var #int8 i8n = -128; #var #uint16 u16 = 65535; #var #uint32 u32 = 1; #var #int32 s32p = 1; #var #int32 s32n1 = -1; #var #int32 s32n2 = -2; #var #uint64 u64 = 0; #var #int64 i64 = 0; #return 1; }");
        TS_ASSERT_EQUALS(ret, 1);
    }

    void testSimpleUnsignedIntegerConversions()
    {
        int ret = RunAndReturnCode("#export (#uint32) main() { #var #uint8 u8 = 100; #var #uint16 u16 = 10; #var #uint32 u32 = u8 * u16; #return u32 - u8; }");
        TS_ASSERT_EQUALS(ret, 900);
    }

    void testSimpleSignedIntegerConversions()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int8 i8 = -100; #var #int16 i16 = 10; #var #int32 i32 = i8 * i16; #return i32 - i8; }");
        TS_ASSERT_EQUALS(ret, -900);
    }

    void testSimpleUnsignedIntegerTruncation()
    {
        int ret = RunAndReturnCode("#export (#uint32) main() { #var #uint64 u64 = 0xffffffffffffffff; #return #truncate u64; }");
        TS_ASSERT_EQUALS(ret, 0xffffffff);
    }

    void testSimpleSignedIntegerTruncation()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int64 i64 = -2; #var #int8 i8 = -1;  #return #truncate i64 + i8; }");
        TS_ASSERT_EQUALS(ret, -3);
    }

    void testSimpleIntegerParenthesisTruncation()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int64 i64 = -1; #var #int8 i8 = -1;  #return #truncate (i64 + i8); }");
        TS_ASSERT_EQUALS(ret, -2);
    }

    void testSimpleIntegerSignTruncation()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int16 i16 = -32768 + 16; #var #uint8 u8 = #truncate i16; #return u8; }");
        TS_ASSERT_EQUALS(ret, 16);
    }

    void testChildBlocks()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 2; { a = a + 3; } { a = a + 4; {}} #return a; }");
        TS_ASSERT_EQUALS(ret, 9);
    }

    void testIfTrue()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #if (#true) #return 1; #return 2; }");
        TS_ASSERT_EQUALS(ret, 1);
    }

    void testIfFalse()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #if (#false) #return 1; #return 2; }");
        TS_ASSERT_EQUALS(ret, 2);
    }

    void testElse()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 2; #if (#false) a = a + 1; #else a = a + 2; a = a + 3; #return a; }");
        TS_ASSERT_EQUALS(ret, 7);
    }

    void testIfBlock()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 2; #if (#true) { a = a + 1; #return a; } #return 1; }");
        TS_ASSERT_EQUALS(ret, 3);
    }

    void testElseBlockEmptyIf()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 2; #if (#false) {} #else { a = a + 1; #return a; } #return 1; }");
        TS_ASSERT_EQUALS(ret, 3);
    }

    void testComparisons()
    {
        int ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 5; #if (a < 9) #return 1; #return 2; }");
        TS_ASSERT_EQUALS(ret, 1);

        ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 5; #if (a > 9) #return 1; #return 2; }");
        TS_ASSERT_EQUALS(ret, 2);

        ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 5; #if (a == 5) #return 1; #return 2; }");
        TS_ASSERT_EQUALS(ret, 1);

        ret = RunAndReturnCode("#export (#int32) main() { #var #int32 a = 5; #if (a != 5) #return 1; #return 2; }");
        TS_ASSERT_EQUALS(ret, 2);
    }
};
