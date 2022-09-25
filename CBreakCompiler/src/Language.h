// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#pragma once

#include <string>
#include <exception>
#include <vector>
#include <memory>

namespace CBreakCompiler
{
    struct ExpressionNode;
    struct StatementsBlock;

    struct StringLocation
    {
        int32_t Line = 1;
        int32_t Column = 1;
    };

    struct SourceLocation
    {
        std::string FileName;
        StringLocation Location;

        SourceLocation() = default;
        SourceLocation(const SourceLocation&) = default;
        SourceLocation(SourceLocation&&) = default;
        SourceLocation& operator=(const SourceLocation&) = default;
        SourceLocation& operator=(SourceLocation&&) = default;

        inline SourceLocation(const std::string &fileName, const StringLocation &loc)
        {
            FileName = fileName;
            Location = loc;
        }
    };

    struct GlobalQualifiers
    {
        bool Export = false;
        std::string CExport; //TODO: figure out how to apply this to variables
        bool Pack = false;

        inline bool IsEmpty() const { return !Export && CExport.empty() && !Pack; }
    };

    struct IdentifierQualifiers
    {
        bool Copy = false;
        bool Move = false;
        bool Truncate = false;

        inline bool IsEmpty() const { return !Copy && !Move && !Truncate; }
    };

    struct ConstantLeaf
    {
        //exactly one of these groups will be used
        bool IsString = false;
        std::string String;

        bool IsBool = false;
        bool IsTrue = false;

        bool IsFloat = false;
        double Float = 0.0; //TODO: double can't represents #float128
        int FloatBits = 0; //will be: 16, 32, 64, 128

        uint64_t UnsignedNumber = 0;
        int UnsignedBits = 0; //number of bits needed to represent the value stored in UnsignedNumber, or 0 if the value is signed
        int64_t SignedNumber = 0;
        int SignedBits = 0; //number of bits needed to represent the value stored in SignedNumber

        inline bool IsInteger() const { return !IsString && !IsBool && !IsFloat; }
    };

    struct VariableLeaf
    {
        std::string Name;
    };

    struct FunctionCallLeaf
    {
        std::string Name;
        std::vector<ExpressionNode> Returns;
        std::vector<ExpressionNode> Parameters;
    };

    struct ExpressionLeaf
    {
        //Exactly one of these will be set:
        std::unique_ptr<ConstantLeaf> Constant;
        std::unique_ptr<VariableLeaf> Variable;
        std::unique_ptr<FunctionCallLeaf> Function;
        std::unique_ptr<ExpressionNode> Group;

        IdentifierQualifiers Qualifiers;

        inline bool IsEmpty() const { return !Constant && !Variable && !Function && !Group; }
    };

    struct ExpressionNode
    {
        bool IsBinary = false;
        std::string Operation;

        std::unique_ptr<ExpressionNode> LeftNode;
        std::unique_ptr<ExpressionNode> RightNode; //only used if binary
        ExpressionLeaf Leaf; //only used if LeftNode is null

        SourceLocation StartLocation;

        inline bool IsEmpty() const { return !LeftNode && !RightNode && Leaf.IsEmpty(); }
    };

    struct VariableInformation
    {
        std::string Type; //Suffixed with * for pointer to type
        std::string Name;
        bool IsConst = false;
        GlobalQualifiers Qualifiers;
        std::unique_ptr<ExpressionNode> Initializer;

        SourceLocation StartLocation;

        VariableInformation CopyWithoutCode() const;
    };

    enum class StatementType
    {
        EvaluateOnly, //Execute Expression
        VariableDeclaration, //Activate a local variable, optionally initializing
        Return, //End function execution
        ChildBlock, //A block of statements in a child scope, Execute CodeBlock
        ConditionalIfElse, //Evaluate Expression, perform CodeBlock if true, perform CodeBlockElse if present and expression was false
    };

    struct StatementInformation
    {
        StatementType Type = StatementType::EvaluateOnly;
        std::unique_ptr<ExpressionNode> Expression;
        std::string VariableName;
        std::unique_ptr<StatementsBlock> CodeBlock;
        std::unique_ptr<StatementsBlock> CodeBlockElse;

        SourceLocation StartLocation;
    };

    struct StatementsBlock
    {
        std::vector<StatementInformation> Statements;
        std::vector<VariableInformation> LocalVariables; //may not be used until activated by a VariableDeclaration statement

        SourceLocation StartLocation;
    };

    struct FunctionInformation
    {
        std::string Name;
        GlobalQualifiers Qualifiers;
        std::vector<VariableInformation> Returns;
        std::vector<VariableInformation> Parameters;
        StatementsBlock CodeBlock;

        SourceLocation StartLocation;

        FunctionInformation CopyWithoutCode() const;
    };

    struct ClassInformation
    {
        std::string Name;
        GlobalQualifiers Qualifiers;
        std::vector<VariableInformation> Variables;
        std::vector<FunctionInformation> Functions;

        SourceLocation StartLocation;
    };

    struct ModuleConfigurationValue
    {
        std::string Name;
        ConstantLeaf Value;

        SourceLocation StartLocation;
    };
}
