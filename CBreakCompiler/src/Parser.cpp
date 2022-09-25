// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "Parser.h"
#include "SourceTokenizer.h"
#include <unordered_set>
#include <limits>
#include <algorithm>
#include <charconv>
#include <unordered_map>

//The parser's job is primarily to convert the source file into a structure that represents the language.
//It does validation of things that are lexically invalid (like 2 binary operators in a row).
//It does not do validation of higher order concepts, such as whether 2 types are compatible or whether a referenced variable exists.

namespace
{
    using namespace CBreakCompiler;

    //A temporary piece of data used while parsing a block of code - Any pointers are no longer valid once that block is done parsing.
    struct ScopeState
    {
        ScopeState() = delete;
        ScopeState(const ScopeState&) = delete;
        ScopeState(ScopeState&&) = default;
        ScopeState(const std::string &fileName, const std::string &nameSpace, StatementsBlock &currentBlock, const ScopeState *parentScope): FileName(fileName), Namespace(nameSpace), CurrentBlock(currentBlock), ParentScope(parentScope) {}

        ScopeState& operator=(const ScopeState&) = delete;
        ScopeState& operator=(ScopeState&&) = default;

        const std::string &FileName;
        const std::string &Namespace;

        StatementsBlock &CurrentBlock;
        const ScopeState *ParentScope = nullptr;

        bool HasVariableNameInCurrentBlock(std::string_view name) const
        {
            return std::find_if(CurrentBlock.LocalVariables.begin(), CurrentBlock.LocalVariables.end(), [&](const auto &v) { return v.Name == name; }) != CurrentBlock.LocalVariables.end();
        }

        bool HasVariableNameInParentBlocks(std::string_view name) const
        {
            if (!ParentScope)
                return false;

            if (std::find_if(ParentScope->CurrentBlock.LocalVariables.begin(), ParentScope->CurrentBlock.LocalVariables.end(), [&](const auto &v) { return v.Name == name; }) != ParentScope->CurrentBlock.LocalVariables.end())
                return true;

            return ParentScope->HasVariableNameInParentBlocks(name);
        }
    };

    std::unordered_set<std::string> UnaryOperators { "!", "~" };
    std::unordered_set<std::string> SingleCharBinaryOperators { "<", ">", "*", "/", "%", "-", "+", "=", "&", "|", "^", "." };
    std::unordered_set<std::string> DoubleCharBinaryOperators { "==", "!=", "<=", ">=", "&&", "||", "^^" };

    std::unordered_map<std::string, int> OperatorPrecedence
    {
        {"!", 1},
        {"~", 1},
        {".", 2},
        {"*", 3},
        {"/", 3},
        {"%", 3},
        {"-", 4},
        {"+", 4},
        {"<", 5},
        {">", 5},
        {"<=", 5},
        {">=", 5},
        {"==", 6},
        {"!=", 6},
        {"&", 7},
        {"^", 8},
        {"|", 9},
        {"&&", 10},
        {"||", 11},
        {"^^", 12},
        {"=", 13}
    };

    void ParseTopLevel(ParsedModuleFragment &targetModule, SourceTokenizer &source, const ScopeState &scopeState);

    ConstantLeaf ParseNumericFloatLiteral(std::string_view src, SourceLocation sourceLocation)
    {
        //TODO: support literals of more than 64-bits

        double value = 0.0;
#if 0 //switch back to this once we have compiler support for this..
        std::from_chars_result result = std::from_chars(src.data(), src.data() + src.size(), value);
        if (result.ptr != src.data() + src.size())
#else
        char *end = const_cast<char*>(src.data());
        value = std::strtod(src.data(), &end);
        if (end != src.data() + src.size())
#endif
            throw ParseFailedException{.Message = "Invalid floating point literal.", .Location = sourceLocation};
        else
        {
            //TODO: actually figure out whether we can get by with float16 for something more meaningful
            int floatBits = 0;
            if (value == 0.0 || value == -1.0 || value == 1.0)
                floatBits = 16;
            else if ((double)(float)value == (double)value)
                floatBits = 32;
            else
                floatBits = 64;

            return ConstantLeaf{.IsFloat = true, .Float = value, .FloatBits = floatBits};
        }
    }

    ConstantLeaf ParseNumericIntegerLiteral(std::string_view src, SourceLocation sourceLocation)
    {
        //TODO: support literals of more than 64-bits

        size_t start = 0;
        bool isNegative = false;
        if (!src.empty() && src[0] == '-')
        {
            ++start;
            isNegative = true;
        }

        std::string digits;
        int base = 0;
        if (start + 3 < src.size() && src[start] == '0' && src[start + 1] == 'b') //binary
        {
            digits = std::string(src.begin() + start + 2, src.end());
            base = 2;
        }
        else if (start + 3 < src.size() && src[start] == '0' && src[start + 1] == 'x') //hex
        {
            digits = std::string(src.begin() + start + 2, src.end());
            base = 16;
        }
        else //decimal
        {
            digits = std::string(src.begin() + start, src.end());
            base = 10;
        }

        if (isNegative)
        {
            digits = "-" + digits;
            int64_t value = 0;
            std::from_chars_result result = std::from_chars(digits.data(), digits.data() + digits.size(), value, base);
            if (result.ptr != digits.data() + digits.size())
                throw ParseFailedException{.Message = "Invalid integer literal.", .Location = sourceLocation};

            if (value == 0)
                return ConstantLeaf{.UnsignedNumber = 0, .UnsignedBits = 1, .SignedNumber = 0, .SignedBits = 1};
            else if (value == -1)
                return ConstantLeaf{.UnsignedNumber = 0, .UnsignedBits = 0, .SignedNumber = -1, .SignedBits = 1};

            for (int bitsNeeded = 63; bitsNeeded > 0; --bitsNeeded)
            {
                if (!((uint64_t)value & ((uint64_t)1 << (bitsNeeded - 1))))
                    return ConstantLeaf{.SignedNumber = (int64_t)value, .SignedBits = bitsNeeded + 1};
            }

            return ConstantLeaf{.SignedNumber = (int64_t)value, .SignedBits = 64};
        }
        else
        {
            uint64_t value = 0;
            std::from_chars_result result = std::from_chars(digits.data(), digits.data() + digits.size(), value, base);
            if (result.ptr != digits.data() + digits.size())
                throw ParseFailedException{.Message = "Invalid integer literal.", .Location = sourceLocation};

            if (value == 0)
                return ConstantLeaf{.UnsignedNumber = 0, .UnsignedBits = 1, .SignedNumber = 0, .SignedBits = 1};

            for (int bitsNeeded = 1; bitsNeeded <= 64; ++bitsNeeded)
            {
                if (value < ((uint64_t)1 << bitsNeeded))
                    return ConstantLeaf{.UnsignedNumber = value, .UnsignedBits = bitsNeeded, .SignedNumber = (int64_t)value, .SignedBits = bitsNeeded};
            }

            return ConstantLeaf{.UnsignedNumber = value, .UnsignedBits = 64, .SignedNumber = (int64_t)value, .SignedBits = 65};
        }
    }

    ConstantLeaf ParseNumericLiteral(std::string_view src, SourceLocation sourceLocation)
    {
        if (src == "#true" || src == "#false")
        {
            return ConstantLeaf{.IsBool = true, .IsTrue = src == "#true"};
        }

        bool isFloat = false;
        for (char c : src)
            isFloat = isFloat || c == '.';

        if (isFloat)
            return ParseNumericFloatLiteral(src, sourceLocation);
        else //integer
            return ParseNumericIntegerLiteral(src, sourceLocation);
    }

    void PrecomputeLiteralMath(ExpressionNode &expr)
    {
        if (expr.LeftNode)
            PrecomputeLiteralMath(*expr.LeftNode);

        if (expr.RightNode)
            PrecomputeLiteralMath(*expr.RightNode);

        if (!expr.IsBinary && expr.Leaf.Group)
        {
            PrecomputeLiteralMath(*expr.Leaf.Group);

            if (expr.Leaf.Group->Leaf.Constant)
            {
                expr.Leaf.Constant = std::move(expr.Leaf.Group->Leaf.Constant);
                expr.Leaf.Group.reset();
            }
        }

        if (expr.IsBinary && expr.LeftNode && expr.RightNode)
        {
            if (expr.LeftNode->Leaf.Constant && expr.RightNode->Leaf.Constant)
            {
                ConstantLeaf *leftConstant = expr.LeftNode->Leaf.Constant.get();
                ConstantLeaf *rightConstant = expr.RightNode->Leaf.Constant.get();

                //TODO: booleans
                //TODO: floats
                //TODO: strings?

                //TODO: figure out overflows problems and all that stuff, and if we need to do anything special for very large unsigned numbers
                if (leftConstant->IsInteger() && rightConstant->IsInteger())
                {
                    int64_t mathResult = 0;
                    if (expr.Operation == "*")
                        mathResult = leftConstant->SignedNumber * rightConstant->SignedNumber;
                    else if (expr.Operation == "+")
                        mathResult = leftConstant->SignedNumber + rightConstant->SignedNumber;
                    else if (expr.Operation == "-")
                        mathResult = leftConstant->SignedNumber - rightConstant->SignedNumber;
                    else if (expr.Operation == "/")
                    {
                        if (rightConstant->SignedNumber == 0)
                            throw ParseFailedException{.Message = "Division by 0.", .Location = expr.StartLocation};
                        mathResult = leftConstant->SignedNumber / rightConstant->SignedNumber;
                    }
                    else if (expr.Operation == "%")
                    {
                        if (rightConstant->SignedNumber == 0)
                            throw ParseFailedException{.Message = "Division by 0.", .Location = expr.StartLocation};
                        mathResult = leftConstant->SignedNumber % rightConstant->SignedNumber;
                    }
                    else
                        return;

                    //TODO: this is dumb.. refactor the code in there out better later so we don't have to make a string here
                    std::string mathResultString = std::to_string(mathResult);
                    expr.IsBinary = false;
                    expr.Operation.clear();
                    expr.LeftNode.reset();
                    expr.RightNode.reset();
                    expr.Leaf.Constant = std::make_unique<ConstantLeaf>(ParseNumericIntegerLiteral(mathResultString, expr.StartLocation));
                    return;
                }
            }
        }

        //TODO: maybe also handle some intrinsic functions here?
    }

    enum class IdentifierDeclarationType
    {
        Global, //function, variable, or class
        Namespace,
        LocalOrArgument,
        ClassField
    };

    std::string ConsumeDeclarationIdentifier(SourceTokenizer &source, const ScopeState &scopeState, IdentifierDeclarationType declarationType)
    {
        std::string name;

        while (source.NextToken().Type != TokenType::Invalid)
        {
            if (source.NextToken().Type == TokenType::Symbol)
            {
                if (source.NextToken().String != ":")
                    break;

                source.ConsumeToken();

                if (name.empty())
                    throw ParseFailedException{.Message = "Unexpected ':'.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

                if (source.NextToken().String == ":") //namespace seperator
                {
                    if (declarationType != IdentifierDeclarationType::Namespace)
                        throw ParseFailedException{.Message = "Unexpected '::'.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

                    source.ConsumeToken();
                    name += "::";
                }
                else
                    throw ParseFailedException{.Message = "Expected additional ':' in identifier.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

                if (source.NextToken().Type != TokenType::Term)
                    throw ParseFailedException{.Message = "Expected term.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
            }
            else if (source.NextToken().Type == TokenType::Term)
            {
                if (name.empty() && declarationType == IdentifierDeclarationType::Global)
                    name = scopeState.Namespace + "::" + std::string{source.NextToken().String};
                else
                    name += source.NextToken().String;

                source.ConsumeToken();

                if (source.NextToken().Type != TokenType::Symbol)
                    break;
            }
            else
                break;
        }

        if (name.empty())
            throw ParseFailedException{.Message = "Expected declaration identifier.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

        return name;
    }

    std::string ConsumeTargetIdentifier(SourceTokenizer &source, const ScopeState &scopeState)
    {
        std::string name;

        while (source.NextToken().Type != TokenType::Invalid)
        {
            if (source.NextToken().Type == TokenType::Symbol)
            {
                bool allowedSymbol = (source.NextToken().String == ":") || (name.empty() && source.NextToken().String == ".");
                if (!allowedSymbol)
                    break;

                if (source.NextToken().String == ":")
                {
                    source.ConsumeToken();
                    if (source.NextToken().String == ":") //namespace seperator
                    {
                        source.ConsumeToken();
                        name += "::";
                    }
                    else //current namespace
                    {
                        if (!name.empty())
                            throw ParseFailedException{.Message = "Unexpected ':' or expected additional ':' in identifier.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

                        name = scopeState.Namespace + "::";
                    }
                }
                else if (source.NextToken().String == ".")
                {
                    source.ConsumeToken();
                    name += ".";
                }
                else
                    break;

                if (source.NextToken().Type != TokenType::Term)
                    throw ParseFailedException{.Message = "Expected term.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
            }
            else if (source.NextToken().Type == TokenType::Term)
            {
                name += source.NextToken().String;

                source.ConsumeToken();

                if (source.NextToken().Type != TokenType::Symbol)
                    break;
            }
            else
                break;
        }

        if (name.empty())
            throw ParseFailedException{.Message = "Expected target identifier.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

        return name;
    }

    GlobalQualifiers ConsumeOptionalGlobalQualifiers(SourceTokenizer &source, const ScopeState &scopeState)
    {
        GlobalQualifiers qualifiers;

        do
        {
            if (source.NextToken().String == "#export")
            {
                qualifiers.Export = true;
                source.ConsumeToken();
                continue;
            }
            else if (source.NextToken().String == "#cexport") //#cexport(name)
            {
                source.ConsumeToken();

                if (source.NextToken().String != "(")
                    throw ParseFailedException{.Message = "Expected '(' for qualifier argument.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
                source.ConsumeToken();

                if (source.NextToken().Type != TokenType::StringLiteral)
                    throw ParseFailedException{.Message = "Expected string literal.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
                qualifiers.CExport = std::string{source.NextToken().String};
                source.ConsumeToken();

                if (source.NextToken().String != ")")
                    throw ParseFailedException{.Message = "Expected ')'.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
                source.ConsumeToken();

                continue;
            }
            else if (source.NextToken().String == "#pack")
            {
                qualifiers.Pack = true;
                source.ConsumeToken();
                continue;
            }

        } while (false);

        return qualifiers;
    }

    IdentifierQualifiers ConsumeOptionalIdentifierQualifiers(SourceTokenizer &source, const ScopeState &scopeState)
    {
        IdentifierQualifiers qualifiers;

        do
        {
            if (source.NextToken().String == "#copy")
            {
                qualifiers.Copy = true;
                source.ConsumeToken();
                continue;
            }
            else if (source.NextToken().String == "#move")
            {
                throw ParseFailedException{.Message = "#move not implemented yet.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

                /*qualifiers.Move = true;
                source.ConsumeToken();
                continue;*/
            }
            else if (source.NextToken().String == "#truncate")
            {
                qualifiers.Truncate = true;
                source.ConsumeToken();
                continue;
            }

        } while (false);

        return qualifiers;
    }

    void ConsumeNamespace(ParsedModuleFragment &targetModule, SourceTokenizer &source, const ScopeState &originalScopeState)
    {
        //We expect the namespace keyword, followed by a possibly-multi-part namespace name, then a scope
        if (source.NextToken().String != "#namespace")
            throw ParseFailedException{.Message = "Expected '#namespace'.", .Location = SourceLocation(originalScopeState.FileName, source.NextToken().StartLocation)};
        source.ConsumeToken();

        std::string id = ConsumeDeclarationIdentifier(source, originalScopeState, IdentifierDeclarationType::Namespace);
        std::string newNamespace;
        if (originalScopeState.Namespace.empty())
            newNamespace = id;
        else
            newNamespace = originalScopeState.Namespace + "::" + id;

        ScopeState scopeState(originalScopeState.FileName, newNamespace, originalScopeState.CurrentBlock, originalScopeState.ParentScope);

        SourceTokenizer namespaceSource = source.ConsumePairedRange("{", "}");
        ParseTopLevel(targetModule, namespaceSource, scopeState);
    }

    bool TryConsumeBinaryOperator(SourceTokenizer &source, const ScopeState &scopeState, bool throwOnError, std::string &outBinaryOperator)
    {
        if (source.NextToken().Type != TokenType::Symbol)
        {
            if (throwOnError)
                throw ParseFailedException{.Message = "Expected operator symbol.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
            else
                return false;
        }

        //we need to peek at the next thing to see whether we have a two-char operator or a one-char operator
        SourceTokenizer firstSource = source;
        source.ConsumeToken();

        if (source.NextToken().Type == TokenType::Symbol && !source.NextToken().WhitespaceBefore) //could be either
        {
            outBinaryOperator = std::string{firstSource.NextToken().String} + std::string{source.NextToken().String};
            if (DoubleCharBinaryOperators.find(outBinaryOperator) != DoubleCharBinaryOperators.end()) //two-char
            {
                source.ConsumeToken();
                return true;
            }
            else //one-char
            {
                outBinaryOperator = std::string{firstSource.NextToken().String};
                if (SingleCharBinaryOperators.find(outBinaryOperator) == SingleCharBinaryOperators.end())
                {
                    if (throwOnError)
                        throw ParseFailedException{.Message = "Unknown binary operator '" + outBinaryOperator + "'.", .Location = SourceLocation(scopeState.FileName, firstSource.NextToken().StartLocation)};
                    else
                        return false;
                }

                return true;
            }
        }
        else //one-char
        {
            outBinaryOperator = std::string{firstSource.NextToken().String};
            if (SingleCharBinaryOperators.find(outBinaryOperator) == SingleCharBinaryOperators.end())
            {
                if (throwOnError)
                    throw ParseFailedException{.Message = "Unknown binary operator '" + outBinaryOperator + "'.", .Location = SourceLocation(scopeState.FileName, firstSource.NextToken().StartLocation)};
                else
                    return false;
            }

            return true;
        }
    }

    ExpressionNode ConsumeExpression(SourceTokenizer &source, const ScopeState &scopeState, bool allowEmpty, bool allowStopOnComma, bool allowBindingFunctionCall)
    {
        // pull out the top level list of "expr op expr op expr..." so we can apply operator precedence
        std::vector<ExpressionNode> expressions;
        std::vector<std::string> operations; //size of 1 less than expressions, indexed such that operations[0] is between expressions[0] and expressions[1]

        bool expectExpression = true;
        while (source.NextToken().Type != TokenType::Invalid && source.NextToken().String != ";" && (!allowStopOnComma || (allowStopOnComma && source.NextToken().String != ",")))
        {
            if (expectExpression)
            {
                IdentifierQualifiers qualifiers = ConsumeOptionalIdentifierQualifiers(source, scopeState);

                if (source.NextToken().String == "(") //could be an ordering group or a function call with binding returns
                {
                    SourceTokenizer parenSource = source.ConsumePairedRange("(", ")");

                    std::string unusedOp;
                    SourceTokenizer preConsumeSource = source;
                    if (!allowBindingFunctionCall || source.NextToken().Type == TokenType::Invalid || (TryConsumeBinaryOperator(source, scopeState, false, unusedOp))) //ordering group
                    {
                        source = preConsumeSource;

                        std::unique_ptr<ExpressionNode> group = std::make_unique<ExpressionNode>(ConsumeExpression(parenSource, scopeState, false, false, false));
                        expressions.emplace_back(ExpressionNode{.Leaf = ExpressionLeaf{.Group = std::move(group), .Qualifiers = qualifiers}, .StartLocation = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)});
                    }
                    else //function call with binding returns
                    {
                        source = preConsumeSource;

                        //Parse the binding returns first
                        std::vector<ExpressionNode> callBindingReturns;
                        while (parenSource.NextToken().Type != TokenType::Invalid)
                        {
                            callBindingReturns.emplace_back(ConsumeExpression(parenSource, scopeState, false, true, false));
                            if (parenSource.NextToken().String == ",")
                                parenSource.ConsumeToken();
                        }

                        //We'll call ConsumeExpression again here, expecting the right half of the tree to resolve to a regular function call, to which we'll bind our returns
                        StringLocation functionNameStartLocation = source.NextToken().StartLocation;
                        ExpressionNode callRoot = ConsumeExpression(source, scopeState, false, false, false);
                        ExpressionNode *cur = &callRoot;
                        while (cur->IsBinary)
                            cur = cur->RightNode.get();

                        if (!cur->Leaf.Function)
                            throw ParseFailedException{.Message = "Expected expression to resolve to a function call.", .Location = SourceLocation(scopeState.FileName, functionNameStartLocation)};

                        cur->Leaf.Function->Returns = std::move(callBindingReturns);
                        expressions.emplace_back(std::move(callRoot));
                    }
                }
                else if (source.NextToken().Type == TokenType::NumericLiteral)
                {
                    std::unique_ptr<ConstantLeaf> constant = std::make_unique<ConstantLeaf>(ParseNumericLiteral(source.NextToken().String, SourceLocation(scopeState.FileName, source.NextToken().StartLocation)));
                    expressions.emplace_back(ExpressionNode{.Leaf = ExpressionLeaf{.Constant = std::move(constant), .Qualifiers = qualifiers}, .StartLocation = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)});
                    source.ConsumeToken();
                }
                else if (source.NextToken().Type == TokenType::StringLiteral)
                {
                    std::unique_ptr<ConstantLeaf> constant = std::make_unique<ConstantLeaf>(ConstantLeaf{.IsString = true, .String = std::string{source.NextToken().String}});
                    expressions.emplace_back(ExpressionNode{.Leaf = ExpressionLeaf{.Constant = std::move(constant), .Qualifiers = qualifiers}, .StartLocation = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)});
                    source.ConsumeToken();
                }
                else if (UnaryOperators.find(std::string{source.NextToken().String}) != UnaryOperators.end())
                {
                    //TODO: handle unary operators
                    source.ConsumeToken();
                    throw ParseFailedException{.Message = "Unary operators not implemented yet.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
                }
                else //either variable or bindless function call
                {
                    StringLocation idLocation = source.NextToken().StartLocation;
                    std::string id = ConsumeTargetIdentifier(source, scopeState);

                    //if the identifier started with '.', replace it with a the binary operation for this.id
                    ExpressionNode nodeToAdd{.StartLocation = SourceLocation(scopeState.FileName, idLocation)};
                    ExpressionNode *nodeToStoreLeafIn = &nodeToAdd;
                    if (!id.empty() && id[0] == '.')
                    {
                        nodeToAdd.IsBinary = true;
                        nodeToAdd.Operation = ".";
                        nodeToAdd.LeftNode = std::make_unique<ExpressionNode>();
                        nodeToAdd.LeftNode->Leaf = ExpressionLeaf{.Variable = std::make_unique<VariableLeaf>(VariableLeaf{.Name = "#this"})};
                        nodeToAdd.RightNode = std::make_unique<ExpressionNode>();
                        nodeToStoreLeafIn = nodeToAdd.RightNode.get();

                        id = std::string(id.begin() + 1, id.end());
                        if (id.empty())
                            throw ParseFailedException{.Message = "Invalid identifier.", .Location = SourceLocation(scopeState.FileName, idLocation)};
                    }

                    if (source.NextToken().String == "(") //function call
                    {
                        SourceTokenizer parenSource = source.ConsumePairedRange("(", ")");
                        auto functionCall = std::make_unique<FunctionCallLeaf>(FunctionCallLeaf{.Name = id});
                        while (parenSource.NextToken().Type != TokenType::Invalid)
                        {
                            functionCall->Parameters.emplace_back(ConsumeExpression(parenSource, scopeState, false, true, false));
                            if (parenSource.NextToken().String == ",")
                                parenSource.ConsumeToken();
                        }

                        nodeToStoreLeafIn->Leaf = ExpressionLeaf{.Function = std::move(functionCall), .Qualifiers = qualifiers};
                    }
                    else //variable
                    {
                        nodeToStoreLeafIn->Leaf = ExpressionLeaf{.Variable = std::make_unique<VariableLeaf>(VariableLeaf{.Name = id}), .Qualifiers = qualifiers};
                    }

                    expressions.emplace_back(std::move(nodeToAdd));
                }

                if (!expressions.back().LeftNode && expressions.back().Leaf.IsEmpty())
                    throw ParseFailedException{.Message = "(internal) Generated a bad ExpressionNode.", .Location = expressions.back().StartLocation};
            }
            else //binary operator
            {
                std::string op;
                TryConsumeBinaryOperator(source, scopeState, true, op);
                operations.emplace_back(op);
            }

            expectExpression = !expectExpression;
        }

        if (expressions.empty())
        {
            if (allowEmpty)
                return ExpressionNode{.StartLocation = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
            else
                throw ParseFailedException{.Message = "Expected expression.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
        }

        if (expressions.size() == operations.size())
            throw ParseFailedException{.Message = "Expected expression after operator.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

        //combine pairs of expressions until we're down to a single expression node as the root
        while (expressions.size() > 1)
        {
            //find the leftmost operator with the best precedence to collapse next
            size_t targetOpIndex = std::numeric_limits<size_t>::max();
            int targetOperatorPrecedence = std::numeric_limits<int>::max();
            for (size_t i = 0; i < operations.size(); ++i)
            {
                int iOpPrecedence = OperatorPrecedence[operations[i]];
                if (iOpPrecedence < targetOperatorPrecedence)
                {
                    targetOpIndex = i;
                    targetOperatorPrecedence = iOpPrecedence;
                }
            }

            ExpressionNode combinedNode{.IsBinary = true, .Operation = operations[targetOpIndex], .StartLocation = expressions[targetOpIndex].StartLocation};
            combinedNode.LeftNode = std::make_unique<ExpressionNode>(std::move(expressions[targetOpIndex]));
            combinedNode.RightNode = std::make_unique<ExpressionNode>(std::move(expressions[targetOpIndex + 1]));

            expressions.erase(expressions.begin() + targetOpIndex);
            operations.erase(operations.begin() + targetOpIndex);
            expressions[targetOpIndex] = std::move(combinedNode);
        }

        PrecomputeLiteralMath(expressions.front());

        return std::move(expressions.front());
    }

    void EnsureNoChildrenVarNamesConflictWithBlock(const StatementsBlock &blockToCompareWith, const StatementsBlock &childBlockToSearch)
    {
        for (const StatementInformation &s : childBlockToSearch.Statements)
        {
            if (s.CodeBlock)
                EnsureNoChildrenVarNamesConflictWithBlock(blockToCompareWith, *s.CodeBlock);

            if (s.CodeBlockElse)
                EnsureNoChildrenVarNamesConflictWithBlock(blockToCompareWith, *s.CodeBlockElse);

            for (const VariableInformation &vc : childBlockToSearch.LocalVariables)
            {
                for (const VariableInformation &vp : blockToCompareWith.LocalVariables)
                {
                    if (vc.Name == vp.Name)
                        throw ParseFailedException{.Message = "Local variable named '" + vc.Name + "' cannot be declared because it is later declared in a parent code block.", .Location = vc.StartLocation, .OtherLocation = vp.StartLocation};
                }
            }
        }
    }

    StatementsBlock ConsumeCodeBlockStatements(FunctionInformation &functionInfo, SourceTokenizer &bodySource, const ScopeState &originalScopeState, bool singleStatementOnly)
    {
        StatementsBlock statements{.StartLocation = SourceLocation(originalScopeState.FileName, bodySource.NextToken().StartLocation)};
        ScopeState scopeState(originalScopeState.FileName, originalScopeState.Namespace, statements, &originalScopeState);

        while (bodySource.NextToken().Type != TokenType::Invalid)
        {
            StatementInformation currentStatement{.StartLocation = SourceLocation(scopeState.FileName, bodySource.NextToken().StartLocation)};
            bool expectTrailingSemicolon = true;

            if (bodySource.NextToken().String == "#return") //return
            {
                bodySource.ConsumeToken();

                if (bodySource.NextToken().String != ";") //single-value return
                {
                    if (functionInfo.Returns.size() == 0)
                        throw ParseFailedException{.Message = "Expected #return statement to be empty.", .Location = SourceLocation(scopeState.FileName, bodySource.NextToken().StartLocation)};
                    else if (functionInfo.Returns.size() > 1) //TODO: allow something like "return x,y"
                        throw ParseFailedException{.Message = "Multi-value #return statement not yet supported.", .Location = SourceLocation(scopeState.FileName, bodySource.NextToken().StartLocation)};

                    //generate an assignment statement and add it now, then let currentStatement be the normal return
                    StatementInformation returnAssignStatement{.StartLocation = currentStatement.StartLocation};
                    returnAssignStatement.Type = StatementType::EvaluateOnly;

                    std::unique_ptr<ExpressionNode> leftExpr = std::make_unique<ExpressionNode>();
                    leftExpr->IsBinary = false;
                    leftExpr->StartLocation = currentStatement.StartLocation;
                    leftExpr->Leaf.Variable = std::make_unique<VariableLeaf>();
                    leftExpr->Leaf.Variable->Name = functionInfo.Returns[0].Name;

                    std::unique_ptr<ExpressionNode> rightExpr = std::make_unique<ExpressionNode>();
                    *rightExpr = ConsumeExpression(bodySource, scopeState, false, false, false);

                    returnAssignStatement.Expression = std::make_unique<ExpressionNode>();
                    returnAssignStatement.Expression->IsBinary = true;
                    returnAssignStatement.Expression->Operation = "=";
                    returnAssignStatement.Expression->StartLocation = returnAssignStatement.StartLocation;
                    returnAssignStatement.Expression->LeftNode = std::move(leftExpr);
                    returnAssignStatement.Expression->RightNode = std::move(rightExpr);

                    statements.Statements.emplace_back(std::move(returnAssignStatement));
                }

                currentStatement.Type = StatementType::Return;
            }
            else if (bodySource.NextToken().String == "#var" || bodySource.NextToken().String == "#const") //variable or constant declaration
            {
                bool isConst = bodySource.NextToken().String == "#const";
                bodySource.ConsumeToken();
                currentStatement.Type = StatementType::VariableDeclaration;

                GlobalQualifiers qualifiers = ConsumeOptionalGlobalQualifiers(bodySource, scopeState);
                std::string varType = ConsumeTargetIdentifier(bodySource, scopeState);
                StringLocation varSourceLocation = bodySource.NextToken().StartLocation;
                std::string varId = ConsumeDeclarationIdentifier(bodySource, scopeState, IdentifierDeclarationType::LocalOrArgument);

                currentStatement.VariableName = varId;

                if (scopeState.HasVariableNameInCurrentBlock(varId))
                    throw ParseFailedException{.Message = "Local variable named '" + varId + "' cannot be declared twice in the same code block.", .Location = SourceLocation(scopeState.FileName, varSourceLocation)};

                if (scopeState.HasVariableNameInParentBlocks(varId))
                    throw ParseFailedException{.Message = "Local variable named '" + varId + "' cannot be declared because it is already declared in a parent code block.", .Location = SourceLocation(scopeState.FileName, varSourceLocation)};

                if (std::find_if(functionInfo.Returns.begin(), functionInfo.Returns.end(), [&](const auto &v) { return v.Name == varId; }) != functionInfo.Returns.end())
                    throw ParseFailedException{.Message = "Local variable '" + varId + "' may not be named the same as a function return argument.", .Location = SourceLocation(scopeState.FileName, varSourceLocation)};

                if (std::find_if(functionInfo.Parameters.begin(), functionInfo.Parameters.end(), [&](const auto &v) { return v.Name == varId; }) != functionInfo.Parameters.end())
                    throw ParseFailedException{.Message = "Local variable '" + varId + "' may not be named the same as a function parameter argument.", .Location = SourceLocation(scopeState.FileName, varSourceLocation)};

                std::unique_ptr<ExpressionNode> initializer = std::make_unique<ExpressionNode>();
                if (bodySource.NextToken().String == "=")
                {
                    bodySource.ConsumeToken();
                    *initializer = ConsumeExpression(bodySource, scopeState, false, false, false);
                }
                else if (isConst)
                    throw ParseFailedException{.Message = "Local const '" + varId + "' must be initialized.", .Location = SourceLocation(scopeState.FileName, varSourceLocation)};

                statements.LocalVariables.emplace_back(VariableInformation{.Type = varType, .Name = varId, .IsConst = isConst, .Initializer = std::move(initializer), .StartLocation = SourceLocation(scopeState.FileName, varSourceLocation)});
            }
            else if (bodySource.NextToken().String == "{") //child statement block
            {
                expectTrailingSemicolon = false;
                SourceTokenizer childSource = bodySource.ConsumePairedRange("{", "}");
                currentStatement.Type = StatementType::ChildBlock;
                currentStatement.CodeBlock = std::make_unique<StatementsBlock>(ConsumeCodeBlockStatements(functionInfo, childSource, scopeState, false));
            }
            else if (bodySource.NextToken().String == "#if") //if conditional
            {
                expectTrailingSemicolon = false;
                bodySource.ConsumeToken();
                SourceTokenizer conditionalSource = bodySource.ConsumePairedRange("(", ")");

                currentStatement.Type = StatementType::ConditionalIfElse;
                currentStatement.Expression = std::make_unique<ExpressionNode>();
                *currentStatement.Expression = ConsumeExpression(conditionalSource, scopeState, false, false, false);
                currentStatement.CodeBlock = std::make_unique<StatementsBlock>(ConsumeCodeBlockStatements(functionInfo, bodySource, scopeState, true));

                if (bodySource.NextToken().String == "#else") //an else statement exists to pair with the if statement
                {
                    bodySource.ConsumeToken();
                    currentStatement.CodeBlockElse = std::make_unique<StatementsBlock>(ConsumeCodeBlockStatements(functionInfo, bodySource, scopeState, true));
                }
            }
            else //general expression
            {
                currentStatement.Expression = std::make_unique<ExpressionNode>();
                *currentStatement.Expression = ConsumeExpression(bodySource, scopeState, true, false, true);
                currentStatement.Type = StatementType::EvaluateOnly;
            }

            //eat the trailing ;
            if (expectTrailingSemicolon && bodySource.NextToken().String != ";")
                throw ParseFailedException{.Message = "Expected ';'", .Location = SourceLocation(scopeState.FileName, bodySource.NextToken().StartLocation)};
            else if (expectTrailingSemicolon)
                bodySource.ConsumeToken();

            statements.Statements.emplace_back(std::move(currentStatement));

            if (singleStatementOnly)
                break;
        }

        //Our previous variable declaration checks prevent duplicate names in the same block, and duplicates between a child block and any parent blocks that are parsed up to the point of the child block we're in.
        //W need to prevent a variable declared in a child block from using the same name as a variable in any parent block, even if it was declared after the block.  Perhaps later we can consider dealing with this and allow it.
        //Since this function is already recurvise, we can accomplish this by only checking whether all children (which are finished parsing) conflict with our current scope (now also finished).
        for (const StatementInformation &s : statements.Statements)
        {
            if (s.CodeBlock)
                EnsureNoChildrenVarNamesConflictWithBlock(statements, *s.CodeBlock);

            if (s.CodeBlockElse)
                EnsureNoChildrenVarNamesConflictWithBlock(statements, *s.CodeBlockElse);
        }

        return statements;
    }

    void ConsumeFunctionBody(FunctionInformation &functionInfo, SourceTokenizer &bodySource, const ScopeState &scopeState)
    {
        functionInfo.CodeBlock = ConsumeCodeBlockStatements(functionInfo, bodySource, scopeState, false);
    }

    //TODO: makeConst is sort of a kludge
    void ConsumeFunctionArguments(std::vector<VariableInformation> &target, SourceTokenizer &source, const ScopeState &scopeState, bool makeConst, std::string defaultArgPrefix = std::string())
    {
        int argIndex = 0;
        while (source.NextToken().Type != TokenType::Invalid)
        {
            //type
            std::string argType = ConsumeTargetIdentifier(source, scopeState);
            StringLocation argSourceLocation = source.NextToken().StartLocation;

            //default or provided name
            std::string argId = defaultArgPrefix + std::to_string(argIndex);
            if (source.NextToken().Type != TokenType::Invalid && source.NextToken().String != ",")
                argId = ConsumeDeclarationIdentifier(source, scopeState, IdentifierDeclarationType::LocalOrArgument);

            target.emplace_back(VariableInformation{.Type = argType, .Name = argId, .IsConst = makeConst, .StartLocation = SourceLocation(scopeState.FileName, argSourceLocation)});

            //next arg
            if (source.NextToken().Type != TokenType::Invalid)
            {
                if (source.NextToken().String != ",")
                    throw ParseFailedException{.Message = "Expected ',' or end of function arguments.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
                source.ConsumeToken();

                if (source.NextToken().Type == TokenType::Invalid)
                    throw ParseFailedException{.Message = "Expected next function argument.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
            }

            ++argIndex;
        }
    }

    void ConsumeFunction(std::vector<FunctionInformation> &target, SourceTokenizer &source, const ScopeState &scopeState, const GlobalQualifiers qualifiers, const std::string &inClass)
    {
        FunctionInformation functionInfo;
        functionInfo.Qualifiers = qualifiers;

        //Return arguments - these need to be pointers so we can store to them
        if (source.NextToken().String != "(")
            throw ParseFailedException{.Message = "Expected '(' to start function return arguments.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

        SourceTokenizer parameterSource = source.ConsumePairedRange("(", ")");
        ConsumeFunctionArguments(functionInfo.Returns, parameterSource, scopeState, false, "#defaultreturn");
        for (VariableInformation &vi : functionInfo.Returns)
            vi.Type += "*";

        if (!qualifiers.CExport.empty() && functionInfo.Returns.size() > 1)
            throw ParseFailedException{.Message = "Functions qualified with #cexport may not have more than one return value.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

        //Function name - if it's a special member function we mangle it as a workaround for not supporting overloads
        bool isSpecialMember = source.NextToken().String.size() >= 1 && source.NextToken().String[0] == '#';
        std::string functionId = (isSpecialMember ? ConsumeTargetIdentifier(source, scopeState) : ConsumeDeclarationIdentifier(source, scopeState, !inClass.empty() ? IdentifierDeclarationType::ClassField : IdentifierDeclarationType::Global));
        if (isSpecialMember)
        {
            if (functionInfo.Returns.size() != 1)
                throw ParseFailedException{.Message = "Expected 1 return argument for special member function.", .Location = SourceLocation(scopeState.FileName, parameterSource.NextToken().StartLocation)};

            if (functionId == "#autocopy" || functionId == "#copy" || functionId == "#move" || functionId == "#truncate")
                functionId += "#" + functionInfo.Returns[0].Type;
            else
                throw ParseFailedException{.Message = "Unknown special member function.", .Location = SourceLocation(scopeState.FileName, parameterSource.NextToken().StartLocation)};
        }

        functionInfo.Name = functionId;

        //Parameter arguments
        if (!inClass.empty()) //classes have an implicit this pointer as the first argument
            functionInfo.Parameters.emplace_back(VariableInformation{.Type = inClass + "*", .Name = "#this", .IsConst = true, .StartLocation = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)});

        if (source.NextToken().String != "(")
            throw ParseFailedException{.Message = "Expected '(' to start function parameter arguments.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

        parameterSource = source.ConsumePairedRange("(", ")");
        ConsumeFunctionArguments(functionInfo.Parameters, parameterSource, scopeState, true, "#defaultparam");

        if (isSpecialMember && functionInfo.Parameters.size() != (inClass.empty() ? 0 : 1))
            throw ParseFailedException{.Message = "Did not expect arguments for special member function.", .Location = SourceLocation(scopeState.FileName, parameterSource.NextToken().StartLocation)};

        //Our scope and the function body
        SourceTokenizer bodySource = source.ConsumePairedRange("{", "}");
        ConsumeFunctionBody(functionInfo, bodySource, scopeState);

        target.emplace_back(std::move(functionInfo));
    }

    void ConsumeVariable(std::vector<VariableInformation> &target, SourceTokenizer &source, const ScopeState &scopeState, const GlobalQualifiers qualifiers, bool inClass)
    {
        bool isConst = source.NextToken().String == "#const";
        source.ConsumeToken();

        std::string type = ConsumeTargetIdentifier(source, scopeState);
        StringLocation startLocation = source.NextToken().StartLocation;
        std::string id = ConsumeDeclarationIdentifier(source, scopeState, inClass ? IdentifierDeclarationType::ClassField : IdentifierDeclarationType::Global);

        std::unique_ptr<ExpressionNode> initializer = std::make_unique<ExpressionNode>();
        if (source.NextToken().String == "=")
        {
            source.ConsumeToken();

            if (source.NextToken().Type == TokenType::NumericLiteral)
            {
                std::unique_ptr<ConstantLeaf> constant = std::make_unique<ConstantLeaf>(ParseNumericLiteral(source.NextToken().String, SourceLocation(scopeState.FileName, source.NextToken().StartLocation)));
                initializer = std::make_unique<ExpressionNode>(ExpressionNode{.Leaf = ExpressionLeaf{.Constant = std::move(constant)}, .StartLocation = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)});
                source.ConsumeToken();
            }
            else if (source.NextToken().Type == TokenType::StringLiteral)
            {
                std::unique_ptr<ConstantLeaf> constant = std::make_unique<ConstantLeaf>(ConstantLeaf{.IsString = true, .String = std::string{source.NextToken().String}});
                initializer = std::make_unique<ExpressionNode>(ExpressionNode{.Leaf = ExpressionLeaf{.Constant = std::move(constant)}, .StartLocation = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)});
                source.ConsumeToken();
            }
            else
                throw ParseFailedException{.Message = "Expected literal.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
        }
        else if (isConst)
            throw ParseFailedException{.Message = "Global const '" + id + "' must be initialized.", .Location = SourceLocation(scopeState.FileName, startLocation)};

        if (source.NextToken().String != ";") //variable
            throw ParseFailedException{.Message = "Expected ';'.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};
        source.ConsumeToken();

        target.emplace_back(VariableInformation{.Type = type, .Name = id, .IsConst = isConst, .Qualifiers = qualifiers, .Initializer = std::move(initializer), .StartLocation = SourceLocation(scopeState.FileName, startLocation)});
    }

    void ConsumeClass(std::vector<ClassInformation> &target, SourceTokenizer &source, const ScopeState &scopeState, const GlobalQualifiers qualifiers)
    {
        source.ConsumeToken();

        StringLocation startLocation = source.NextToken().StartLocation;
        std::string id = ConsumeDeclarationIdentifier(source, scopeState, IdentifierDeclarationType::Global);

        ClassInformation classInfo{.Name = id, .Qualifiers = qualifiers, .StartLocation = SourceLocation(scopeState.FileName, startLocation)};

        SourceTokenizer classSource = source.ConsumePairedRange("{", "}");
        while (classSource.NextToken().Type != TokenType::Invalid)
        {
            GlobalQualifiers qualifiers = ConsumeOptionalGlobalQualifiers(classSource, scopeState);
            //TODO validate qualifiers are even sensible here

            if (classSource.NextToken().String == "#var")
            {
                ConsumeVariable(classInfo.Variables, classSource, scopeState, qualifiers, true);
            }
            else //function
            {
                ConsumeFunction(classInfo.Functions, classSource, scopeState, qualifiers, id);
            }
        }

        target.emplace_back(std::move(classInfo));
    }

    void ConsumeConfiguration(ParsedModuleFragment &target, const ScopeState &scopeState, SourceTokenizer &source)
    {
        source.ConsumeToken();

        SourceTokenizer configurationSource = source.ConsumePairedRange("{", "}");
        while (configurationSource.NextToken().Type != TokenType::Invalid)
        {
            ModuleConfigurationValue mcv{.StartLocation = SourceLocation(scopeState.FileName, configurationSource.NextToken().StartLocation)};

            mcv.Name = std::string{configurationSource.NextToken().String};
            if (mcv.Name.empty() || mcv.Name[0] != '#')
                throw ParseFailedException{.Message = "Invalid name encountered in #configuration.", .Location = SourceLocation(scopeState.FileName, configurationSource.NextToken().StartLocation)};
            configurationSource.ConsumeToken();

            if (configurationSource.NextToken().Type == TokenType::NumericLiteral)
                mcv.Value = ParseNumericLiteral(configurationSource.NextToken().String, SourceLocation(scopeState.FileName, configurationSource.NextToken().StartLocation));
            else if (configurationSource.NextToken().Type == TokenType::StringLiteral)
                mcv.Value = ConstantLeaf{.IsString = true, .String = std::string{configurationSource.NextToken().String}};
            else
                throw ParseFailedException{.Message = "Expected literal.", .Location = SourceLocation(scopeState.FileName, configurationSource.NextToken().StartLocation)};
            configurationSource.ConsumeToken();

            if (configurationSource.NextToken().String != ";")
                throw ParseFailedException{.Message = "Expected ';'", .Location = SourceLocation(scopeState.FileName, configurationSource.NextToken().StartLocation)};
            configurationSource.ConsumeToken();

            target.Configurations.emplace_back(std::move(mcv));
        }
    }

    void ParseTopLevel(ParsedModuleFragment &targetModule, SourceTokenizer &source, const ScopeState &scopeState)
    {
        for (; source.NextToken().Type != TokenType::Invalid; )
        {
            GlobalQualifiers qualifiers = ConsumeOptionalGlobalQualifiers(source, scopeState);

            if (source.NextToken().String == "#namespace")
            {
                if (!qualifiers.IsEmpty())
                    throw ParseFailedException{.Message = "Qualifiers not allowed on #namespace.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

                ConsumeNamespace(targetModule, source, scopeState);
            }
            else if (source.NextToken().String == "#var" || source.NextToken().String == "#const")
            {
                ConsumeVariable(targetModule.GlobalVariables, source, scopeState, qualifiers, false);
            }
            else if (source.NextToken().String == "#class")
            {
                ConsumeClass(targetModule.GlobalClasses, source, scopeState, qualifiers);
            }
            else if (source.NextToken().String == "#configuration")
            {
                if (!qualifiers.IsEmpty())
                    throw ParseFailedException{.Message = "Qualifiers not allowed on #configuration.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

                if (!scopeState.Namespace.empty())
                    throw ParseFailedException{.Message = "#configuration must be in the root namespace.", .Location = SourceLocation(scopeState.FileName, source.NextToken().StartLocation)};

                ConsumeConfiguration(targetModule, scopeState, source);
            }
            else //function
            {
                ConsumeFunction(targetModule.GlobalFunctions, source, scopeState, qualifiers, std::string());
            }
        }
    }
}

namespace CBreakCompiler
{
    ParsedModuleFragment ParseSource(std::string_view sourceText, std::string_view fileNameForDiagnostics)
    {
        ParsedModuleFragment targetModule{.FileName = std::string{fileNameForDiagnostics}};
        try
        {
            SourceTokenizer source{sourceText, StringLocation()};
            StatementsBlock dummyStatements{.StartLocation = SourceLocation(std::string(fileNameForDiagnostics), StringLocation())};
            ScopeState scopeState(std::string(fileNameForDiagnostics), std::string(), dummyStatements, nullptr);
            ParseTopLevel(targetModule, source, scopeState);
        }
        catch (ParseFailedException &pfe)
        {
            //things thrown by SourceTokenizer won't have a filename, so fill that in.
            pfe.Location.FileName = fileNameForDiagnostics;
            throw pfe;
        }

        return targetModule;
    }
}
