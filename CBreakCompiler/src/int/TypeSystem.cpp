// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "ModuleCompilerInternal.h"
#include <unordered_set>

namespace
{
    using namespace CBreakCompiler;
    using namespace CBreakCompilerInternal;

    std::unordered_map<int, std::pair<std::string, std::string>> DefaultIntBitsTypes
    {
        {8, {"#int8", "#uint8"}},
        {16, {"#int16", "#uint16"}},
        {32, {"#int32", "#uint32"}},
        {64, {"#int64", "#uint64"}},
        {128, {"#int128", "#uint128"}}
    };

    std::unordered_map<int, std::string> DefaultFloatBitsTypes
    {
        {16, "#float16"},
        {32, "#float32"},
        {64, "#float64"},
        {128, "#float128"}
    };

    std::unordered_map<std::string, int> BuiltInSignedIntTypeBits
    {
        {"#int8", 8},
        {"#int16", 16},
        {"#int32", 32},
        {"#int64", 64},
        {"#int128", 128}
    };

    std::unordered_map<std::string, int> BuiltInUnsignedIntTypeBits
    {
        {"#uint8", 8},
        {"#uint16", 16},
        {"#uint32", 32},
        {"#uint64", 64},
        {"#uint128", 128}
    };

    std::unordered_map<std::string, int> BuiltInIntTypeBits
    {
        {"#int8", 8},
        {"#int16", 16},
        {"#int32", 32},
        {"#int64", 64},
        {"#int128", 128},
        {"#uint8", 8},
        {"#uint16", 16},
        {"#uint32", 32},
        {"#uint64", 64},
        {"#uint128", 128}
    };

    std::unordered_map<std::string, int> BuiltInFloatTypeBits
    {
        {"#float16", 16},
        {"#float32", 32},
        {"#float64", 64},
        {"#float128", 128}
    };

    std::unordered_set<std::string> mathBinaryOperatorsThatResultInBool { "==", "!=", "<", "<=", ">", ">=" };

    void AddSimpleType(TypeSystem &types, const std::string &typeName, llvm::Type *irType, const SourceLocation &definedSourceLocation)
    {
        TypeDetails td{.ClassName = typeName, .Name = typeName, .IRType = irType, .StartLocation = definedSourceLocation};

        types.AddType(std::move(td));
    }

    bool TryReinterpretLiteral(TypeSystem &types, const ConstantLeaf &literal, const TypeDetails &targetType, llvm::IRBuilder<> &irBuilder, const SourceLocation &sourceLocation, EvaluationResult &result)
    {
        //if the target type isn't integral, there's nothing to do here
        auto sIntBitIter = BuiltInSignedIntTypeBits.find(targetType.Name);
        auto uIntBitIter = BuiltInUnsignedIntTypeBits.find(targetType.Name);
        int targetSignedBits = sIntBitIter != BuiltInSignedIntTypeBits.end() ? sIntBitIter->second : 0;
        int targetUnsignedBits = uIntBitIter != BuiltInUnsignedIntTypeBits.end() ? uIntBitIter->second : 0;

        if (targetSignedBits == 0 && targetUnsignedBits == 0)
            return false;

        //
        if (targetUnsignedBits)
        {
            if (literal.UnsignedBits != 0 && literal.UnsignedBits <= targetUnsignedBits)
            {
                result = types.EvaluateLiteral(literal, sourceLocation, &types.GetTypeDetails("#uint" + std::to_string(targetUnsignedBits), sourceLocation));
                return true;
            }
            else
                return false;
        }
        else //targetSignedBits
        {
            if (literal.SignedBits != 0 && literal.SignedBits <= targetSignedBits)
            {
                result = types.EvaluateLiteral(literal, sourceLocation, &types.GetTypeDetails("#int" + std::to_string(targetSignedBits), sourceLocation));
                return true;
            }
            else
                return false;
        }
    }

    std::function<llvm::Value*(llvm::Value*)> TryGetBuiltInConvertFunction(const TypeDetails &classType, const TypeDetails &convertToType, llvm::IRBuilder<> &irBuilder)
    {
        if (convertToType.ClassName == "#int128" && (classType.ClassName == "#int8" || classType.ClassName == "#int16" || classType.ClassName == "#int32" || classType.ClassName == "#int64"))
            return [&](llvm::Value *from){ return irBuilder.CreateSExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#int64" && (classType.ClassName == "#int8" || classType.ClassName == "#int16" || classType.ClassName == "#int32"))
            return [&](llvm::Value *from){ return irBuilder.CreateSExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#int32" && (classType.ClassName == "#int8" || classType.ClassName == "#int16"))
            return [&](llvm::Value *from){ return irBuilder.CreateSExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#int16" && classType.ClassName == "#int8")
            return [&](llvm::Value *from){ return irBuilder.CreateSExt(from, convertToType.IRType, "#convert"); };

        else if (convertToType.ClassName == "#uint128" && (classType.ClassName == "#uint8" || classType.ClassName == "#uint16" || classType.ClassName == "#uint32" || classType.ClassName == "#uint64"))
            return [&](llvm::Value *from){ return irBuilder.CreateZExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#uint64" && (classType.ClassName == "#uint8" || classType.ClassName == "#uint16" || classType.ClassName == "#uint32"))
            return [&](llvm::Value *from){ return irBuilder.CreateZExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#uint32" && (classType.ClassName == "#uint8" || classType.ClassName == "#uint16"))
            return [&](llvm::Value *from){ return irBuilder.CreateZExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#uint16" && classType.ClassName == "#uint8")
            return [&](llvm::Value *from){ return irBuilder.CreateZExt(from, convertToType.IRType, "#convert"); };

        else if (convertToType.ClassName == "#int128" && (classType.ClassName == "#uint8" || classType.ClassName == "#uint16" || classType.ClassName == "#uint32" || classType.ClassName == "#uint64"))
            return [&](llvm::Value *from){ return irBuilder.CreateZExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#int64" && (classType.ClassName == "#uint8" || classType.ClassName == "#uint16" || classType.ClassName == "#uint32"))
            return [&](llvm::Value *from){ return irBuilder.CreateZExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#int32" && (classType.ClassName == "#uint8" || classType.ClassName == "#uint16"))
            return [&](llvm::Value *from){ return irBuilder.CreateZExt(from, convertToType.IRType, "#convert"); };
        else if (convertToType.ClassName == "#int16" && classType.ClassName == "#uint8")
            return [&](llvm::Value *from){ return irBuilder.CreateZExt(from, convertToType.IRType, "#convert"); };

        return std::function<llvm::Value*(llvm::Value*)>();
    }

    /*FunctionDetails* TryGetSpecialConvertFunction(const TypeDetails &classType, const TypeDetails &convertToType)
    {
        //TODO
        return nullptr;
    }*/

    std::function<llvm::Value*(llvm::Value*)> TryGetBuiltInTruncateFunction(const TypeDetails &classType, const TypeDetails &convertToType, llvm::IRBuilder<> &irBuilder)
    {
        //if both types aren't integral, there's nothing to do here
        int sourceBits = 0;
        auto bitsIter = BuiltInIntTypeBits.find(classType.Name);
        if (bitsIter != BuiltInIntTypeBits.end())
            sourceBits = bitsIter->second;

        int destBits = 0;
        bitsIter = BuiltInIntTypeBits.find(convertToType.Name);
        if (bitsIter != BuiltInIntTypeBits.end())
            destBits = bitsIter->second;

        if (sourceBits == 0 || destBits == 0)
            return std::function<llvm::Value*(llvm::Value*)>();

        //we only need to truncates when moving smaller, otherwise just reflect the original value
        if (destBits < sourceBits)
            return [&](llvm::Value *from){ return irBuilder.CreateTrunc(from, convertToType.IRType, "#truncate"); };
        else
            return [&](llvm::Value *from){ return from; };

        return std::function<llvm::Value*(llvm::Value*)>();
    }

    /*FunctionDetails* TryGetSpecialTruncateFunction(const TypeDetails &classType, const TypeDetails &convertToType)
    {
        //TODO:
        return nullptr;
    }*/

    bool TryExecuteBuiltInBinaryOperator(TypeSystem &types, const std::string &operation, const EvaluationResult &leftResult, const EvaluationResult &rightResult, const TypeDetails &operandType, llvm::IRBuilder<> &irBuilder, const SourceLocation &sourceLocation, EvaluationResult &result)
    {
        bool isSInt = BuiltInSignedIntTypeBits.find(operandType.Name) != BuiltInSignedIntTypeBits.end();
        bool isUInt = BuiltInUnsignedIntTypeBits.find(operandType.Name) != BuiltInUnsignedIntTypeBits.end();
        bool isFloat = BuiltInFloatTypeBits.find(operandType.Name) != BuiltInFloatTypeBits.end();

        llvm::Value *resultVal = nullptr;
        if (isSInt || isUInt)
        {
            if (operation == "+")
                resultVal = irBuilder.CreateAdd(leftResult.IRValue, rightResult.IRValue, "BinOpPlus");
            else if (operation == "-")
                resultVal = irBuilder.CreateSub(leftResult.IRValue, rightResult.IRValue, "BinOpMinus");
            else if (operation == "*")
                resultVal = irBuilder.CreateMul(leftResult.IRValue, rightResult.IRValue, "BinOpMul");
            else if (operation == "==")
                resultVal = irBuilder.CreateICmpEQ(leftResult.IRValue, rightResult.IRValue, "BinOpEq");
            else if (operation == "!=")
                resultVal = irBuilder.CreateICmpNE(leftResult.IRValue, rightResult.IRValue, "BinOpNe");

            if (isSInt)
            {
                if (operation == "/")
                    resultVal = irBuilder.CreateSDiv(leftResult.IRValue, rightResult.IRValue, "BinOpDiv");
                else if (operation == "%")
                    resultVal = irBuilder.CreateSRem(leftResult.IRValue, rightResult.IRValue, "BinOpRem");
                else if (operation == "<")
                    resultVal = irBuilder.CreateICmpSLT(leftResult.IRValue, rightResult.IRValue, "BinOpLt");
                else if (operation == "<=")
                    resultVal = irBuilder.CreateICmpSLE(leftResult.IRValue, rightResult.IRValue, "BinOpLe");
                else if (operation == ">")
                    resultVal = irBuilder.CreateICmpSGT(leftResult.IRValue, rightResult.IRValue, "BinOpGt");
                else if (operation == ">=")
                    resultVal = irBuilder.CreateICmpSGE(leftResult.IRValue, rightResult.IRValue, "BinOpGe");
            }
            else //isUInt
            {
                if (operation == "/")
                    resultVal = irBuilder.CreateUDiv(leftResult.IRValue, rightResult.IRValue, "BinOpDiv");
                else if (operation == "%")
                    resultVal = irBuilder.CreateURem(leftResult.IRValue, rightResult.IRValue, "BinOpRem");
                else if (operation == "<")
                    resultVal = irBuilder.CreateICmpULT(leftResult.IRValue, rightResult.IRValue, "BinOpLt");
                else if (operation == "<=")
                    resultVal = irBuilder.CreateICmpULE(leftResult.IRValue, rightResult.IRValue, "BinOpLe");
                else if (operation == ">")
                    resultVal = irBuilder.CreateICmpUGT(leftResult.IRValue, rightResult.IRValue, "BinOpGt");
                else if (operation == ">=")
                    resultVal = irBuilder.CreateICmpUGE(leftResult.IRValue, rightResult.IRValue, "BinOpGe");
            }
        }
        else if (isFloat)
        {
            if (operation == "+")
                resultVal = irBuilder.CreateFAdd(leftResult.IRValue, rightResult.IRValue, "BinOpPlus");
            else if (operation == "-")
                resultVal = irBuilder.CreateFSub(leftResult.IRValue, rightResult.IRValue, "BinOpMinus");
            else if (operation == "*")
                resultVal = irBuilder.CreateFMul(leftResult.IRValue, rightResult.IRValue, "BinOpMul");
            else if (operation == "/")
                resultVal = irBuilder.CreateFDiv(leftResult.IRValue, rightResult.IRValue, "BinOpDiv");
            else if (operation == "%")
                resultVal = irBuilder.CreateFRem(leftResult.IRValue, rightResult.IRValue, "BinOpRem");
            else if (operation == "==")
                resultVal = irBuilder.CreateFCmpUEQ(leftResult.IRValue, rightResult.IRValue, "BinOpEq");
            else if (operation == "!=")
                resultVal = irBuilder.CreateFCmpUNE(leftResult.IRValue, rightResult.IRValue, "BinOpNe");
            else if (operation == "<")
                resultVal = irBuilder.CreateFCmpULT(leftResult.IRValue, rightResult.IRValue, "BinOpLt");
            else if (operation == "<=")
                resultVal = irBuilder.CreateFCmpULE(leftResult.IRValue, rightResult.IRValue, "BinOpLe");
            else if (operation == ">")
                resultVal = irBuilder.CreateFCmpUGT(leftResult.IRValue, rightResult.IRValue, "BinOpGt");
            else if (operation == ">=")
                resultVal = irBuilder.CreateFCmpUGE(leftResult.IRValue, rightResult.IRValue, "BinOpGe");
        }

        if (resultVal)
        {
            if (mathBinaryOperatorsThatResultInBool.find(operation) != mathBinaryOperatorsThatResultInBool.end())
                result = EvaluationResult{.Type = &types.GetTypeDetails("#bool", sourceLocation), .IRValue = resultVal, .IsConst = true, .NeedsDereferenceForValue = false};
            else
                result = EvaluationResult{.Type = &operandType, .IRValue = resultVal, .IsConst = true, .NeedsDereferenceForValue = false};

            return true;
        }

        return false;
    }
}

namespace CBreakCompilerInternal
{
    using namespace CBreakCompiler;

    TypeSystem::TypeSystem(ModuleCompilerInternal &compiler):
        compiler(compiler)
    {
    }

    void TypeSystem::CreateBuiltInTypes()
    {
        SourceLocation builtInLocation("(built-in)", StringLocation{.Line = -1, .Column = -1});

        AddSimpleType(*this, "#void", llvm::Type::getVoidTy(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#bool", llvm::Type::getInt1Ty(compiler.LLVMContext), builtInLocation);

        AddSimpleType(*this, "#int8", llvm::Type::getInt8Ty(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#uint8", llvm::Type::getInt8Ty(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#int16", llvm::Type::getInt16Ty(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#uint16", llvm::Type::getInt16Ty(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#int32", llvm::Type::getInt32Ty(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#uint32", llvm::Type::getInt32Ty(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#int64", llvm::Type::getInt64Ty(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#uint64", llvm::Type::getInt64Ty(compiler.LLVMContext), builtInLocation);

        AddSimpleType(*this, "#float16", llvm::Type::getHalfTy(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#float32", llvm::Type::getFloatTy(compiler.LLVMContext), builtInLocation);
        AddSimpleType(*this, "#float64", llvm::Type::getDoubleTy(compiler.LLVMContext), builtInLocation);

        //map built-in type aliases
        auto defaultIntMap = DefaultIntBitsTypes[compiler.Config.DefaultIntBits];
        TypeDetails intCopy = GetTypeDetails(defaultIntMap.first, builtInLocation);
        AllTypes.emplace("#int", intCopy);
        TypeDetails uintCopy = GetTypeDetails(defaultIntMap.second, builtInLocation);
        AllTypes.emplace("#uint", uintCopy);

        auto defaultFloatMap = DefaultFloatBitsTypes[compiler.Config.DefaultFloatBits];
        TypeDetails floatCopy = GetTypeDetails(defaultFloatMap, builtInLocation);
        AllTypes.emplace("#int", floatCopy);
    }

    void TypeSystem::AddType(TypeDetails &&newType)
    {
        auto i = AllTypes.find(newType.Name);
        if (i != AllTypes.end())
            throw CodeGenerationFailedException{.Message = "Attempt to redefine type '" + newType.Name + "'.", .Location = newType.StartLocation, .OtherLocation = i->second.StartLocation};

        AllTypes.emplace(newType.Name, std::move(newType));
    }

    TypeDetails& TypeSystem::GetTypeDetails(const std::string &typeName, const SourceLocation &referencedSourceLocation)
    {
        TypeDetails *td = TryGetTypeDetails(typeName);

        if (!td)
            throw CodeGenerationFailedException{.Message = "Use of unknown type '" + typeName + "'.", .Location = referencedSourceLocation};

        return *td;
    }

    TypeDetails* TypeSystem::TryGetTypeDetails(const std::string &typeName)
    {
        //we may already have it cached
        auto i = AllTypes.find(typeName);
        if (i != AllTypes.end())
            return &i->second;

        //if this is a pointer type, resolve that from the base type and cache it, otherwise it's just something we don't know about
        int pointerDepth = 0;
        std::string baseTypeName = typeName;
        while (!baseTypeName.empty() && baseTypeName.back() == '*')
        {
            ++pointerDepth;
            baseTypeName.pop_back();
        }

        if (pointerDepth > 0)
        {
            TypeDetails *nonpointerTypeDetails = TryGetTypeDetails(baseTypeName);
            if (!nonpointerTypeDetails)
                return nullptr;

            TypeDetails typeCopy = *nonpointerTypeDetails;
            typeCopy.Name = typeName;
            typeCopy.PointerDepth = pointerDepth;

            for (int i = 0; i < pointerDepth; ++i)
                typeCopy.IRType = typeCopy.IRType->getPointerTo();

            AllTypes.emplace(typeName, std::move(typeCopy));
            return &AllTypes.find(typeName)->second;
        }

        return nullptr;
    }

    TypeDetails& TypeSystem::DereferenceType(const TypeDetails &type, const SourceLocation &referencedSourceLocation)
    {
        if (type.PointerDepth == 0)
            throw CodeGenerationFailedException{.Message = "Attempt to dereference non-pointer type '" + type.Name + "'.", .Location = referencedSourceLocation};

        std::string dereferencedTypeName = type.Name;
        if (dereferencedTypeName.empty() || dereferencedTypeName.back() != '*')
            throw CodeGenerationFailedException{.Message = "(internal) Invalid type details trying to dereference pointer.", .Location = referencedSourceLocation};

        dereferencedTypeName.pop_back();
        return GetTypeDetails(dereferencedTypeName, referencedSourceLocation);
    }

    EvaluationResult TypeSystem::EvaluateLiteral(const ConstantLeaf &constant, const SourceLocation &definedSourceLocation, const TypeDetails *forceType)
    {
        if (constant.IsString)
            throw CodeGenerationFailedException{.Message = "(internal) TODO: Implement string literals.", .Location = definedSourceLocation};

        if (constant.IsFloat)
        {
            const TypeDetails *typeToUse = forceType ? forceType : &compiler.Types.GetTypeDetails("#float" + std::to_string(constant.FloatBits), definedSourceLocation);

            llvm::Value *val = llvm::ConstantFP::get(compiler.LLVMContext, llvm::APFloat(constant.Float)); //TODO: how does it know whether to be a halfloat, float, double, etc?
            return EvaluationResult{.Type = typeToUse, .IRValue = val, .IsConst = true, .Literal = &constant};
        }

        if (constant.IsBool)
        {
            const TypeDetails *typeToUse = forceType ? forceType : &compiler.Types.GetTypeDetails("#bool", definedSourceLocation);

            llvm::Value *val = constant.IsTrue ? llvm::ConstantInt::getTrue(compiler.LLVMContext) : llvm::ConstantInt::getFalse(compiler.LLVMContext);
            return EvaluationResult{.Type = typeToUse, .IRValue = val, .IsConst = true, .Literal = &constant};
        }

        if (constant.IsInteger())
        {
            const TypeDetails *typeToUse = forceType;

            if (constant.UnsignedBits)
            {
                if (!typeToUse)
                {
                    int nearestTypeBits = 8;
                    for (; nearestTypeBits <= 128; nearestTypeBits *= 2)
                    {
                        if (constant.UnsignedBits <= nearestTypeBits)
                        {
                            typeToUse = &compiler.Types.GetTypeDetails("#uint" + std::to_string(nearestTypeBits), definedSourceLocation);
                            break;
                        }
                    }
                }

                if (!typeToUse)
                    throw CodeGenerationFailedException{.Message = "(internal) Bad unsigned integer generated by parser?", .Location = definedSourceLocation};

                llvm::Value *unsignedIRValue = llvm::ConstantInt::get(typeToUse->IRType, constant.UnsignedNumber);

                return EvaluationResult{.Type = typeToUse, .IRValue = unsignedIRValue, .IsConst = true, .Literal = &constant};
            }
            else if (constant.SignedBits)
            {
                if (!typeToUse)
                {
                    int nearestTypeBits = 8;
                    for (; nearestTypeBits <= 128; nearestTypeBits *= 2)
                    {
                        if (constant.SignedBits <= nearestTypeBits)
                        {
                            typeToUse = &compiler.Types.GetTypeDetails("#uint" + std::to_string(nearestTypeBits), definedSourceLocation);
                            break;
                        }
                    }
                }

                if (!typeToUse)
                    throw CodeGenerationFailedException{.Message = "(internal) Bad signed integer generated by parser?", .Location = definedSourceLocation};

                llvm::Value *signedIRValue = llvm::ConstantInt::get(typeToUse->IRType, constant.SignedNumber);

                return EvaluationResult{.Type = typeToUse, .IRValue = signedIRValue, .IsConst = true, .Literal = &constant};
            }
        }

        throw CodeGenerationFailedException{.Message = "(internal) Unhandled literal.", .Location = definedSourceLocation};
    }

    bool TypeSystem::TryMakeResultCompatibleWithType(const EvaluationResult &originalResult, const TypeDetails &targetType, llvm::IRBuilder<> &irBuilder, const SourceLocation &referencedSourceLocation, EvaluationResult &result, bool &truncationAvailable)
    {
        //we may already have what we need
        if (originalResult.Type->Name == targetType.Name)
        {
            result = originalResult;
            return true;
        }

        //if we have a literal, another form of it may be directly compatible
        if (originalResult.Literal && TryReinterpretLiteral(*this, *originalResult.Literal, targetType, irBuilder, referencedSourceLocation, result))
            return true;

        //see if we can losslessly convert it
        std::function<llvm::Value*(llvm::Value*)> builtInConvert = TryGetBuiltInConvertFunction(*originalResult.Type, targetType, irBuilder);
        if (builtInConvert)
        {
            llvm::Value *val = builtInConvert(originalResult.IRValue);
            result = EvaluationResult{.Type = &targetType, .IRValue = val, .IsConst = true, .NeedsDereferenceForValue = false, .AllowTruncate = false};
            return true;
        }

        /*FunctionDetails *convert = TryGetSpecialConvertFunction(*originalResult.Type, targetType);
        llvm::Value *valueToConvert = originalResult.IRValue;

        if (convert)
        {
            std::vector<llvm::Value*> argVals;
            if (convert->Arguments.size() != 1)
                throw CodeGenerationFailedException{.Message = "(internal) Problem with #convert method.", .Location = referencedSourceLocation};
            argVals.emplace_back(valueToConvert);

            llvm::Value *val = irBuilder.CreateCall(convert->IRFunction, argVals, "call " + convert->Name);
            result = EvaluationResult{.Type = convert->ReturnType, .IRValue = val, .IsConst = true, .NeedsDereferenceForValue = false, .AllowTruncate = false};
            return true;
        }*/

        //see if we can truncate to it
        std::function<llvm::Value*(llvm::Value*)> builtInTruncate = TryGetBuiltInTruncateFunction(*originalResult.Type, targetType, irBuilder);
        if (builtInTruncate && originalResult.AllowTruncate)
        {
            llvm::Value *val = builtInTruncate(originalResult.IRValue);
            result = EvaluationResult{.Type = &targetType, .IRValue = val, .IsConst = true, .NeedsDereferenceForValue = false, .AllowTruncate = false};
            truncationAvailable = true;
            return true;
        }

        /*FunctionDetails *truncate = TryGetSpecialTruncateFunction(*originalResult.Type, targetType);
        llvm::Value *valueToTruncate = originalResult.IRValue;

        if (truncate && originalResult.AllowTruncate)
        {
            std::vector<llvm::Value*> argVals;
            if (truncate->Arguments.size() != 1)
                throw CodeGenerationFailedException{.Message = "(internal) Problem with #truncate method.", .Location = referencedSourceLocation};
            argVals.emplace_back(valueToTruncate);

            llvm::Value *val = irBuilder.CreateCall(truncate->IRFunction, argVals, "call " + truncate->Name);
            result = EvaluationResult{.Type = truncate->ReturnType, .IRValue = val, .IsConst = true, .NeedsDereferenceForValue = false, .AllowTruncate = false};
            truncationAvailable = true;
            return true;
        }*/

        return false;
    }

    EvaluationResult TypeSystem::MakeResultCompatibleWithType(const EvaluationResult &originalResult, const TypeDetails &targetType, llvm::IRBuilder<> &irBuilder, const SourceLocation &referencedSourceLocation)
    {
        bool truncationAvailable = false;
        EvaluationResult result;
        if (!TryMakeResultCompatibleWithType(originalResult, targetType, irBuilder, referencedSourceLocation, result, truncationAvailable))
            throw CodeGenerationFailedException{.Message = "Cannot convert from type '" + originalResult.Type->Name + "'" + " to type '" + targetType.Name + "'." + (truncationAvailable ? "  (#truncate is possible)" : ""), .Location = referencedSourceLocation};

        return result;
    }

    EvaluationResult TypeSystem::ExecuteUnaryOperator(const std::string &operation, const EvaluationResult &originalResult, const TypeDetails &operandType, llvm::IRBuilder<> &irBuilder, const SourceLocation &sourceLocation)
    {
        throw CodeGenerationFailedException{.Message = "Unary operator '" + operation + "' not implemented yet.", .Location = sourceLocation};
    }

    EvaluationResult TypeSystem::ExecuteBinaryOperator(const std::string &operation, const EvaluationResult &leftResult, const EvaluationResult &rightResult, const TypeDetails &operandType, llvm::IRBuilder<> &irBuilder, const SourceLocation &sourceLocation)
    {
        EvaluationResult result;
        if (TryExecuteBuiltInBinaryOperator(*this, operation, leftResult, rightResult, operandType, irBuilder, sourceLocation, result))
            return result;
        /*else if (TryExecuteUserDefinedBinaryOperator())
        {
        }*/

        throw CodeGenerationFailedException{.Message = "Type '" + operandType.Name + "' does not provide operator '" + operation + "'", .Location = sourceLocation};
    }
}
