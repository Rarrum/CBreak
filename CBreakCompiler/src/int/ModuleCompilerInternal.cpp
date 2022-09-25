// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "ModuleCompilerInternal.h"
#include <unordered_set>

namespace
{
    using namespace CBreakCompiler;
    using namespace CBreakCompilerInternal;

    EvaluationResult EvaluateExpression(const ExpressionNode &expr, EvaluationContext &context);
    EvaluationResult EvaluateExpressionAsValue(const ExpressionNode &expr, EvaluationContext &context);

    std::unordered_set<std::string> allMathBinaryOperators { "==", "!=", "<=", ">=", "&&", "||", "^^", "<", ">", "*", "/", "%", "-", "+", "=", "&", "|", "^" };

    EvaluationResult DereferenceValue(ModuleCompilerInternal &compiler, llvm::IRBuilder<> &irBuilder, const EvaluationResult &originalValue, const SourceLocation &referencedSourceLocation)
    {
        if (originalValue.Type->PointerDepth == 0)
            throw CodeGenerationFailedException{.Message = "Attempt to dereference non-pointer value of type '" + originalValue.Type->Name + "'.", .Location = referencedSourceLocation};

        llvm::Value *derefValue = irBuilder.CreateLoad(originalValue.IRValue);
        TypeDetails &derefType = compiler.Types.DereferenceType(*originalValue.Type, referencedSourceLocation);
        return EvaluationResult{.Type = &derefType, .IRValue = derefValue, .IsConst = true, .NeedsDereferenceForValue = false, .AllowTruncate = originalValue.AllowTruncate};
    }

    void InitializeVariable(const TypeDetails &td, llvm::Value *target, ExpressionNode *initializer, EvaluationContext &context)
    {
        if (td.IsClass())
        {
            if (initializer && !initializer->IsEmpty())
                throw CodeGenerationFailedException{.Message = "(internal) Initializers not yet supported for class types.", .Location = context.GeneralLocation};

            for (size_t fieldIndex = 0; fieldIndex < td.Members->Fields.size(); ++fieldIndex)
            {
                const ClassField &cf = td.Members->Fields[fieldIndex];
                std::vector<llvm::Value*> gepIndex;
                gepIndex.emplace_back(llvm::ConstantInt::get(context.Compiler.LLVMContext, llvm::APInt(32, 0, 10))); //array index 0
                gepIndex.emplace_back(llvm::ConstantInt::get(context.Compiler.LLVMContext, llvm::APInt(32, fieldIndex, 10))); //field number
                llvm::Value *gepValue = context.IRBuilder->CreateGEP(td.IRType, target, gepIndex, "GEP Init " + cf.Name);

                InitializeVariable(*cf.Type, gepValue, cf.Initializer, context);
            }
        }
        else
        {
            //if they provided an initializer we use that, otherwise its 0
            if (initializer && !initializer->IsEmpty())
            {
                EvaluationResult originalEvalResult = EvaluateExpressionAsValue(*initializer, context);
                EvaluationResult compatibleEvalResult = context.Compiler.Types.MakeResultCompatibleWithType(originalEvalResult, td, *context.IRBuilder, context.GeneralLocation);
                context.IRBuilder->CreateStore(compatibleEvalResult.IRValue, target);
            }
            else
            {
                llvm::Value *nullvalue = llvm::Constant::getNullValue(td.IRType);
                context.IRBuilder->CreateStore(nullvalue, target);
            }
        }
    }

    EvaluationResult EvaluateGeneralVariable(const std::string &name, EvaluationContext &context)
    {
        for (EvaluationContext *block = &context; block; block = block->GetParentContext())
        {
            const VariableDetails *vd = block->LocalVariables.TryFindName(name);
            if (vd)
            {
                if (!vd->Activated)
                    throw CodeGenerationFailedException{.Message = "Local variable '" + name + "' cannot be used before it's declared.", .Location = context.GeneralLocation};

                return EvaluationResult{.Type = &context.Compiler.Types.GetTypeDetails(vd->Type->Name + "*", context.GeneralLocation), .IRValue = vd->IRLocalVariable, .IsConst = vd->IsConst, .NeedsDereferenceForValue = true};
            }
        }

        if (context.GetFunctionTarget())
        {
            const VariableDetails *vd = context.GetFunctionTarget()->Returns.TryFindName(name);
            if (vd)
                return EvaluationResult{.Type = vd->Type, .IRValue = vd->IRFunctionParameter, .IsConst = vd->IsConst, .NeedsDereferenceForValue = true};

            vd = context.GetFunctionTarget()->Parameters.TryFindName(name);
            if (vd)
                return EvaluationResult{.Type = vd->Type, .IRValue = vd->IRFunctionParameter, .IsConst = vd->IsConst, .NeedsDereferenceForValue = false};
        }

        const VariableDetails *vd = context.Compiler.Globals.Variables.TryFindName(name);
        if (vd)
        {
            if (!vd->IRGlobalVariable)
                throw CodeGenerationFailedException{.Message = "(internal) IRGlobalVariable is not set.", .Location = context.GeneralLocation};

            return EvaluationResult{.Type = &context.Compiler.Types.GetTypeDetails(vd->Type->Name + "*", context.GeneralLocation), .IRValue = vd->IRGlobalVariable, .IsConst = vd->IsConst, .NeedsDereferenceForValue = true};
        }

        throw CodeGenerationFailedException{.Message = "Unknown variable name '" + name + "'.", .Location = context.GeneralLocation};
    }

    EvaluationResult EvaluateClassVariable(const EvaluationResult &targetClass, const std::string &name, EvaluationContext &context)
    {
        targetClass.SanityCheckType();
        if (!targetClass.Type->IsClass())
            throw CodeGenerationFailedException{.Message = "(internal) Attempt to evaluate a class variable on a non-class.", .Location = context.GeneralLocation};
        else if (targetClass.Type->PointerDepth != 1)
            throw CodeGenerationFailedException{.Message = "Class variable evaluation may only be applied to a direct pointer.", .Location = context.GeneralLocation};

        TypeDetails &targetDerefType = context.Compiler.Types.DereferenceType(*targetClass.Type, context.GeneralLocation);

        size_t fieldIndex = targetClass.Type->Members->Fields.TryFindIndexForName(name);
        if (fieldIndex == VectorWithNameLookup_NotFoundIndex)
            throw CodeGenerationFailedException{.Message = "Unknown class field name '" + name + "'.", .Location = context.GeneralLocation};

        const ClassField &cf = targetClass.Type->Members->Fields[fieldIndex];
        std::vector<llvm::Value*> gepIndex;
        gepIndex.emplace_back(llvm::ConstantInt::get(context.Compiler.LLVMContext, llvm::APInt(32, 0, 10))); //array index 0
        gepIndex.emplace_back(llvm::ConstantInt::get(context.Compiler.LLVMContext, llvm::APInt(32, fieldIndex, 10))); //field number
        llvm::Value *gepValue = context.IRBuilder->CreateGEP(targetDerefType.IRType, targetClass.IRValue, gepIndex, "GEP " + targetClass.Type->ClassName + "." + cf.Name);

        return EvaluationResult{.Type = &context.Compiler.Types.GetTypeDetails(cf.Type->Name + "*", context.GeneralLocation), .IRValue = gepValue, .IsConst = false, .NeedsDereferenceForValue = true};
    }

    //emits a call to a function, and returns the return values from that call
    std::vector<std::tuple<llvm::Value*, const TypeDetails*>> EmitFunctionCall(const FunctionCallLeaf &functionCall, const EvaluationResult *targetClass, EvaluationContext &context)
    {
        const FunctionDetails *fd = nullptr;
        std::vector<llvm::Value*> argVals;

        //the target could be a class function or a global function
        bool classArgCount = 0;
        if (targetClass)
        {
            classArgCount = 1;

            fd = targetClass->Type->Members->Functions.TryFindName(functionCall.Name);
            if (!fd)
                throw CodeGenerationFailedException{.Message = "Unknown class function name '" + functionCall.Name + "' in class '" + targetClass->Type->ClassName + "'.", .Location = context.GeneralLocation};
        }
        else
        {
            fd = context.Compiler.Globals.Functions.TryFindName(functionCall.Name);
            if (!fd)
                throw CodeGenerationFailedException{.Message = "Unknown global function name '" + functionCall.Name + "'.", .Location = context.GeneralLocation};
        }

        //possible return arguments
        bool useTemporaryLocalsForReturn = false;
        if (functionCall.Returns.size() != fd->Returns.size())
        {
            if (functionCall.Returns.size() == 0) // it is acceptable to provide no return args at all
                useTemporaryLocalsForReturn = true;
            else
                throw CodeGenerationFailedException{.Message = "Wrong number of return arguments to '" + functionCall.Name + "'.", .Location = context.GeneralLocation, .OtherLocation = fd->StartLocation};
        }

        std::vector<std::tuple<llvm::Value*, const TypeDetails*>> returnVals;
        for (size_t returnIndex = 0; returnIndex < fd->Returns.size(); ++returnIndex)
        {
            if (useTemporaryLocalsForReturn)
            {
                const TypeDetails *returnType = fd->Returns[returnIndex].Type;
                const TypeDetails *allocReturnType = &context.Compiler.Types.DereferenceType(*returnType, functionCall.Returns[returnIndex].StartLocation);
                llvm::Value *returnVal = context.LocalsAllocIRBuilder.CreateAlloca(allocReturnType->IRType, 0, "#templocal" + std::to_string(returnIndex));
                returnVals.emplace_back(returnVal, returnType);
            }
            else // call return arg length matches function return arg length
            {
                EvaluationResult returnEval = EvaluateExpression(functionCall.Returns[returnIndex], context);
                if (returnEval.IsConst)
                    throw CodeGenerationFailedException{.Message = "A return argument cannot be #const.", .Location = functionCall.Returns[returnIndex].StartLocation, .OtherLocation = fd->Returns[returnIndex].StartLocation};

                returnVals.emplace_back(returnEval.IRValue, returnEval.Type);
            }

            argVals.emplace_back(std::get<0>(returnVals[returnIndex]));
        }

        //possible class argument (first non-return arg is always a pointer to the class instance)
        if (targetClass)
            argVals.emplace_back(targetClass->IRValue);

        //parameter arguments
        if (fd->Parameters.size() != functionCall.Parameters.size() + classArgCount)
            throw CodeGenerationFailedException{.Message = "Wrong number of parameter arguments to '" + functionCall.Name + "'.", .Location = context.GeneralLocation, .OtherLocation = fd->StartLocation};

        size_t paramIndex = classArgCount;
        for (const ExpressionNode &expr : functionCall.Parameters)
        {
            const VariableDetails &destArg = fd->Parameters[paramIndex];
            EvaluationResult originalArgEval = EvaluateExpressionAsValue(expr, context);
            EvaluationResult compatibleArgEval = context.Compiler.Types.MakeResultCompatibleWithType(originalArgEval, *destArg.Type, *context.IRBuilder, context.GeneralLocation);

            argVals.emplace_back(compatibleArgEval.IRValue);
            ++paramIndex;
        }

        context.IRBuilder->CreateCall(fd->IRFunction, argVals);
        //TODO: some way to tag a function as being a C function we need to call, and use the return from CreateCall

        return returnVals;
    }

    void CreateExportWrapper(ModuleCompilerInternal &compiler, FunctionDetails &functionTarget, const FunctionInformation &fi, const ClassInformation *ci)
    {
        //TODO: Allow this by making the first paramater magically be an instance of the class
        if (ci)
            throw CodeGenerationFailedException{.Message = "#cexport cannot be used on functions that are a member of a class.", .Location = fi.StartLocation};

        if (fi.Qualifiers.CExport == fi.Name)
            throw CodeGenerationFailedException{.Message = "#cexport name for function '" + fi.Name + "' must differ from its name.", .Location = fi.StartLocation};

        if (fi.Returns.size() > 1)
            throw CodeGenerationFailedException{.Message = "#cexport requires function '" + fi.Name + "' to have 0 or 1 returns.", .Location = fi.StartLocation};

        //Create the C function wrapper
        std::vector<llvm::Type*> exportArgIrTypes;
        for (const VariableInformation &vi : fi.Parameters)
        {
            const TypeDetails &type = compiler.Types.GetTypeDetails(vi.Type, vi.StartLocation);
            exportArgIrTypes.emplace_back(type.IRType);
        }

        llvm::Type *exportReturnIrType = nullptr;
        if (fi.Returns.empty())
            exportReturnIrType = llvm::Type::getVoidTy(compiler.LLVMContext);
        else
        {
            //cbreak return types end up being pointers, but we need the non-pointer type for the actual c declaration
            const TypeDetails &type = compiler.Types.GetTypeDetails(fi.Returns[0].Type, fi.Returns[0].StartLocation);
            const TypeDetails &derefType = compiler.Types.DereferenceType(type, fi.Returns[0].StartLocation);
            exportReturnIrType = derefType.IRType;
        }

        llvm::FunctionType *exportFunctionIrType = llvm::FunctionType::get(exportReturnIrType, exportArgIrTypes, false);
        llvm::Function *irWrapperFunction = llvm::Function::Create(exportFunctionIrType, llvm::GlobalValue::ExternalLinkage, fi.Qualifiers.CExport, &compiler.LLVMModule);

        size_t argIndex = 0;
        for (auto &irFuncArg : irWrapperFunction->args())
        {
            irFuncArg.setName(functionTarget.Parameters[argIndex].Name);
            ++argIndex;
        }

        //Create a FunctionDetails for the C wrapper
        //NOTE: We don't fill in returns, because that concept doesn't exist for the C wrapper.. we have code to return in the C way below
        FunctionDetails wrapperDetails;
        wrapperDetails.Name = fi.Qualifiers.CExport;
        wrapperDetails.StartLocation = fi.StartLocation;
        wrapperDetails.IRFunctionType = exportFunctionIrType;
        wrapperDetails.IRFunction = irWrapperFunction;

        argIndex = 0;
        for (auto &irFuncArg : irWrapperFunction->args())
        {
            //params are the same as the real function, but we need to point it to our own arg
            wrapperDetails.Parameters.Add(functionTarget.Parameters[argIndex].Name, functionTarget.Parameters[argIndex]);
            wrapperDetails.Parameters.back().IRFunctionParameter = &irFuncArg;
        }

        //Create the code to call our function from the C function
        llvm::BasicBlock *bbWrapperFunctionLocals = llvm::BasicBlock::Create(compiler.LLVMContext, "CExport wrapper for " + fi.Name, irWrapperFunction);
        llvm::IRBuilder<> wrapperIRBuilderLocals(compiler.LLVMContext);
        wrapperIRBuilderLocals.SetInsertPoint(bbWrapperFunctionLocals);

        EvaluationContext cexportCallContext(compiler, wrapperIRBuilderLocals, &wrapperDetails, nullptr);

        FunctionCallLeaf callLeaf{ .Name = fi.Name };

        for (VariableDetails &vd : functionTarget.Parameters)
        {
            callLeaf.Parameters.emplace_back();
            callLeaf.Parameters.back().StartLocation = functionTarget.StartLocation;
            callLeaf.Parameters.back().Leaf.Variable = std::make_unique<VariableLeaf>();
            callLeaf.Parameters.back().Leaf.Variable->Name = vd.Name;
        }

        std::vector<std::tuple<llvm::Value*, const TypeDetails*>> returnVals = EmitFunctionCall(callLeaf, nullptr, cexportCallContext);
        wrapperIRBuilderLocals.CreateBr(cexportCallContext.BasicBlock);

        if (fi.Returns.empty())
            cexportCallContext.IRBuilder->CreateRetVoid();
        else
        {
            llvm::Value *derefValue = cexportCallContext.IRBuilder->CreateLoad(std::get<0>(returnVals.back()), "#creturn");
            cexportCallContext.IRBuilder->CreateRet(derefValue);
        }
    }

    EvaluationResult EvaluateLeaf(const EvaluationResult *targetClass, const ExpressionLeaf &leaf, EvaluationContext &context)
    {
        if (leaf.Constant)
        {
            if (targetClass)
                throw CodeGenerationFailedException{.Message = "(internal) Did not expect class constant leaf.", .Location = context.GeneralLocation};

            EvaluationResult result = context.Compiler.Types.EvaluateLiteral(*leaf.Constant, context.GeneralLocation);

            result.AllowTruncate = leaf.Qualifiers.Truncate;
            return result;
        }
        else if (leaf.Variable)
        {
            EvaluationResult result;
            if (targetClass)
                result = EvaluateClassVariable(*targetClass, leaf.Variable->Name, context);
            else
                result = EvaluateGeneralVariable(leaf.Variable->Name, context);

            result.AllowTruncate = leaf.Qualifiers.Truncate;
            return result;
        }
        else if (leaf.Function)
        {
            std::vector<std::tuple<llvm::Value*, const TypeDetails*>> returnVals = EmitFunctionCall(*leaf.Function, targetClass, context);

            llvm::Value *returnVal = nullptr;
            const TypeDetails *returnType = nullptr;
            if (returnVals.empty())
                returnType = &context.Compiler.Types.GetTypeDetails("#void", context.GeneralLocation);
            else if (returnVals.size() > 1)
                returnType = &context.Compiler.Types.GetTypeDetails("#void", context.GeneralLocation); //TODO: we should return something special here so other parts of code can emit a better message if this is used as part of an expression
            else //single return args can be used in an expression
            {
                //returns end up being pointers, so we need to deref it back to its real value
                std::tie(returnVal, returnType) = returnVals.front();
                EvaluationResult origReturn = {.Type = returnType, .IRValue = returnVal, .IsConst = true, .NeedsDereferenceForValue = true};
                EvaluationResult dereffedReturn = DereferenceValue(context.Compiler, *context.IRBuilder, origReturn, context.GeneralLocation);
                returnType = dereffedReturn.Type;
                returnVal = dereffedReturn.IRValue;
            }

            return EvaluationResult{.Type = returnType, .IRValue = returnVal, .IsConst = true, .NeedsDereferenceForValue = false, .AllowTruncate = leaf.Qualifiers.Truncate};
        }
        else if (leaf.Group)
        {
            EvaluationResult result = EvaluateExpression(*leaf.Group, context); 
            result.AllowTruncate = leaf.Qualifiers.Truncate;
            return result;
        }

        throw CodeGenerationFailedException{.Message = "(internal) ExpressionLeaf is not set.", .Location = context.GeneralLocation};
    }

    EvaluationResult HandleAssignmentOperator(const ExpressionNode &exprLeft, const ExpressionNode &exprRight, EvaluationContext &context)
    {
        //left side
        EvaluationResult evalTarget = EvaluateExpression(exprLeft, context);
        evalTarget.SanityCheckType();
        if (evalTarget.IsConst)
            throw CodeGenerationFailedException{.Message = "Cannot reassign to constant.", .Location = context.GeneralLocation};
        else if (evalTarget.Type->PointerDepth == 0)
            throw CodeGenerationFailedException{.Message = "Left side of assignment must resolve to a pointer.", .Location = context.GeneralLocation};

        TypeDetails &targetDerefType = context.Compiler.Types.DereferenceType(*evalTarget.Type, context.GeneralLocation);

        //right side
        EvaluationResult rightExprInitialResult = EvaluateExpressionAsValue(exprRight, context);
        rightExprInitialResult.SanityCheckType();

        EvaluationResult rightExprCompatibleResult = context.Compiler.Types.MakeResultCompatibleWithType(rightExprInitialResult, targetDerefType, *context.IRBuilder, context.GeneralLocation);

        //store
        context.IRBuilder->CreateStore(rightExprCompatibleResult.IRValue, evalTarget.IRValue);

        return EvaluationResult{.Type = &context.Compiler.Types.GetTypeDetails("#void", context.GeneralLocation), .IRValue = nullptr, .IsConst = true, .NeedsDereferenceForValue = false};
    }

    EvaluationResult HandleDotOperator(const ExpressionNode &exprLeft, const ExpressionNode &exprRight, EvaluationContext &context)
    {
        EvaluationResult leftEval = EvaluateExpression(exprLeft, context);
        leftEval.SanityCheckType();
        if (!leftEval.Type->IsClass())
            throw CodeGenerationFailedException{.Message = "Operator '.' may only be applied to a class.", .Location = context.GeneralLocation};
        else if (leftEval.Type->PointerDepth != 1)
            throw CodeGenerationFailedException{.Message = "Operator '.' may only be applied to a direct pointer.", .Location = context.GeneralLocation};

        if (exprRight.Leaf.IsEmpty())
            throw CodeGenerationFailedException{.Message = "(internal) TODO: Binary '.' operator doesn't support complex expansions yet", .Location = context.GeneralLocation};

        return EvaluateLeaf(&leftEval, exprRight.Leaf, context);
    }

    EvaluationResult HandleMathBinaryOperator(const ExpressionNode &exprLeft, const ExpressionNode &exprRight, const std::string &operation, EvaluationContext &context)
    {
        EvaluationResult originalLeftResult = EvaluateExpressionAsValue(exprLeft, context);
        EvaluationResult originalRightResult = EvaluateExpressionAsValue(exprRight, context);

        EvaluationResult leftResultToUse;
        EvaluationResult rightResultToUse;
        bool truncationAvailable = false;
        if (context.Compiler.Types.TryMakeResultCompatibleWithType(originalLeftResult, *originalRightResult.Type, *context.IRBuilder, context.GeneralLocation, leftResultToUse, truncationAvailable))
            rightResultToUse = originalRightResult;
        else if (context.Compiler.Types.TryMakeResultCompatibleWithType(originalRightResult, *originalLeftResult.Type, *context.IRBuilder, context.GeneralLocation, rightResultToUse, truncationAvailable))
            leftResultToUse = originalLeftResult;
        else
            throw CodeGenerationFailedException{.Message = "Types '" + originalLeftResult.Type->Name + "' and '" + originalRightResult.Type->Name + "' are not compatible." + (truncationAvailable ? " (truncation available)" : ""), .Location = context.GeneralLocation};

        if (leftResultToUse.Type->PointerDepth > 0 || rightResultToUse.Type->PointerDepth > 0)
            throw CodeGenerationFailedException{.Message = "Math operator '" + operation + "' does not work on pointers yet.", .Location = context.GeneralLocation};

        return context.Compiler.Types.ExecuteBinaryOperator(operation, leftResultToUse, rightResultToUse, *leftResultToUse.Type, *context.IRBuilder, context.GeneralLocation);
    }

    EvaluationResult EvaluateExpression(const ExpressionNode &expr, EvaluationContext &context)
    {
        context.GeneralLocation = expr.StartLocation;

        if (expr.IsBinary)
        {
            if (!expr.LeftNode)
                throw CodeGenerationFailedException{.Message = "(internal) Binary operator missing left expression node.", .Location = context.GeneralLocation};
            else if (!expr.RightNode)
                throw CodeGenerationFailedException{.Message = "(internal) Binary operator missing right expression node.", .Location = context.GeneralLocation};

            if (expr.Operation == "=")
                return HandleAssignmentOperator(*expr.LeftNode, *expr.RightNode, context);
            else if (expr.Operation == ".")
                return HandleDotOperator(*expr.LeftNode, *expr.RightNode, context);
            else if (allMathBinaryOperators.find(expr.Operation) != allMathBinaryOperators.end())
                return HandleMathBinaryOperator(*expr.LeftNode, *expr.RightNode, expr.Operation, context);
            else
                throw CodeGenerationFailedException{.Message = "Unknown binary operator '" + expr.Operation + "'.", .Location = context.GeneralLocation};
        }
        else
        {
            if (expr.RightNode)
                throw CodeGenerationFailedException{.Message = "(internal) Did not expect right expression node for non-binary operator.", .Location = context.GeneralLocation};

            EvaluationResult leftResult;
            if (expr.LeftNode)
                leftResult = EvaluateExpression(*expr.LeftNode, context);
            else
                leftResult = EvaluateLeaf(nullptr, expr.Leaf, context);

            if (expr.Operation.empty())
                return leftResult;

            throw CodeGenerationFailedException{.Message = "Unary operator '" + expr.Operation + "' not implemented yet.", .Location = context.GeneralLocation};
        }
    }

    EvaluationResult EvaluateExpressionAsValue(const ExpressionNode &expr, EvaluationContext &context)
    {
        EvaluationResult evalResult = EvaluateExpression(expr, context);
        if (evalResult.NeedsDereferenceForValue && evalResult.Type->PointerDepth > 0)
            return DereferenceValue(context.Compiler, *context.IRBuilder, evalResult, expr.StartLocation);
        else
            return evalResult;
    }

    //returns the (possibly nullptr) last block in this code block, such that if more code follows this block, the caller must emit a branch from the returned block to the new one
    llvm::BasicBlock* ProcessCodeBlock(const StatementsBlock &sourceBlock, EvaluationContext &parentContext, bool createNewBlock)
    {
        EvaluationContext context(parentContext, createNewBlock, "CodeBlock");
        context.GeneralLocation = sourceBlock.StartLocation;

        //create a branch from the end of the parent block to this new block
        if (createNewBlock)
            parentContext.IRBuilder->CreateBr(context.BasicBlock);

        //allocate locals for this block
        for (const VariableInformation &vi : sourceBlock.LocalVariables)
        {
            const TypeDetails &type = context.Compiler.Types.GetTypeDetails(vi.Type, vi.StartLocation);
            VariableDetails vd{.Type = &type, .Name = vi.Name, .IsConst = vi.IsConst, .StartLocation = vi.StartLocation};
            vd.Activated = false;
            vd.IRLocalVariable = context.LocalsAllocIRBuilder.CreateAlloca(type.IRType, 0, vi.Name);

            context.LocalVariables.Add(vd.Name, std::move(vd));
        }

        //process statements, some of which may activate local variables for use
        bool processedReturn = false;
        bool needsLastBlockLinkedToParentReturn = true;
        for (const StatementInformation &si : sourceBlock.Statements)
        {
            context.GeneralLocation = si.StartLocation;
            needsLastBlockLinkedToParentReturn = true;

            if (processedReturn)
                throw CodeGenerationFailedException{.Message = "A #return statement may only appear as the last line in a block of code.", .Location = si.StartLocation};

            if (si.Type == StatementType::EvaluateOnly)
            {
                if (si.Expression && !si.Expression->IsEmpty())
                    EvaluateExpression(*si.Expression, context);
            }
            else if (si.Type == StatementType::VariableDeclaration)
            {
                //activate and possibly initialize
                size_t localIndex = context.LocalVariables.TryFindIndexForName(si.VariableName);
                if (localIndex == VectorWithNameLookup_NotFoundIndex)
                    throw CodeGenerationFailedException{.Message = "(internal) Could not find local variable '" + si.VariableName + "' allocation.", .Location = context.GeneralLocation};

                VariableDetails &vd = context.LocalVariables[localIndex];
                const VariableInformation &vi = sourceBlock.LocalVariables[localIndex]; //note: indexes the same

                InitializeVariable(*vd.Type, vd.IRLocalVariable, vi.Initializer.get(), context);
                vd.Activated = true;

                if (si.Expression)
                    throw CodeGenerationFailedException{.Message = "(internal) Did not expect statement expression for local variable activation.", .Location = context.GeneralLocation};
            }
            else if (si.Type == StatementType::Return)
            {
                processedReturn = true;
                needsLastBlockLinkedToParentReturn = false;
                context.IRBuilder->CreateRetVoid();
            }
            else if (si.Type == StatementType::ChildBlock)
            {
                if (!si.CodeBlock)
                    throw CodeGenerationFailedException{.Message = "(internal) CodeBlock missing for ChildBlock statement.", .Location = context.GeneralLocation};

                llvm::BasicBlock *previousBlock = ProcessCodeBlock(*si.CodeBlock, context, true);

                //after ProcessCodeBlock returns, we need a new block for the remainder of this context, and to link the previous block to this one
                if (previousBlock)
                {
                    context.CreateNewBlock();
                    llvm::IRBuilder<> prevBuilder(context.Compiler.LLVMContext);
                    prevBuilder.SetInsertPoint(previousBlock);
                    prevBuilder.CreateBr(context.BasicBlock);
                }
                else
                    needsLastBlockLinkedToParentReturn = false;
            }
            else if (si.Type == StatementType::ConditionalIfElse)
            {
                EvaluationResult originalConditionResult = EvaluateExpression(*si.Expression, context);
                EvaluationResult boolConditionResult = context.Compiler.Types.MakeResultCompatibleWithType(originalConditionResult, context.Compiler.Types.GetTypeDetails("#bool", si.StartLocation), *context.IRBuilder, si.StartLocation);

                //emit blocks for the if and else (even if there is no else - llvm's CreateCondBr seems to crash if you don't provide the false condition) and conditionally branch to them
                EvaluationContext ifContext(context, true, "IfTrue");
                llvm::BasicBlock *ifBlockStart = ifContext.BasicBlock;
                llvm::BasicBlock *ifBlockEnd = ProcessCodeBlock(*si.CodeBlock, ifContext, false);

                EvaluationContext elseContext(context, true, "IfFalse");
                llvm::BasicBlock *elseBlockStart = elseContext.BasicBlock;
                llvm::BasicBlock *elseBlockEnd = elseBlockStart;
                if (si.CodeBlockElse)
                    elseBlockEnd = ProcessCodeBlock(*si.CodeBlockElse, elseContext, false);

                context.IRBuilder->CreateCondBr(boolConditionResult.IRValue, ifBlockStart, elseBlockStart);

                //hook the end of the if (and else if exists) up to a new block in this context
                context.CreateNewBlock();
                llvm::IRBuilder<> prevBuilder(context.Compiler.LLVMContext);
                if (ifBlockEnd)
                {
                    prevBuilder.SetInsertPoint(ifBlockEnd);
                    prevBuilder.CreateBr(context.BasicBlock);
                }

                if (elseBlockEnd)
                {
                    prevBuilder.SetInsertPoint(elseBlockEnd);
                    prevBuilder.CreateBr(context.BasicBlock);
                }
            }
            else
                throw CodeGenerationFailedException{.Message = "(internal) Unknown StatementType.", .Location = context.GeneralLocation};
        }

        if (needsLastBlockLinkedToParentReturn)
            return context.BasicBlock;
        else
            return nullptr;
    }
}

namespace CBreakCompilerInternal
{
    using namespace CBreakCompiler;

    FunctionDetails FunctionDetails::CreateDeclaration(const FunctionInformation &funcDecl, bool externLinkage, const std::string &linkName, ModuleCompilerInternal &compiler)
    {
        FunctionDetails fd{.Name = funcDecl.Name, .StartLocation = funcDecl.StartLocation};

        //return and parameter arguments
        for (const VariableInformation &vi : funcDecl.Returns)
        {
            const TypeDetails &type = compiler.Types.GetTypeDetails(vi.Type, vi.StartLocation);
            VariableDetails vd{.Type = &type, .Name = vi.Name, .IsConst = vi.IsConst, .StartLocation = vi.StartLocation};
            fd.Returns.Add(vi.Name, std::move(vd));
        }

        for (const VariableInformation &vi : funcDecl.Parameters)
        {
            const TypeDetails &type = compiler.Types.GetTypeDetails(vi.Type, vi.StartLocation);
            VariableDetails vd{.Type = &type, .Name = vi.Name, .IsConst = vi.IsConst, .StartLocation = vi.StartLocation};
            fd.Parameters.Add(vi.Name, std::move(vd));
        }

        std::vector<VariableDetails*> args;
        std::vector<llvm::Type*> argIrTypes;

        //order all the arguments
        for (VariableDetails &vd : fd.Returns)
        {
            auto i = std::find_if(args.begin(), args.end(), [&](VariableDetails* existingVar) { return vd.Name == existingVar->Name; });
            if (i != args.end())
                throw CodeGenerationFailedException{.Message = "Attempt to redefine parameter '" + vd.Name + "'.", .Location = vd.StartLocation, .OtherLocation = (**i).StartLocation};

            args.emplace_back(&vd);
            argIrTypes.emplace_back(vd.Type->IRType);
        }

        for (VariableDetails &vd : fd.Parameters)
        {
            auto i = std::find_if(args.begin(), args.end(), [&](VariableDetails* existingVar) { return vd.Name == existingVar->Name; });
            if (i != args.end())
                throw CodeGenerationFailedException{.Message = "Attempt to redefine parameter '" + vd.Name + "'.", .Location = vd.StartLocation, .OtherLocation = (**i).StartLocation};

            args.emplace_back(&vd);
            argIrTypes.emplace_back(vd.Type->IRType);
        }

        //function ir
        fd.IRFunctionType = llvm::FunctionType::get(llvm::Type::getVoidTy(compiler.LLVMContext), argIrTypes, false);
        fd.IRFunction = llvm::Function::Create(fd.IRFunctionType, externLinkage ? llvm::GlobalValue::ExternalLinkage : llvm::Function::InternalLinkage, linkName, &compiler.LLVMModule);

        size_t argIndex = 0;
        for (auto &irFuncArg : fd.IRFunction->args())
        {
            irFuncArg.setName(args[argIndex]->Name);
            args[argIndex]->IRFunctionParameter = &irFuncArg;
            ++argIndex;
        }

        return fd;
    }

    ModuleCompilerInternal::ModuleCompilerInternal(CompiledModule &targetModule, llvm::Module &llvmModule, const GenerationOptions &options):
        TargetModule(targetModule), LLVMContext(llvmModule.getContext()), LLVMModule(llvmModule), Types(*this), Options(options)
    {
    }

    void ModuleCompilerInternal::AddGlobalVariable(const VariableInformation &vi)
    {
        const TypeDetails &type = Types.GetTypeDetails(vi.Type, vi.StartLocation);

        const VariableDetails *existingVar = Globals.Variables.TryFindName(vi.Name);
        if (existingVar)
            throw CodeGenerationFailedException{.Message = "Attempt to redefine global variable '" + vi.Name + "'.", .Location = vi.StartLocation, .OtherLocation = existingVar->StartLocation};

        VariableDetails vd{.Type = &type, .Name = vi.Name, .IsConst = vi.IsConst, .StartLocation = vi.StartLocation};
        LLVMModule.getOrInsertGlobal(vi.Name, type.IRType);
        vd.IRGlobalVariable = LLVMModule.getNamedGlobal(vi.Name);
        vd.IRGlobalVariable->setLinkage(vi.Qualifiers.Export ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::InternalLinkage);

        //TODO: come up with a better way to initialize globals that allows for more than just a simple constant
        if (type.IsClass())
            throw CodeGenerationFailedException{.Message = "Global variables cannot be classes yet.", .Location = vi.StartLocation};

        if (vi.Initializer && !vi.Initializer->IsEmpty())
        {
            if (vi.Initializer->LeftNode || vi.Initializer->RightNode)
                throw CodeGenerationFailedException{.Message = "(internal) Did not expect global variable to have complex initializer.", .Location = vi.StartLocation};
            else if (!vi.Initializer->Leaf.Constant)
                throw CodeGenerationFailedException{.Message = "(internal) Expect global variable initializer to have a constant leaf.", .Location = vi.StartLocation};

            //TODO: handle alternate type correctly
            llvm::IRBuilder<> localsBuilder(LLVMContext);
            EvaluationContext noContext(*this, localsBuilder, nullptr, nullptr);
            noContext.GeneralLocation = vi.StartLocation;
            EvaluationResult initResult = EvaluateLeaf(nullptr, vi.Initializer->Leaf, noContext);
            if (initResult.Type->Name != vd.Type->Name)
                throw CodeGenerationFailedException{.Message = "Initializer type not compatible with global variable.", .Location = vi.StartLocation};
            else if (!initResult.Literal)
                throw CodeGenerationFailedException{.Message = "Initializer for global variables must be a literal.", .Location = vi.StartLocation};

            llvm::Constant *irConstantValue = static_cast<llvm::Constant*>(initResult.IRValue); //This is dirty
            vd.IRGlobalVariable->setInitializer(irConstantValue);
        }
        else
            vd.IRGlobalVariable->setInitializer(llvm::Constant::getNullValue(type.IRType));

        Globals.Variables.Add(vi.Name, std::move(vd));
    }

    VariableDetails& ModuleCompilerInternal::GetGlobalVariable(const std::string &name, const SourceLocation &referencedSourceLocation)
    {
        VariableDetails *existingVar = Globals.Variables.TryFindName(name);
        if (!existingVar)
            throw CodeGenerationFailedException{.Message = "Use of unknown global variable '" + name + "'.", .Location = referencedSourceLocation};

        return *existingVar;
    }

    void ModuleCompilerInternal::AddFunctionDeclaration(const FunctionInformation &fi, const ClassInformation *ci)
    {
        //check for redeclaration
        bool isExported = fi.Qualifiers.Export;
        std::string linkName = fi.Name;

        TypeDetails *classType = nullptr;
        if (ci)
        {
            classType = &Types.GetTypeDetails(ci->Name, ci->StartLocation);
            isExported = isExported || ci->Qualifiers.Export;
            linkName = ci->Name + "." + linkName;
        }

        if (classType)
        {
            const FunctionDetails *existing = classType->Members->Functions.TryFindName(fi.Name);
            if (existing)
                throw CodeGenerationFailedException{.Message = "Attempt to redefine function '" + fi.Name + "' in class '" + ci->Name + "'.", .Location = fi.StartLocation, .OtherLocation = existing->StartLocation};
        }
        else
        {
            const FunctionDetails *existing = Globals.Functions.TryFindName(fi.Name);
            if (existing)
                throw CodeGenerationFailedException{.Message = "Attempt to redefine global function '" + fi.Name + "'.", .Location = fi.StartLocation, .OtherLocation = existing->StartLocation};
        }

        FunctionDetails fd = FunctionDetails::CreateDeclaration(fi, isExported, linkName, *this);

        if (classType)
        {
            classType->Members->Functions.Add(fi.Name, std::move(fd));
            //TODO: Add exported class type to TargetModule if needed
        }
        else
        {
            Globals.Functions.Add(fi.Name, std::move(fd));

            if (isExported)
                TargetModule.ExportedGlobalFunctions.emplace_back(fi.CopyWithoutCode());
        }
    }

    void ModuleCompilerInternal::AddExternalFunctionDeclaration(const FunctionInformation &funcDecl)
    {
        //TODO: check for name collisions!!!!!
        //TODO: class stuff

        std::string linkName = funcDecl.Name;
        FunctionDetails fd = FunctionDetails::CreateDeclaration(funcDecl, true, linkName, *this);
        Globals.Functions.Add(funcDecl.Name, std::move(fd));
    }

    FunctionDetails& ModuleCompilerInternal::GetGlobalFunction(const std::string &name, const SourceLocation &referencedSourceLocation)
    {
        FunctionDetails *existing = Globals.Functions.TryFindName(name);
        if (!existing)
            throw CodeGenerationFailedException{.Message = "Use of unknown global function '" + name + "'.", .Location = referencedSourceLocation};

        return *existing;
    }

    bool ModuleCompilerInternal::AddClassDeclaration(const ClassInformation &ci, bool throwIfTypesCannotBeResolved)
    {
        std::vector<llvm::Type*> irTypeFields;
        VectorWithNameLookup<ClassField> fields;
        for (const VariableInformation &field : ci.Variables)
        {
            TypeDetails *fieldType = Types.TryGetTypeDetails(field.Type);
            if (!fieldType)
            {
                if (throwIfTypesCannotBeResolved)
                    throw CodeGenerationFailedException{.Message = "Use of unknown type '" + field.Type + "' for class field '" + field.Name + "'.", .Location = field.StartLocation};
                else
                    return false;
            }

            irTypeFields.emplace_back(fieldType->IRType);
            fields.Add(field.Name, ClassField{.Name = field.Name, .Type = fieldType, .Initializer = field.Initializer.get()});
        }

        //TODO: external linkage mechanism for classes?
        llvm::StructType *irType = llvm::StructType::create(LLVMContext, irTypeFields, ci.Name, ci.Qualifiers.Pack);
        TypeDetails td{.ClassName = ci.Name, .Name = ci.Name, .IRType = irType, .StartLocation = ci.StartLocation};
        td.Members->IsClass = true;
        td.Members->Fields = std::move(fields);

        Types.AddType(std::move(td));
        return true;
    }

    void ModuleCompilerInternal::AddFunctionBody(FunctionDetails &functionTarget, const FunctionInformation &fi, const EvaluationResult *classContext)
    {
        //set up a block for allocating locals up-front
        std::string friendlyFullFunctionName = (classContext ? classContext->Type->ClassName + "." : "") + functionTarget.Name;
        llvm::BasicBlock *bbLocals = llvm::BasicBlock::Create(LLVMContext, "function " + friendlyFullFunctionName + " locals", functionTarget.IRFunction);
        llvm::IRBuilder<> localsBuilder(LLVMContext);
        localsBuilder.SetInsertPoint(bbLocals);

        //set up our initial top-level context for evaluating the function
        EvaluationContext context(*this, localsBuilder, &functionTarget, classContext);
        context.GeneralLocation = fi.StartLocation;

        llvm::BasicBlock *initialFunctionCodeBlock = context.BasicBlock;

        //the function itself is one giant code block to be processed like normal
        ProcessCodeBlock(fi.CodeBlock, context, false);

        //TODO: enforce that all return values are assigned to somehow

        //after evaluating everything (which can result in additional locals being created), we can hook up the end of locals block to the start of the function block
        localsBuilder.CreateBr(initialFunctionCodeBlock);

        //validate that llvm is happy with the function
        if (!Options.DisableLLVMVerifier)
        {
            std::string validationErrors;
            llvm::raw_string_ostream validationErrorsStream(validationErrors);
            if (llvm::verifyFunction(*functionTarget.IRFunction, &validationErrorsStream))
            {
                validationErrorsStream.flush();
                CodeValidationException cve;
                cve.Message = "(internal) llvm function verification failed for '" + fi.Name + "':\r\n" + validationErrors;
                cve.Location = context.GeneralLocation;
                throw std::move(cve);
            }
        }

        //we have also have a CExport wrapper to create
        if (!fi.Qualifiers.CExport.empty())
        {
            //TODO: add class pointer?
            CreateExportWrapper(*this, functionTarget, fi, nullptr);
        }
    }
}
