// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "ModuleCompilerInternal.h"

namespace CBreakCompilerInternal
{
    using namespace CBreakCompiler;

    EvaluationContext::EvaluationContext(ModuleCompilerInternal &compiler, llvm::IRBuilder<> &localsAllocBuilder, const FunctionDetails *functionTarget, const EvaluationResult *classTarget): Compiler(compiler), LocalsAllocIRBuilder(localsAllocBuilder)
    {
        this->functionTarget = functionTarget;
        this->classTarget = classTarget;
        nameHint = "Root";

        CreateNewBlock();
    }

    EvaluationContext::EvaluationContext(EvaluationContext &parent, bool createNewBlockInsteadOfUsingParents, const std::string &nameHint): Compiler(parent.Compiler), LocalsAllocIRBuilder(parent.LocalsAllocIRBuilder)
    {
        this->parent = &parent;
        this->functionTarget = parent.functionTarget;
        this->classTarget = parent.classTarget;
        this->nameHint = nameHint;

        if (createNewBlockInsteadOfUsingParents)
            CreateNewBlock();
        else
        {
            BasicBlock = parent.BasicBlock;
            IRBuilder = parent.IRBuilder;
        }
    }

    void EvaluationContext::CreateNewBlock()
    {
        BasicBlock = llvm::BasicBlock::Create(Compiler.LLVMContext, ComputeFriendlyBlockName(), functionTarget ? functionTarget->IRFunction : nullptr);
        IRBuilder = std::make_shared<llvm::IRBuilder<>>(Compiler.LLVMContext);
        IRBuilder->SetInsertPoint(BasicBlock);
    }

    std::string EvaluationContext::ComputeFriendlyBlockName() const
    {
        return nameHint + (classTarget || functionTarget ? " for " : "") + (classTarget ? classTarget->Type->ClassName + "." : "") + (functionTarget ? functionTarget->Name : "") + "-";
    }
}
