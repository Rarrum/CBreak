// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#pragma once
#include "../ModuleCompiler.h"

#include <memory>
#include <unordered_map>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Verifier.h"

#include "VectorWithNameLookup.h"

namespace CBreakCompilerInternal
{
    using namespace CBreakCompiler;

    struct ClassField;
    struct FunctionDetails;
    class ModuleCompilerInternal;

    class ModuleConfiguration
    {
    public:
        ModuleIdentifier Identifier;
        std::vector<ModuleIdentifier> ModuleImports;
        int DefaultIntBits = 0;
        int DefaultFloatBits = 0;

        void CreateFromFragments(ModuleCompilerInternal &compiler, const std::vector<ParsedModuleFragment> &parsedFragments);

    private:
        void ApplyConfigurationValue(const ModuleConfigurationValue &mcv);
        void FinalizeConfig(ModuleCompilerInternal &compiler);

        bool finalized = false;
    };

    struct TypeMemberData
    {
        //only classes have these:
        bool IsClass = false;
        VectorWithNameLookup<ClassField> Fields;

        //all types may have these:
        VectorWithNameLookup<FunctionDetails> Functions;
    };

    struct TypeDetails
    {
        std::string ClassName; //or fundamental type name
        std::string Name; //will include trailing '*' characters corresponding to PointerDepth
        int PointerDepth = 0;
        llvm::Type *IRType = nullptr;

        std::shared_ptr<TypeMemberData> Members = std::make_shared<TypeMemberData>();

        SourceLocation StartLocation;

        inline bool IsClass() const { return Members->IsClass; }
    };

    struct ClassField
    {
        std::string Name;
        const TypeDetails *Type = nullptr;
        ExpressionNode *Initializer = nullptr;
    };

    struct EvaluationResult
    {
        const TypeDetails *Type = nullptr;
        llvm::Value *IRValue = nullptr;
        bool IsConst = false;
        bool NeedsDereferenceForValue = false; //TODO: get rid of this and use whether it's a pointer instead?
        bool AllowTruncate = false;

        //Only present from direct evaluation of literal constants.  This lets us potentially interpret an unsigned value as signed if there value would fit within the lower bits without setting the sign bit.
        const ConstantLeaf *Literal = nullptr;

        inline void SanityCheckType() const
        {
            if (IRValue->getType() != Type->IRType)
                throw CodeGenerationFailedException{.Message = "(internal) Evaluation type mismatch for type '" + Type->Name + "'."};
        }
    };

    struct VariableDetails
    {
        const TypeDetails *Type = nullptr;
        std::string Name;
        bool Activated = true;
        bool IsConst = false;

        //Exactly one of these will be set:
        llvm::GlobalVariable *IRGlobalVariable = nullptr;
        llvm::Value *IRFunctionParameter = nullptr;
        llvm::AllocaInst *IRLocalVariable = nullptr;

        SourceLocation StartLocation;
    };

    struct FunctionDetails
    {
        std::string Name;
        VectorWithNameLookup<VariableDetails> Returns;
        VectorWithNameLookup<VariableDetails> Parameters;

        llvm::FunctionType *IRFunctionType = nullptr;
        llvm::Function *IRFunction = nullptr;

        SourceLocation StartLocation;

        static FunctionDetails CreateDeclaration(const FunctionInformation &funcDecl, bool externLinkage, const std::string &linkName, ModuleCompilerInternal &compiler);
    };

    struct EvaluationContext
    {
        EvaluationContext() = delete;
        EvaluationContext(const EvaluationContext&) = delete;
        EvaluationContext(EvaluationContext&&) = default;
        EvaluationContext(ModuleCompilerInternal &compiler, llvm::IRBuilder<> &localsAllocBuilder, const FunctionDetails *functionTarget, const EvaluationResult *classTarget);
        EvaluationContext(EvaluationContext &parent, bool createNewBlockInsteadOfUsingParents, const std::string &nameHint);

        EvaluationContext& operator=(const EvaluationContext&) = delete;
        EvaluationContext& operator=(EvaluationContext&&) = default;

        ModuleCompilerInternal &Compiler;
        llvm::IRBuilder<> &LocalsAllocIRBuilder; //for allocating temporaries/locals at the start of the current function stack frame only

        inline EvaluationContext* GetParentContext() { return parent; }

        inline const FunctionDetails* GetFunctionTarget() const { return functionTarget; }
        inline const EvaluationResult* GetClassTarget() const { return classTarget; }

        //should be set immediately after ctor if applicable
        SourceLocation GeneralLocation;

        //for the current code block - created by ctor, but may be replaced during evaluation
        llvm::BasicBlock *BasicBlock = nullptr;
        std::shared_ptr<llvm::IRBuilder<>> IRBuilder;

        void CreateNewBlock(); //resets BasicBlock and IRBuilder - does NOT emit a branch to this new block

        VectorWithNameLookup<VariableDetails> LocalVariables;

    private:
        EvaluationContext *parent = nullptr;
        const FunctionDetails *functionTarget = nullptr;
        const EvaluationResult *classTarget = nullptr;
        std::string nameHint;

        std::string ComputeFriendlyBlockName() const;
    };

    class TypeSystem
    {
    public:
        TypeSystem(ModuleCompilerInternal &compiler);

        void CreateBuiltInTypes();
        void AddType(TypeDetails &&newType);

        TypeDetails& GetTypeDetails(const std::string &typeName, const SourceLocation &referencedSourceLocation);
        TypeDetails* TryGetTypeDetails(const std::string &typeName);
        TypeDetails& DereferenceType(const TypeDetails &type, const SourceLocation &referencedSourceLocation);

        EvaluationResult EvaluateLiteral(const ConstantLeaf &constant, const SourceLocation &definedSourceLocation, const TypeDetails *forceType = nullptr);

        bool TryMakeResultCompatibleWithType(const EvaluationResult &originalResult, const TypeDetails &targetType, llvm::IRBuilder<> &irBuilder, const SourceLocation &referencedSourceLocation, EvaluationResult &result, bool &truncationAvailable);
        EvaluationResult MakeResultCompatibleWithType(const EvaluationResult &originalResult, const TypeDetails &targetType, llvm::IRBuilder<> &irBuilder, const SourceLocation &referencedSourceLocation);

        EvaluationResult ExecuteUnaryOperator(const std::string &operation, const EvaluationResult &originalResult, const TypeDetails &operandType, llvm::IRBuilder<> &irBuilder, const SourceLocation &sourceLocation);
        EvaluationResult ExecuteBinaryOperator(const std::string &operation, const EvaluationResult &leftResult, const EvaluationResult &rightResult, const TypeDetails &operandType, llvm::IRBuilder<> &irBuilder, const SourceLocation &sourceLocation);

    private:
        ModuleCompilerInternal &compiler;

        std::unordered_map<std::string, TypeDetails> AllTypes;
    };

    struct GlobalItems
    {
        VectorWithNameLookup<VariableDetails> Variables;
        VectorWithNameLookup<FunctionDetails> Functions;
    };

    class ModuleCompilerInternal
    {
    public:
        CompiledModule &TargetModule;
        llvm::LLVMContext &LLVMContext;
        llvm::Module &LLVMModule;

        ModuleConfiguration Config;
        TypeSystem Types;
        GlobalItems Globals;

        const GenerationOptions &Options;

        ModuleCompilerInternal(CompiledModule &targetModule, llvm::Module &llvmModule, const GenerationOptions &options);

        void AddGlobalVariable(const VariableInformation &vi);
        VariableDetails& GetGlobalVariable(const std::string &name, const SourceLocation &referencedSourceLocation);
        void AddFunctionDeclaration(const FunctionInformation &fi, const ClassInformation *ci);
        void AddExternalFunctionDeclaration(const FunctionInformation &funcDecl);
        FunctionDetails& GetGlobalFunction(const std::string &name, const SourceLocation &referencedSourceLocation);
        bool AddClassDeclaration(const ClassInformation &ci, bool throwIfTypesCannotBeResolved);
        void AddFunctionBody(FunctionDetails &functionTarget, const FunctionInformation &fi, const EvaluationResult *classContext);
    };
}
