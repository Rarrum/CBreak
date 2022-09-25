// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include "CompiledOutput.h"

#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/IR/LegacyPassManager.h>

namespace
{
    class LLVMVectorOStream : public llvm::raw_ostream
    {
    public:
        LLVMVectorOStream(std::vector<char> &vec):
            vec(vec)
        {
            SetUnbuffered();
        }

        ~LLVMVectorOStream()
        {
            flush();
        }

    private:
        std::vector<char> &vec;

        void write_impl(const char *Ptr, size_t Size) override
        {
            vec.insert(vec.end(), Ptr, Ptr + Size);
        }

        uint64_t current_pos() const override
        {
            return vec.size();
        }
    };

    class LLVMVectorPStream : public llvm::raw_pwrite_stream
    {
    public:
        LLVMVectorPStream(std::vector<char> &vec):
            vec(vec)
        {
            SetUnbuffered();
        }

        ~LLVMVectorPStream()
        {
            flush();
        }

    private:
        std::vector<char> &vec;

        void pwrite_impl(const char * Ptr, size_t Size, uint64_t Offset) override
        {
            if (Offset == vec.size())
                vec.insert(vec.end(), Ptr, Ptr + Size);
            else if (Offset + Size > vec.size())
                throw CBreakCompiler::IRProcessingFailedException{.Message = "pwrite_impl call invalid"};
            else
                memcpy(vec.data() + Offset, Ptr, Size);
        }

        void write_impl(const char *Ptr, size_t Size) override
        {
            vec.insert(vec.end(), Ptr, Ptr + Size);
        }

        uint64_t current_pos() const override
        {
            return vec.size();
        }
    };
}

namespace CBreakCompiler
{
    std::vector<char> GenerateOutput(const CompiledModule &module, OutputFormat outputFormat)
    {
        std::vector<char> outputBytes;

        if (outputFormat == OutputFormat::IRText)
        {
            LLVMVectorOStream outputStream{outputBytes};
            module.LLVMModule->print(outputStream, nullptr);
        }
        else if (outputFormat == OutputFormat::Bitcode)
        {
            LLVMVectorOStream outputStream{outputBytes};
            llvm::WriteBitcodeToFile(*module.LLVMModule, outputStream);
        }
        else if (outputFormat == OutputFormat::NativeObject || outputFormat == OutputFormat::AsmText)
        {
            llvm::InitializeNativeTarget();
            llvm::InitializeNativeTargetAsmPrinter();

            std::string targetTriple = llvm::sys::getDefaultTargetTriple();

            std::string error;
            const llvm::Target *target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
            if (!target)
                throw IRProcessingFailedException{.Message = error};

            llvm::TargetOptions targetOptions;
            auto relocModel = llvm::Optional<llvm::Reloc::Model>();
            llvm::TargetMachine* targetMachine = target->createTargetMachine(targetTriple, "generic", "", targetOptions, relocModel);

            module.LLVMModule->setDataLayout(targetMachine->createDataLayout());
            module.LLVMModule->setTargetTriple(targetTriple);

            llvm::legacy::PassManager pass;
            llvm::CodeGenFileType passOutputType = (outputFormat == OutputFormat::NativeObject ? llvm::CGFT_ObjectFile: llvm::CGFT_AssemblyFile);

            LLVMVectorPStream outputStream{outputBytes};
            if (targetMachine->addPassesToEmitFile(pass, outputStream, nullptr, passOutputType))
                throw IRProcessingFailedException{.Message = "TargetMachine can't emit a file of this type"};

            pass.run(*module.LLVMModule);
        }
        else
            throw IRProcessingFailedException{.Message = "Bad OutputFormat."};

        return outputBytes;
    }
}
