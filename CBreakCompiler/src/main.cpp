// Copyright (c) Luke Lenhart.  See LICENSE.txt for details.

#include <string>
#include <iostream>
#include <fstream>
#include <filesystem>

#include "Parser.h"
#include "ModuleCompiler.h"
#include "CompiledOutput.h"

namespace
{
    //Arguments:
    std::vector<std::string> InputFiles;
    std::string OutputFile;
    CBreakCompiler::GenerationOptions GenOptions;
    CBreakCompiler::OutputFormat OutputFormat = CBreakCompiler::OutputFormat::NativeObject;

    void PrintHelp()
    {
        std::cout<<"Usage: <InputFile0> [InputFile1...] [options]"<<std::endl;
        std::cout<<"  -o <File> - Place output into <File>.  <File> may be stdout."<<std::endl;
        std::cout<<"  -otype <Type> -  What to output: none or irtext, asmtext, bitcode, or nativeobject(default)."<<std::endl;
        std::cout<<"  -nollvmverify - Disables llvm's verifier."<<std::endl;
    }

    int ParseCommandLine(int argc, char **argv)
    {
        if (argc <= 1)
        {
            PrintHelp();
            return -1;
        }

        std::string nextOption;

        for (int i = 1; i < argc; ++i)
        {
            std::string arg = argv[i];

            if (arg == "-?" || arg == "-help" || arg == "--help")
            {
                PrintHelp();
                return -1;
            }
            else if (!nextOption.empty())
            {
                if (nextOption == "-o")
                    OutputFile = arg;
                else if (nextOption == "-otype")
                {
                    if (arg == "irtext")
                        OutputFormat = CBreakCompiler::OutputFormat::IRText;
                    else if (arg == "asmtext")
                        OutputFormat = CBreakCompiler::OutputFormat::AsmText;
                    else if (arg == "bitcode")
                        OutputFormat = CBreakCompiler::OutputFormat::Bitcode;
                    else if (arg == "nativeobject")
                        OutputFormat = CBreakCompiler::OutputFormat::NativeObject;
                    else
                    {
                        std::cout<<"Bad output type specified."<<std::endl;
                        return -1;
                    }
                }
                else
                {
                    std::cout<<"Unrecognized option: "<<nextOption<<std::endl;
                    return -1;
                }

                nextOption.clear();
            }
            else if (arg == "-nollvmverify")
                GenOptions.DisableLLVMVerifier = true;
            else if (arg.size() >= 1 && arg[0] == '-')
                nextOption = arg;
            else
                InputFiles.emplace_back(arg);
        }

        if (!nextOption.empty())
        {
            std::cout<<"Incomplete commandline argument."<<std::endl;
            return -1;
        }

        if (InputFiles.empty())
        {
            std::cout<<"At least 1 input file must be provided."<<std::endl;
            return -1;
        }

        return 0;
    }
}

int main(int argc, char **argv)
{
    int commandLineResult = ParseCommandLine(argc, argv);
    if (commandLineResult)
        return commandLineResult;

    std::vector<CBreakCompiler::ParsedModuleFragment> parsedFragments;
    CBreakCompiler::DefaultModuleImporter moduleImporter;

    for (const std::string inputFileName : InputFiles)
    {
        //load input file
        std::cout<<"Reading "<<inputFileName<<" ..."<<std::endl;
        std::error_code errorCode;
        size_t estimatedSize = std::filesystem::file_size(inputFileName, errorCode);
        std::ifstream inputFile{inputFileName, std::ios::binary | std::ios::in};
        if (!inputFile.is_open() || errorCode)
        {
            std::cout<<"Could not open input file: " + inputFileName<<std::endl;
            return 1;
        }

        std::string sourceText;
        sourceText.reserve(estimatedSize);

        sourceText.assign((std::istreambuf_iterator<char>(inputFile)), std::istreambuf_iterator<char>());

        //parse source
        std::cout<<"Parsing "<<inputFileName<<" ..."<<std::endl;
        try
        {
            parsedFragments.emplace_back(CBreakCompiler::ParseSource(sourceText, inputFileName));
        }
        catch (const CBreakCompiler::ParseFailedException &ex)
        {
            std::cout<<ex.Location.FileName<<":"<<ex.Location.Location.Line<<":"<<ex.Location.Location.Column<<": error: "<<ex.Message<<std::endl;
            if (!ex.OtherLocation.FileName.empty())
                std::cout<<"  (Other location: "<<ex.OtherLocation.FileName<<":"<<ex.OtherLocation.Location.Line<<":"<<ex.OtherLocation.Location.Column<<")";
            return 1;
        }
    }

    //compile source to llvm IR
    std::cout<<"Generating Module ..."<<std::endl;
    std::unique_ptr<llvm::LLVMContext> llvmContext = std::make_unique<llvm::LLVMContext>();
    CBreakCompiler::CompiledModule module;
    try
    {
        module = CBreakCompiler::GenerateModule(*llvmContext, parsedFragments, &moduleImporter, GenOptions);
    }
    catch (const CBreakCompiler::CodeValidationException &ex)
    {
        std::cout<<ex.Location.FileName<<":"<<ex.Location.Location.Line<<":"<<ex.Location.Location.Column<<": error: "<<ex.Message;
        if (!ex.OtherLocation.FileName.empty())
            std::cout<<"  (Other location: "<<ex.OtherLocation.FileName<<":"<<ex.OtherLocation.Location.Line<<":"<<ex.OtherLocation.Location.Column<<")";
        std::cout<<std::endl;

        std::cout<<"\nGenerated IR:"<<std::endl;
        std::vector<char> irOutput = CBreakCompiler::GenerateOutput(ex.PartialModule, CBreakCompiler::OutputFormat::IRText);
        std::cout<<std::string{irOutput.data(), irOutput.size()}<<std::endl;

        return 1;
    }
    catch (const CBreakCompiler::CodeGenerationFailedException &ex)
    {
        std::cout<<ex.Location.FileName<<":"<<ex.Location.Location.Line<<":"<<ex.Location.Location.Column<<": error: "<<ex.Message;
        if (!ex.OtherLocation.FileName.empty())
            std::cout<<"  (Other location: "<<ex.OtherLocation.FileName<<":"<<ex.OtherLocation.Location.Line<<":"<<ex.OtherLocation.Location.Column<<")";
        std::cout<<std::endl;
        return 1;
    }

    //convert the ir to something useful
    std::cout<<"Generating output ..."<<std::endl;
    std::vector<char> finalOutput;
    try
    {
        finalOutput = CBreakCompiler::GenerateOutput(module, OutputFormat);
    }
    catch (const CBreakCompiler::IRProcessingFailedException &ex)
    {
        std::cout<<"IRProcessingFailed: "<<ex.Message<<std::endl;
        return 1;
    }

    if (OutputFile.empty())
    {
        std::cout<<"No output file specified.  Result is "<<finalOutput.size()<<" bytes."<<std::endl;
    }
    else
    {
        if (OutputFile == "stdout")
            std::cout<<"\nOutput:\n\n"<<std::string{finalOutput.data(), finalOutput.size()}<<std::endl;
        else
        {
            std::cout<<"Writing "<<OutputFile<<" ..."<<std::endl;
            std::ofstream outputFile(OutputFile, std::ios::binary | std::ios::out);
            outputFile.write(finalOutput.data(), finalOutput.size());
        }
    }

    std::cout<<"Done."<<std::endl;
    return 0;
}
