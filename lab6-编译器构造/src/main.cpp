#include <iostream>
#include <string.h>
#include <unistd.h>
#include "common.h"
#include "Ast.h"
#include "Unit.h"
#include "MachineCode.h"
#include "LinearScan.h"

extern FILE *yyin;
extern FILE *yyout;
int yyparse();

Ast ast;
Unit unit;
MachineUnit mUnit;
char outfile[256] = "a.out";
dump_type_t dump_type = ASM;

int main(int argc, char *argv[])
{
    int opt;
    while ((opt = getopt(argc, argv, "Siato:")) != -1)
    {
        switch (opt)
        {
        case 'o':
            strcpy(outfile, optarg);
            break;
        case 'a':
            dump_type = AST;
            break;
        case 't':
            dump_type = TOKENS;
            break;
        case 'i':
            dump_type = IR;
            break;
        case 'S':
            dump_type = ASM;
            break;
        default:
            fprintf(stderr, "Usage: %s [-o outfile] infile\n", argv[0]);
            exit(EXIT_FAILURE);
            break;
        }
    }
    if (optind >= argc)
    {
        fprintf(stderr, "no input file\n");
        exit(EXIT_FAILURE);
    }
    if (!(yyin = fopen(argv[optind], "r")))
    {
        fprintf(stderr, "%s: No such file or directory\nno input file\n", argv[optind]);
        exit(EXIT_FAILURE);
    }
    if (!(yyout = fopen(outfile, "w")))
    {
        fprintf(stderr, "%s: fail to open output file\n", outfile);
        exit(EXIT_FAILURE);
    }

    //与上次不同 这次要包含函数的类型
    SymbolEntry *se1,*se2,*se3,*se4,*se5,*se6,*se7,*se8,*se9,*se10;
    Type *funcType1,*funcType2,*funcType3,*funcType4,*funcType5,*funcType6,*funcType7,*funcType8,*funcType9,*funcType10;
    //其实PUTF只要用这里的就够了
    //getint没有参数 
    funcType1 = new FunctionType(TypeSystem::intType,{},0);
    se1 = new IdentifierSymbolEntry(funcType1, "getint", identifiers->getLevel(), true);   
    //getch没有参数 
    funcType2 = new FunctionType(TypeSystem::intType,{},0);
    se2 = new IdentifierSymbolEntry(funcType2, "getch", identifiers->getLevel(), true);    
    //putint 有一个int参数
    funcType3 = new FunctionType(TypeSystem::voidType,{TypeSystem::intType},1);
    se3 = new IdentifierSymbolEntry(funcType3, "putint", identifiers->getLevel(), true);  
    //putch 有一个int参数
    funcType4 = new FunctionType(TypeSystem::voidType,{TypeSystem::intType},1);
    se4 = new IdentifierSymbolEntry(funcType4, "putch", identifiers->getLevel(), true);   
    //getarray 1个参数
    std::vector<Type*> paramType1;
    paramType1.push_back(TypeSystem::intType);
    funcType5 = new FunctionType(TypeSystem::intType,paramType1, 1);
    se5 = new IdentifierSymbolEntry(funcType5, "getarray", identifiers->getLevel(), true);
    //putarray 2个参数
    std::vector<Type*> paramType2;
    paramType2.push_back(TypeSystem::intType);
    paramType2.push_back(new PointerType(new ArrayType({})));
    funcType6 = new FunctionType(TypeSystem::voidType, paramType2, 2);
    se6 = new IdentifierSymbolEntry(funcType6, "putarray", identifiers->getLevel(), true);
    //putfloat 1个参数
    funcType7 = new FunctionType(TypeSystem::voidType,{TypeSystem::floatType},1);
    se7 = new IdentifierSymbolEntry(funcType7, "putfloat", identifiers->getLevel(), true);
    //getfloat 0个参数
    funcType8 = new FunctionType(TypeSystem::floatType,{},0);
    se8 = new IdentifierSymbolEntry(funcType8, "getfloat", identifiers->getLevel(), true);
    //putfarray 2个参数
    std::vector<Type*> paramType3;
    paramType3.push_back(TypeSystem::intType);
    paramType3.push_back(new PointerType(new ArrayType({}, TypeSystem::floatType)));
    funcType9 = new FunctionType(TypeSystem::voidType, paramType3, 2);
    se9 = new IdentifierSymbolEntry(funcType9, "putfarray", identifiers->getLevel(), true);
    //getfarray 1个参数
    funcType10 = new FunctionType(TypeSystem::intType, {new PointerType(new ArrayType({}, TypeSystem::floatType))}, 1);
    se10 = new IdentifierSymbolEntry(funcType10, "getfarray", identifiers->getLevel(), true);
    //将sysy运行时库函数加入符号表中
    identifiers->install("getint",se1);
    identifiers->install("getch",se2);
    identifiers->install("putint",se3);
    identifiers->install("putch", se4);
    identifiers->install("getarray",se5);
    identifiers->install("putarray",se6);
    identifiers->install("putfloat",se7);
    identifiers->install("getfloat",se8);
    identifiers->install("putfarray",se9);
    identifiers->install("getfarray",se10);
    unit.insertDeclare(se1);
    unit.insertDeclare(se2);
    unit.insertDeclare(se3);
    unit.insertDeclare(se4);
    unit.insertDeclare(se5);
    unit.insertDeclare(se6);
    unit.insertDeclare(se7);
    unit.insertDeclare(se8);
    unit.insertDeclare(se9);
    unit.insertDeclare(se10);
    
    yyparse();
    if(dump_type == AST)
        ast.output();
    ast.typeCheck();
    ast.genCode(&unit);
    if(dump_type == IR)
        unit.output();
    unit.genMachineCode(&mUnit);
    LinearScan linearScan(&mUnit);
    linearScan.allocateRegisters();
    if(dump_type == ASM)
        mUnit.output();
    return 0;
}