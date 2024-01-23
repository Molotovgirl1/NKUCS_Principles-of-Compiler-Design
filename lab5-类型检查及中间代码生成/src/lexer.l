%option noyywrap
%option nounput
%option noinput
%top{

    #include <stdarg.h>
    #include "common.h"
    #include "parser.h"
    #include "SymbolTable.h"
    #include<string>
    #include <ostream>
    #include <fstream>
    #include <iostream>
    using namespace std;
}
%{
    extern dump_type_t dump_type;

    inline void dump_tokens(const char* format, ...){
        va_list args;
        va_start(args, format);
        if (dump_type == TOKENS)
            vfprintf(yyout, format, args);
        va_end(args);
    }
    int colnum=0;
    int rownum=0;

%}
/* 正则表达式 */

INTEGER ([1-9][0-9]*|0)

/*八进制和十六进制*/
OCTAL (0[0-7][0-7]*)
HEXAL (0(x|X)[0-9a-fA-F][0-9a-fA-F]*)

/*浮点数*/
FLOATING ((([0-9]*[.][0-9]*([eE][+-]?[0-9]+)?)|([0-9]+[eE][+-]?[0-9]+))[fLlL]?)
HEXADECIMAL_FLOAT (0[xX](([0-9A-Fa-f]*[.][0-9A-Fa-f]*([pP][+-]?[0-9]+)?)|([0-9A-Fa-f]+[pP][+-]?[0-9]+))[fLlL]?)

/*标识符*/
ID [[:alpha:]_][[:alpha:][:digit:]_]*
EOL (\r\n|\n|\r)
WHITE [\t ]

/*注释相关*/
COMMENT (\/\/[^\n]*)
commentbegin "/*"
commentelement .
commentline \n
commentend "*/"
%x COMMENT
%x COMBLOCK

%%
{commentbegin} {BEGIN COMMENT;}
<COMMENT>{commentelement} {}
<COMMENT>{commentline} {yylineno++;}
<COMMENT>{commentend} {BEGIN INITIAL;}

"int" {
    dump_tokens("INT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return TYPE_INT;
}

"float" {
    dump_tokens("FLOAT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return TYPE_FLOAT;
}

"void" {
    dump_tokens("VOID\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return TYPE_VOID;
}
"const" {
    dump_tokens("CONST\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return CONST;
}
"if" {
    dump_tokens("IF\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return IF;
}
"else" {
    dump_tokens("ELSE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return ELSE;
}
"while" {
    dump_tokens("WHILE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return WHILE;
}
"break" {
    dump_tokens("BREAK\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return BREAK;
}
"continue" {
    dump_tokens("CONTINUE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return CONTINUE;
}
"return" {
   dump_tokens("RETURN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return RETURN;
}
"=" {
    dump_tokens("ASSIGN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return ASSIGN;
}
"==" {
    dump_tokens("EQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return EQ;
}
"!=" {
    dump_tokens("NEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return NEQ;
}
"<" {
    dump_tokens("LESS\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return LESS;
}
"<=" {
    dump_tokens("LEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return LESSEQ;
}
">" {
    dump_tokens("GREATER\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return GREAT;
}
">=" {
    dump_tokens("GEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return GREATEQ;
}
"+" {
    dump_tokens("ADD\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return ADD;
}
"-" {
    dump_tokens("SUB\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return SUB;
}
"*" {
    dump_tokens("MUL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return MUL;
}
"/" {
    dump_tokens("DIV\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return DIV;
}
"%" {
    dump_tokens("MOD\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return MOD;
}
"||" {
    dump_tokens("OR\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return OR;
}
"&&" {
    dump_tokens("AND\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return AND;
}
"!" {
    dump_tokens("NOT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return NOT;
}
"," {
    dump_tokens("COMMA\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return COMMA;
}
";" {
    dump_tokens("SEMICOLON\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return SEMICOLON;
}
"(" {
    dump_tokens("LPAREN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return LPAREN;
}
")" {
    dump_tokens("RPAREN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return RPAREN;
}
"[" {
    dump_tokens("LBRACKET\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return LBRACKET;
}
"]" {
    dump_tokens("RBRACKET\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return RBRACKET;
}
"{" {
    dump_tokens("LBRACE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return LBRACE;
}
"}" {
    dump_tokens("RBRACE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return RBRACE;
}
"getint" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::intType, {});//返回类型int，无参数
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"getch" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::intType, {});//返回类型int，无参数
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"getfloat" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::floatType, {});//返回类型int，无参数
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"getarray" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        std::vector<Type*> vec; //形参类型表
        Type* arrayType = new IntArrayType();
        ((IntArrayType*)arrayType)->pushBackDimension(-1);
        vec.push_back(arrayType);
        Type* funcType = new FunctionType(TypeSystem::intType, vec);//返回类型int，参数[int array]
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"getfarray" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    std::vector<Type*> vec; //形参类型表
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* arrayType = new FloatArrayType();
        ((FloatArrayType*)arrayType)->pushBackDimension(-1);
        vec.push_back(arrayType);
        Type* funcType = new FunctionType(TypeSystem::intType, vec);//返回类型int，参数[float array]
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"putint" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        std::vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::intType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数表[int]
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"putch" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        std::vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::intType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数表[int]
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"putfloat" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        std::vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::floatType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数表[int]
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"putarray" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        std::vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::intType);
        Type* arrayType = new IntArrayType();
        ((IntArrayType*)arrayType)->pushBackDimension(-1);
        vec.push_back(arrayType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数表[int, int array]
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"putfarray" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        std::vector<Type*> vec; //形参类型表
        vec.push_back(TypeSystem::intType);
        Type* arrayType = new FloatArrayType();
        ((FloatArrayType*)arrayType)->pushBackDimension(-1);
        vec.push_back(arrayType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//返回类型void，参数表[int, float array]
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"putf" {
    /*todo: implement putf*/
}
"starttime" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::voidType, {});//返回类型void，无参数
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
"stoptime" {
    char *lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    if(identifiers->lookup(yytext)==nullptr){//符号表内未找到，插入
        Type* funcType = new FunctionType(TypeSystem::voidType, {});//返回类型void，无参数
        SymbolTable* globalTable;    //全域符号表
        for(globalTable = identifiers;globalTable->getPrev();globalTable = globalTable->getPrev()); //全域符号表
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//作用域GLOBAL(0)
        globalTable->install(yytext, entry);
    }
    return ID;
}
{INTEGER} {
    int decimal;
    decimal = atoi(yytext);
    dump_tokens("DECIMAL\t%s\t%d\t%d\t%d\n", yytext, decimal,rownum,colnum);
    yylval.itype = decimal;
    colnum+=strlen(yytext);
    return INTEGER;
}
{FLOATING} {
    float temp;
    sscanf(yytext, "%f", &temp);
    dump_tokens("FLOAT\t%s\t%d\t%d\t%d\n", yytext, temp,rownum,colnum);
    yylval.ftype = double(temp);
    colnum+=strlen(yytext);
    return FLOATING;
}
{HEXADECIMAL_FLOAT} {
    float temp;
    sscanf(yytext, "%f", &temp);
    dump_tokens("FLOAT\t%s\t%d\t%d\t%d\n", yytext, temp,rownum,colnum);
    yylval.ftype = double(temp);
    colnum+=strlen(yytext);
    return FLOATING;
}

{ID} {
    char *lexeme;
    dump_tokens("ID\t%s\t%d\t%d\n", yytext,rownum,colnum);
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    // ID这个单词的属性值类型（语义类型）为strtype（char*类型），其属性就是词素
    yylval.strtype = lexeme;
    colnum+=strlen(yytext);
    return ID;
}
{OCTAL} {
    char* stop;
    int dec = strtol(yytext, &stop, 8);
    dump_tokens("OCTAL\t%s\t%d\t%d\t%d\n", yytext,rownum,colnum,stop);
    colnum+=strlen(yytext);
    yylval.itype = dec;
    return INTEGER;
}
{HEXAL} {
    char* stop;
    int dec = strtol(yytext, &stop, 16);
    dump_tokens("HEXADECIMAL\t%s\t%d\t%d\t%d\n", yytext,rownum,colnum,stop);
    colnum+=strlen(yytext); 
    yylval.itype = dec;
    return INTEGER;
}

{EOL} {
    yylineno++;
    rownum++;
    colnum=0;
}
{WHITE} {
}
{COMMENT}
%%
