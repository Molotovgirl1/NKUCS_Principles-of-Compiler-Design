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

DECIMAL_FLOAT ((([0-9]*[.][0-9]*([eE][+-]?[0-9]+)?)|([0-9]+[eE][+-]?[0-9]+))[fLlL]?)
HEXADECIMAL_FLOAT (0[xX](([0-9A-Fa-f]*[.][0-9A-Fa-f]*([pP][+-]?[0-9]+)?)|([0-9A-Fa-f]+[pP][+-]?[0-9]+))[fLlL]?)
DECIMIAL ([1-9][0-9]*|0)
OCTAL (0[0-7]+)
HEXADECIMAL (0[xX][0-9A-Fa-f]+)
ID [[:alpha:]_][[:alpha:][:digit:]_]*
EOL (\r\n|\n|\r)
WHITE [\t ]
BLOCKCOMMENTBEGIN \/\*
BLOCKCOMMENTELEMENT .
BLOCKCOMMENTEND \*\/
%x BLOCKCOMMENT
LINECOMMENT \/\/[^\n]*

%%
"int" {
    dump_tokens("INT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return INT;
}

"float" {
    dump_tokens("FLOAT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return FLOAT;
}

"void" {
    dump_tokens("VOID\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return VOID;
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
    return EQUAL;
}
"!=" {
    dump_tokens("NEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return NOTEQUAL;
}
"<" {
    dump_tokens("LESS\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return LESS;
}
"<=" {
    dump_tokens("LEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return LESSEQUAL;
}
">" {
    dump_tokens("GREATER\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return GREATER;
}
">=" {
    dump_tokens("GEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return GREATEREQUAL;
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

{DECIMIAL} {
    int decimal;
    decimal = atoi(yytext);
    dump_tokens("DECIMAL\t%s\t%d\t%d\t%d\n", yytext, decimal,rownum,colnum);
    yylval.inttype = decimal;
    colnum+=strlen(yytext);
    return INTEGER;
}
{DECIMAL_FLOAT} {
    float temp;
    sscanf(yytext, "%f", &temp);
    dump_tokens("FLOAT\t%s\t%d\t%d\t%d\n", yytext, temp,rownum,colnum);
    yylval.floattype = double(temp);
    colnum+=strlen(yytext);
    return FLOATNUM;
}
{HEXADECIMAL_FLOAT} {
    float temp;
    sscanf(yytext, "%f", &temp);
    dump_tokens("FLOAT\t%s\t%d\t%d\t%d\n", yytext, temp,rownum,colnum);
    yylval.floattype = double(temp);
    colnum+=strlen(yytext);
    return FLOATNUM;
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
    yylval.inttype = dec;
    return INTEGER;
}
{HEXADECIMAL} {
    char* stop;
    int dec = strtol(yytext, &stop, 16);
    dump_tokens("HEXADECIMAL\t%s\t%d\t%d\t%d\n", yytext,rownum,colnum,stop);
    colnum+=strlen(yytext); 
    yylval.inttype = dec;
    return INTEGER;
}

{EOL} {
    yylineno++;
    rownum++;
    colnum=0;
}
{WHITE}
{LINECOMMENT} {}
{BLOCKCOMMENTBEGIN} {BEGIN BLOCKCOMMENT;}
<BLOCKCOMMENT>{BLOCKCOMMENTELEMENT} {}
<BLOCKCOMMENT>{BLOCKCOMMENTEND} {BEGIN INITIAL;}
%%

