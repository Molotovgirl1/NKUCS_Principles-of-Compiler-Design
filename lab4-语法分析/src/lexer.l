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

DECIMIAL ([1-9][0-9]*|0)

/*新增八进制和十六进制*/
OCTAL (0[0-7]+)           
HEXADECIMAL (0[xX][0-9A-Fa-f]+)

/*新增注释相关*/

/* 块注释 */
%x BLOCKCOMMENT
BLOCKCOMMENTBEGIN "/*"
BLOCKCOMMENTELEMENT .|EOL
BLOCKCOMMENTEND "*/"
 //单行注释：以双斜杠开头，后跟若干个非换行的字符
LINECOMMENT \/\/[^\n]*

ID [[:alpha:]_][[:alpha:][:digit:]_]*
EOL (\r\n|\n|\r)
WHITE [\t ]

/*  Your code here (lab3). */

%%
    /* 规则段 */
"int" {
    dump_tokens("INT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return INT;
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

"return" {
    dump_tokens("RETURN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return RETURN;
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

"=" {
    dump_tokens("ASSIGN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return ASSIGN;
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

"&&" {
    dump_tokens("AND\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return AND;
}

"||" {
    dump_tokens("OR\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return OR;
}

"!" {
    dump_tokens("NOT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return NOT;
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

"," {
    dump_tokens("COMMA\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
    return COMMA;
}

{DECIMIAL} {
    int decimal;
    decimal = atoi(yytext);
    dump_tokens("DECIMAL\t%s\t%d\t%d\t%d\n", yytext, decimal,rownum,colnum);
    yylval.itype = decimal;
    colnum+=strlen(yytext);
    return INTEGER;
}

{OCTAL} {
    int octal;
    sscanf(yytext, "%o", &octal);
    dump_tokens("OCTAL\t%s\t%d\t%d\t%d\n", yytext,rownum,colnum,octal);
    colnum+=strlen(yytext);
    yylval.itype = octal;
    return INTEGER;
}

{HEXADECIMAL} {
    int hexadecimal;
    sscanf(yytext, "%x", &hexadecimal);
    dump_tokens("HEXADECIMAL\t%s\t%d\t%d\t%d\n", yytext,rownum,colnum,hexadecimal);
    colnum+=strlen(yytext); 
    yylval.itype = hexadecimal;
    return INTEGER;
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

<*>{EOL} {              // EOL回车换行符
    yylineno++;
    rownum++;
    colnum=0;
}
{WHITE}
{LINECOMMENT} {}   //处理注释，使用定义好的独占状态BLOCKCOMMENT
{BLOCKCOMMENTBEGIN} {BEGIN BLOCKCOMMENT;}
<BLOCKCOMMENT>{BLOCKCOMMENTELEMENT} {}
<BLOCKCOMMENT>{BLOCKCOMMENTEND} {BEGIN INITIAL;} //使用宏BEGIN来切换状态，初始状态默认为INITIAL 

%%

