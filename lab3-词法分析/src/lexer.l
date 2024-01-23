%option noyywrap
%option nounput
%option noinput
%top{
    #include <stdarg.h>
    #include "common.h"
    #include<string>
    using namespace std;
    #define Max_size 1000
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

    /* Your code here, if desired (lab3). */
    int colnum=0;
    int rownum=0;
struct SymNode { 
      string name;
      int lineno;
      int colno;
};
class SymTable {
private:
	int top;
	SymNode data[Max_size];
public:
	SymTable();
	void insert(string name,  int lineno, int colno);
	SymNode* lookfor(string s);
};
SymTable SymbolTable;
%}

/* definitions section */
DECIMIAL ([1-9][0-9]*|0)

/*新增八进制和十六进制*/
OCTAL (0[0-7]+)           
HEXADECIMAL (0[xX][0-9A-Fa-f]+)

/*新增float浮点数*/
FLOAT([0-9]+\.[0-9]*([eE][+-]?[0-9]+)?|[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)

/*十六进制进制浮点数*/
FLOAT_HEXADECIMAL (0[xX][0-9A-Fa-f]*\.[0-9A-Fa-f]+([pP][+-]?[0-9]+)?|0[xX][0-9A-Fa-f]*\.[0-9A-Fa-f]*([pP][+-]?[0-9]+)?|0[xX][0-9A-Fa-f]+[pP][+-]?[0-9]+)

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
    /* rules section */

"int" {
    dump_tokens("INT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"void" {
    dump_tokens("VOID\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"float" {
    dump_tokens("FLOAT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"const" {
    dump_tokens("CONST\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"if" {
    dump_tokens("IF\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"else" {
    dump_tokens("ELSE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"while" {
    dump_tokens("WHILE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"continue" {
    dump_tokens("CONTINUE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"break" {
    dump_tokens("BREAK\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"return" {
    dump_tokens("RETURN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"=" {
    dump_tokens("ASSIGN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"==" {
    dump_tokens("EQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"!=" {
    dump_tokens("NEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"<" {
    dump_tokens("LESS\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"<=" {
    dump_tokens("LEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

">" {
    dump_tokens("GREATER\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

">=" {
    dump_tokens("GEQUAL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"+" {
    dump_tokens("ADD\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"-" {
    dump_tokens("SUB\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"*" {
    dump_tokens("MUL\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"/" {
    dump_tokens("DIV\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"%" {
    dump_tokens("MOD\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"||" {
    dump_tokens("OR\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"&&" {
    dump_tokens("AND\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"!" {
    dump_tokens("NOT\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

";" {
    dump_tokens("SEMICOLON\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"," {
    dump_tokens("COMMA\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"(" {
    dump_tokens("LPAREN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

")" {
    dump_tokens("RPAREN\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"[" {
    dump_tokens("LBRACKET\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"]" {
    dump_tokens("RBRACKET\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"{" {
    dump_tokens("LBRACE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

"}" {
    dump_tokens("RBRACE\t%s\t%d\t%d\n", yytext,rownum,colnum);
    colnum+=strlen(yytext);
}

{DECIMIAL} {
    int decimal;
    decimal = atoi(yytext);
    dump_tokens("DECIMAL\t%s\t%d\t%d\t%d\n", yytext,rownum,colnum,decimal);
    colnum+=strlen(yytext);
}

{OCTAL} {
    int octal;
    sscanf(yytext, "%o", &octal);
    dump_tokens("OCTAL\t%s\t%d\t%d\t%d\n", yytext,rownum,colnum,octal);
    colnum+=strlen(yytext);
}

{HEXADECIMAL} {
    int hexadecimal;
    sscanf(yytext, "%x", &hexadecimal);
    dump_tokens("HEXADECIMAL\t%s\t%d\t%d\t%d\n", yytext,rownum,colnum,hexadecimal);
    colnum+=strlen(yytext);
}

{FLOAT} {
    float decimal;
    decimal = atof(yytext);
    dump_tokens("FLOAT\t%s\t%d\t%d\t%f\n", yytext,rownum,colnum,decimal);
    colnum+=strlen(yytext);
}

{FLOAT_HEXADECIMAL} {
    float Float1 = atof(yytext);
    dump_tokens("FLOAT_HEXADECIMAL\t%s\t%d\t%d\t%f\n", yytext, rownum, colnum, Float1);
    colnum+=strlen(yytext);
}

{ID} {
    string str = yytext; //yytext = 词素
    SymNode *p = SymbolTable.lookfor(str);
    if(p!=NULL)
    {
        dump_tokens("ID\t%s\t%d\t%d\t%p\n", yytext,rownum,colnum,p);
        colnum+=strlen(yytext);
    }
    else
    {
        SymbolTable.insert(yytext,rownum,colnum);
        SymNode *p = SymbolTable.lookfor(str);
        dump_tokens("ID\t%s\t%d\t%d\t%p\n", yytext,rownum,colnum,p);
        colnum+=strlen(yytext);
    }  
}

{EOL} {
    yylineno++;
    rownum++;
    colnum=0;
}

{WHITE}
{BLOCKCOMMENTBEGIN} {BEGIN BLOCKCOMMENT;}
<BLOCKCOMMENT>{BLOCKCOMMENTELEMENT} {}
<BLOCKCOMMENT>{EOL} {yylineno++;rownum++;}
<BLOCKCOMMENT>{BLOCKCOMMENTEND} {BEGIN INITIAL;}
{LINECOMMENT} {
    // 单行注释，不做任何处理
}
    /*  Your code here (lab3). */
%%
/* user code section */
SymTable::SymTable(){
    top=-1;
}
void SymTable::insert(string name, int lineno, int colno){
    top++;
    data[top].name=name;
    data[top].lineno=lineno;
    data[top].colno=colno;
}
SymNode* SymTable::lookfor(string s){  
    if(top==-1)
    {
       return NULL;
     }
    for(int i=0;i<=top;i++){
        if(data[i].name==s)
           {
           return data+i;
           }
        }
        return NULL;
}
