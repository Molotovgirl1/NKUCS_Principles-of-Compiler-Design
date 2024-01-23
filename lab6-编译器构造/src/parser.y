%code top{
    #include "parser.h"
    #include <iostream>
    #include <assert.h>
    #include <stack>
    #include <utility>
    #include <cstring>
    extern Ast ast;
    int yylex();
    #define YYERROR_VERBOSE 1
    int yyerror(char const*);
    std::stack<StmtNode*> whileStack;
    Type *recentVarType;
    Type *recentFuncRetType;
    int spillPos = 1; // 记录当前参数溢出位置
    int intArgNum = 0; // 记录当前函数参数是第几个参数，前四个参数用寄存器存，之后的参数用栈传递
    int floatArgNum = 0;
    std::stack<std::vector<int>> dimesionStack; // 维度栈
    ExprNode **initArray = nullptr; // 存储当前数组的初始化数组的基地址
    int idx; // 上边initArray的索引
}

%code requires {
    #include "Ast.h"
    #include "Type.h"
    #include "SymbolTable.h"
}

%union {
    int inttype;
    double floattype;
    char* strtype;
    Type* type;
    SymbolEntry* se;
    ExprNode* exprtype;
    StmtNode* stmttype;
}

%start Program
%token <strtype> ID
%token <inttype> INTEGER
%token <floattype> FLOATNUM
%token INT FLOAT VOID CONST
%token ADD SUB MUL DIV MOD OR AND LESS LESSEQUAL GREATER GREATEREQUAL ASSIGN EQUAL NOTEQUAL NOT
%token IF ELSE WHILE
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMICOLON
%token RETURN CONTINUE BREAK

%type<stmttype> Stmts Stmt AssignStmt ExprStmt BlockStmt IfStmt WhileStmt BreakStmt ContinueStmt ReturnStmt DeclStmt FuncDef ConstDeclStmt VarDeclStmt ConstDefList VarDef ConstDef VarDefList FuncFParam FuncFParams FuncFParamsList BlankStmt VarDefSingle VarDefAssign
%type<exprtype> Exp AddExp Cond LOrExp PrimaryExp LVal RelExp LAndExp MulExp ConstExp EqExp UnaryExp InitVal ConstInitVal FuncRParams FuncRParamsList ArrayIndices
%type<type> Type

%precedence THEN
%precedence ELSE
%%
Program
    : Stmts {
        ast.setRoot($1);
    }
    ;
Stmts
    : Stmt { $$ = $1; }
    | Stmts Stmt{
        $$ = new SeqNode($1, $2);
    }
    ;
Stmt
    : AssignStmt { $$ = $1; }
    | ExprStmt { $$ = $1; }
    | BlockStmt { $$ = $1; }
    | BlankStmt { $$ = $1; }
    | IfStmt { $$ = $1; }
    | WhileStmt { $$ = $1; }
    | BreakStmt { $$ = $1; }
    | ContinueStmt { $$ = $1; }
    | ReturnStmt { $$ = $1; }
    | DeclStmt { $$ = $1; }
    | FuncDef { $$ = $1; }
    ;
LVal
    : ID {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        if(se == nullptr)
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);
            delete []$1;
            exit(-1);
        }
        $$ = new Id(se);
        delete []$1;
    }
    | ID ArrayIndices {
        SymbolEntry* se = identifiers->lookup($1);
        $$ = new Id(se, $2);
        delete []$1;
    }
    ; 
AssignStmt
    : LVal ASSIGN Exp SEMICOLON {
        $$ = new AssignStmt($1, $3);
    }
    ;
ExprStmt
    : Exp SEMICOLON {
        $$ = new ExprStmt($1);
    }
    ;
BlankStmt
    : SEMICOLON {
        $$ = new BlankStmt();
    }
    ;
BlockStmt
    : LBRACE {
        identifiers = new SymbolTable(identifiers);
    } 
      Stmts RBRACE {
        $$ = new CompoundStmt($3);
        SymbolTable* top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
    }
    | LBRACE RBRACE {
        $$ = new CompoundStmt();
    }
    ;
IfStmt
    : IF LPAREN Cond RPAREN Stmt %prec THEN {
        //((ExprNode*)$3)->isBr=true;
        $$ = new IfStmt($3, $5);
    }
    | IF LPAREN Cond RPAREN Stmt ELSE Stmt {
        //((ExprNode*)$3)->isBr=true;
        $$ = new IfElseStmt($3, $5, $7);
    }
    ;
WhileStmt
    : WHILE LPAREN Cond RPAREN {
        //((ExprNode*)$3)->isBr=true;
        StmtNode *whileStmt = new WhileStmt($3);
        whileStack.push(whileStmt);
    } Stmt {
        $$ = whileStack.top();
        whileStack.pop();
        ((WhileStmt*)$$)->setStmt($6);
    }
    ;
BreakStmt
    : BREAK SEMICOLON {
        if (whileStack.size() <= 0) {
            $$ = new BreakStmt(nullptr);
        }
        else {
            $$ = new BreakStmt(whileStack.top());
        }
    }
    ;
ContinueStmt
    : CONTINUE SEMICOLON {
        if (whileStack.size() <= 0) {
            $$ = new ContinueStmt(nullptr);
        }
        else {
            $$ = new ContinueStmt(whileStack.top());
        }
    }
    ;
ReturnStmt
    : RETURN SEMICOLON {
        $$ = new ReturnStmt(nullptr, recentFuncRetType);
    }
    | RETURN Exp SEMICOLON {
        $$ = new ReturnStmt($2, recentFuncRetType);
    }
    ;
Exp
    :
    AddExp {$$ = $1;}
    ;
Cond
    :
    LOrExp {$$ = $1;$$->isBr=true;}
    ;
PrimaryExp
    : LPAREN Exp RPAREN {
        $$ = $2;
    }
    | LVal {
        $$ = $1;
    }
    | INTEGER {
        SymbolEntry* se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se);
    }
    | FLOATNUM {
        SymbolEntry* se = new ConstantSymbolEntry(TypeSystem::floatType, $1);
        $$ = new Constant(se);
    }
    ;
UnaryExp 
    : PrimaryExp {$$ = $1;}
    | ID LPAREN FuncRParamsList RPAREN {
        SymbolEntry* se;
        se = globals->lookup($1);
        // 判断函数未定义
        if(se == nullptr)
        {
            fprintf(stderr, "function \"%s\" is undefined\n", (char*)$1);
            delete []$1;
            assert(se != nullptr);
        }
        $$ = new CallExpr(se, $3);
    }
    | ADD UnaryExp {$$ = $2;}
    | SUB UnaryExp {
        SymbolEntry* se;
        if ($2->getType()->isFloat())
            se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
        else
            se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
    }
    | NOT UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::NOT, $2);
    }
    ;
MulExp
    : UnaryExp {$$ = $1;}
    | MulExp MUL UnaryExp {
        SymbolEntry* se;
        if ($1->getType()->isFloat() || $3->getType()->isFloat())
            se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
        else 
            se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
    }
    | MulExp DIV UnaryExp {
        SymbolEntry* se;
        if ($1->getType()->isFloat() || $3->getType()->isFloat())
            se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
        else 
            se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
    }
    | MulExp MOD UnaryExp {
        if ($1->getType()->isFloat() || $3->getType()->isFloat()) { 
            // 这一点留作错误处理，mod两个操作数不能为float

        }
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;
AddExp
    : MulExp {$$ = $1;}
    | AddExp ADD MulExp {
        SymbolEntry* se;
        if ($1->getType()->isFloat() || $3->getType()->isFloat())
            se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
        else 
            se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
    }
    | AddExp SUB MulExp {
        SymbolEntry* se;
        if ($1->getType()->isFloat() || $3->getType()->isFloat())
            se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
        else 
            se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
    }
    ;
RelExp
    : AddExp {$$ = $1;}
    | RelExp LESS AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
    }
    | RelExp LESSEQUAL AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESSEQUAL, $1, $3);
    }
    | RelExp GREATER AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3);
    }
    | RelExp GREATEREQUAL AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATEREQUAL, $1, $3);
    }
    ;
EqExp
    : RelExp {$$ = $1;}
    | EqExp EQUAL RelExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::EQUAL, $1, $3);
    }
    | EqExp NOTEQUAL RelExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::NOTEQUAL, $1, $3);
    }
    ;
LAndExp
    : EqExp {$$ = $1;}
    | LAndExp AND EqExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
    }
    ;
LOrExp
    : LAndExp {$$ = $1;}
    | LOrExp OR LAndExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
    }
    ;
ConstExp
    : AddExp {$$ = $1;}
    ;
FuncRParamsList
    : FuncRParams {$$ = $1;}
    | %empty {$$ = nullptr;}
    ;
FuncRParams 
    : Exp {$$ = $1;}
    | FuncRParams COMMA Exp {
        $$ = $1;
        $$->setNext($3);
    }
    ;
Type
    : INT {
        $$ = TypeSystem::intType;
        recentVarType = $$;
    }
    | FLOAT {
        $$ = TypeSystem::floatType;
        recentVarType = $$;
    }
    | VOID {
        $$ = TypeSystem::voidType;
    }
    ;
DeclStmt
    : VarDeclStmt {$$ = $1;}
    | ConstDeclStmt {$$ = $1;}
    ;
VarDeclStmt
    : Type VarDefList SEMICOLON {
        $$ = $2;
    }
    ;
VarDefList
    : VarDefList COMMA VarDef {
        $$ = $1;
        $1->setNext($3);
    } 
    | VarDef { $$ = $1; }
    ;
VarDef
    : VarDefSingle {
        $$=$1;
    }
    | VarDefAssign{$$=$1;}
    ;
VarDefSingle
    : ID {
        SymbolEntry* se;
        se = identifiers->lookup($1, true);
        if (se != nullptr) { //重复定义了
            fprintf(stderr, "variable \"%s\" is repeated declared\n", (char*)$1);
            delete []$1;
            assert(se == nullptr);
        }
        se = new IdentifierSymbolEntry(recentVarType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se));
        delete []$1;
    }
    | ID ArrayIndices {
        SymbolEntry* se;
        se = identifiers->lookup($1, true);
        if (se != nullptr) { //重复定义了
            fprintf(stderr, "variable \"%s\" is repeated declared\n", (char*)$1);
            delete []$1;
            assert(se == nullptr);
        }
        ExprNode *expr = $2;
        std::vector<int> indexs;
        while (expr) {
            indexs.push_back(expr->getValue());
            expr = (ExprNode*)expr->getNext();
        }
        Type *arrType = new ArrayType(indexs, recentVarType);
        se = new IdentifierSymbolEntry(arrType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se));
        delete []$1;
    }
    ;
VarDefAssign
    :ID ASSIGN InitVal {
        SymbolEntry* se;
        se = identifiers->lookup($1, true);
        if (se != nullptr) { //重复定义了
            fprintf(stderr, "variable \"%s\" is repeated declared\n", (char*)$1);
            delete []$1;
            assert(se == nullptr);
        }
        se = new IdentifierSymbolEntry(recentVarType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        ((IdentifierSymbolEntry*)se)->setValue($3->getValue());
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1;
    }
    | ID ArrayIndices ASSIGN {
        SymbolEntry* se;
        se = identifiers->lookup($1, true);
        if (se != nullptr) { //重复定义了
            fprintf(stderr, "variable \"%s\" is repeated declared\n", (char*)$1);
            delete []$1;
            assert(se == nullptr);
        }
        ExprNode *expr = $2;
        std::vector<int> indexs;
        while (expr) {
            indexs.push_back(expr->getValue());
            expr = (ExprNode*)expr->getNext();
        }
        Type *arrType = new ArrayType(indexs, recentVarType);
        se = new IdentifierSymbolEntry(arrType, $1, identifiers->getLevel());
        $<se>$ = se;
        identifiers->install($1, se);
        initArray = new ExprNode*[arrType->getSize() / TypeSystem::intType->getSize()];
        memset(initArray, 0, sizeof(ExprNode*) * (arrType->getSize() / TypeSystem::intType->getSize()));
        idx = 0;
        std::stack<std::vector<int>>().swap(dimesionStack); // 这一句的作用就是清空栈
        dimesionStack.push(indexs);
        delete []$1;
    } InitVal {
        $$ = new DeclStmt(new Id($<se>4, $2));
        ((DeclStmt*)$$)->setInitArray(initArray);
        initArray = nullptr;
        idx = 0;
    }
    ;


ArrayIndices
    : LBRACKET ConstExp RBRACKET {
        $$ = $2; 
    }
    | ArrayIndices LBRACKET ConstExp RBRACKET {
        $$ = $1;
        $$->setNext($3);
    }
    ;
ConstDeclStmt
    : CONST Type ConstDefList SEMICOLON {
        $$ = $3;
    }
    ;
ConstDefList
    : ConstDefList COMMA ConstDef {
        $$ = $1;
        $1->setNext($3);
    }
    | ConstDef {$$ = $1;}
    ;
ConstDef
    : ID ASSIGN ConstInitVal {
        SymbolEntry* se;
        se = identifiers->lookup($1, true);
        if (se != nullptr) { //重复定义了
            fprintf(stderr, "variable \"%s\" is repeated declared\n", (char*)$1);
            delete []$1;
            assert(se == nullptr);
        }
        if (recentVarType->isInt())
            se = new IdentifierSymbolEntry(TypeSystem::constIntType, $1, identifiers->getLevel());
        else if (recentVarType->isFloat())
            se = new IdentifierSymbolEntry(TypeSystem::constFloatType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        ((IdentifierSymbolEntry*)se)->setValue($3->getValue());
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1;
    }
    | ID ArrayIndices ASSIGN {
        SymbolEntry* se;
        se = identifiers->lookup($1, true);
        if (se != nullptr) { //重复定义了
            fprintf(stderr, "variable \"%s\" is repeated declared\n", (char*)$1);
            delete []$1;
            assert(se == nullptr);
        }
        ExprNode *expr = $2;
        std::vector<int> indexs;
        while (expr) {
            indexs.push_back(expr->getValue());
            expr = (ExprNode*)expr->getNext();
        }
        Type *arrType = nullptr;
        if (recentVarType->isInt())
            arrType = new ArrayType(indexs, TypeSystem::constIntType);
        else if (recentVarType->isFloat())
            arrType = new ArrayType(indexs, TypeSystem::constFloatType);
        assert(arrType != nullptr);
        se = new IdentifierSymbolEntry(arrType, $1, identifiers->getLevel());
        $<se>$ = se;
        identifiers->install($1, se);
        initArray = new ExprNode*[arrType->getSize() / TypeSystem::intType->getSize()];
        memset(initArray, 0, sizeof(ExprNode*) * (arrType->getSize() / TypeSystem::intType->getSize()));
        idx = 0;
        std::stack<std::vector<int>>().swap(dimesionStack); // 这一句的作用就是清空栈
        dimesionStack.push(indexs);
        delete []$1;
    } ConstInitVal {
        $$ = new DeclStmt(new Id($<se>4));
        ((DeclStmt*)$$)->setInitArray(initArray);
        initArray = nullptr;
        idx = 0;
    }
    ;
InitVal 
    : Exp {
        $$ = $1;
        if (initArray != nullptr) {
            initArray[idx++] = $1;
        }
    }
    | LBRACE RBRACE {
        std::vector<int> dimesion = dimesionStack.top();
        int size = 1;
        for (auto dim : dimesion) {
            size *= dim;
        }
        idx += size;
    }
    | LBRACE {
        std::vector<int> dimesion = dimesionStack.top();
        dimesionStack.push({-1, idx});
        dimesion.erase(dimesion.begin());
        if (dimesion.size() <= 0) {
            dimesion.push_back(1);
        }
        dimesionStack.push(dimesion);
    } InitValList RBRACE {
        while (dimesionStack.top()[0] != -1) {
            dimesionStack.pop();
        }
        idx = dimesionStack.top()[1];
        dimesionStack.pop();
        std::vector<int> dimesion = dimesionStack.top();
        int size = 1;
        for (auto dim : dimesion) {
            size *= dim;
        }
        idx += size;
    }
    ;
InitValList
    : InitVal
    | InitValList COMMA InitVal
    ;
ConstInitVal
    : ConstExp {
        $$ = $1;
        if (initArray != nullptr) {
            initArray[idx++] = $1;
        }
    }
    | LBRACE RBRACE {
        std::vector<int> dimesion = dimesionStack.top();
        int size = 1;
        for (auto dim : dimesion) {
            size *= dim;
        }
        idx += size;
    }
    | LBRACE {
        std::vector<int> dimesion = dimesionStack.top();
        dimesionStack.push({-1, idx});
        dimesion.erase(dimesion.begin());
        if (dimesion.size() <= 0) {
            dimesion.push_back(1);
        }
        dimesionStack.push(dimesion);
    } ConstInitValList RBRACE {
        while (dimesionStack.top()[0] != -1) {
            dimesionStack.pop();
        }
        idx = dimesionStack.top()[1];
        dimesionStack.pop();
        std::vector<int> dimesion = dimesionStack.top();
        int size = 1;
        for (auto dim : dimesion) {
            size *= dim;
        }
        idx += size;
    }
    ;
ConstInitValList
    : ConstInitVal
    | ConstInitValList COMMA ConstInitVal
    ;
FuncDef
    :
    Type ID {
        recentFuncRetType = $1;
        identifiers = new SymbolTable(identifiers);
        spillPos = -1;
        intArgNum = 0;
        floatArgNum = 0;
    }
    LPAREN FuncFParamsList RPAREN {
        std::vector<Type*> vec;//存储参数类型
        DeclStmt* temp = (DeclStmt*)$5;//获取参数列表，将其压入vec中
        while(temp){
            vec.push_back(temp->getId()->getSymPtr()->getType());
            temp = (DeclStmt*)(temp->getNext());
        }
        //生成函数类型，其中包括返回值类型以及参数类型
        Type* funcType = new FunctionType($1, vec);
        SymbolEntry* se = new IdentifierSymbolEntry(funcType, $2, identifiers->getPrev()->getLevel());
        identifiers->getPrev()->install($2, se);
    } 
    BlockStmt {
        SymbolEntry* se;
        se = identifiers->lookup($2);
        assert(se != nullptr);
        $$ = new FunctionDef(se, (DeclStmt*)$5, $8);
        SymbolTable* top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    ;
FuncFParamsList
    : FuncFParams {$$ = $1;}
    | %empty {$$ = nullptr;}
FuncFParams
    : FuncFParams COMMA FuncFParam {
        $$ = $1;
        $$->setNext($3);
    }
    | FuncFParam {$$ = $1;}
    ;
FuncFParam
    : Type ID {
        SymbolEntry* se;
        int argNum;//用于统计参数个数
        //需要对浮点数和整数分别处理，因为浮点数和整数可使用的固定参数个数不同
        //整数：因为前四个参数用寄存器存，之后的参数用栈传递
        if ($1->isFloat()) {
            argNum = floatArgNum;
            if (argNum > 15) {
                argNum = spillPos;
                spillPos--;
            }
            floatArgNum++;
        }
        else {
            argNum = intArgNum;
            if (argNum > 3) {
                argNum = spillPos;
                spillPos--;
            }
            intArgNum++;
        }
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel(), argNum);
        identifiers->install($2, se);
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    | Type ID LBRACKET RBRACKET {
        //指针类型和整数型同样
        SymbolEntry* se;
        int argNum = intArgNum;
        if (argNum > 3) {
            argNum = spillPos;
            spillPos--;
        }
        intArgNum++;
        se = new IdentifierSymbolEntry(new PointerType(new ArrayType({}, $1)), $2, identifiers->getLevel(), argNum);
        identifiers->install($2, se);
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    | Type ID LBRACKET RBRACKET ArrayIndices {
        std::vector<int> indexs;
        ExprNode *expr = $5;
        while (expr) {
            indexs.push_back(expr->getValue());
            expr = (ExprNode*)expr->getNext();
        }
        int argNum = intArgNum;
        if (argNum > 3) {
            argNum = spillPos;
            spillPos--;
        }
        intArgNum++;
        SymbolEntry* se;
        se = new IdentifierSymbolEntry(new PointerType(new ArrayType(indexs, $1)), $2, identifiers->getLevel(), argNum);
        identifiers->install($2, se);
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    ;
%%

int yyerror(char const* message)
{
    std::cerr << message << std::endl;
    return -1;
}
