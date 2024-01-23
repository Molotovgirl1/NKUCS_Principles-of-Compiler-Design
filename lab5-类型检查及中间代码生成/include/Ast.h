#ifndef __AST_H__
#define __AST_H__
// 抽象语法树
#include <fstream>
#include "Operand.h"
#include <vector>
#include <stack>

class SymbolEntry;
class Type;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class IRBuilder;

class Node
{
private:
    static int counter;
    int seq;
protected:
    std::vector<Instruction*> true_list;  //真值条件下的指令
    std::vector<Instruction*> false_list; //假值条件下的指令
    static IRBuilder *builder;
    // 回填指令列表
    void backPatch(std::vector<Instruction*> &list, BasicBlock*bb);
    // 指令合并
    std::vector<Instruction*> merge(std::vector<Instruction*> &list1, std::vector<Instruction*> &list2);
    //++++++++++类型转换函数
    Operand* typeCast(Type* targetType, Operand* operand);
public:
    Node();
    int getSeq() const {return seq;};
    static void setIRBuilder(IRBuilder*ib) {builder = ib;};
    virtual void output(int level) = 0;
    virtual void typeCheck(Node** parentToChild) = 0;
    virtual void genCode() = 0;
    std::vector<Instruction*>& trueList() {return true_list;}
    std::vector<Instruction*>& falseList() {return false_list;}
};

// 表达式结点
class ExprNode : public Node
{
protected:
    SymbolEntry *symbolEntry;
    Operand *dst;   // The result of the subtree is stored into dst.
public:
    ExprNode(SymbolEntry *se) : symbolEntry(se){dst = new Operand(se);}
    Operand* getOperand() {return dst;};
    SymbolEntry* getSymPtr() {return symbolEntry;};
    //++++++++++新增获取类型与设置类型
    Type* getType();
    void setType(Type* type);
};

// 二元运算
class BinaryExpr : public ExprNode
{
private:
    int op;
    ExprNode *expr1, *expr2;
public:
    enum {ADD, SUB, MUL, DIV, MOD, AND, OR, LESS, LESSEQ, GREAT, GREATEQ, EQ, NEQ};
    BinaryExpr(SymbolEntry *se, int op, ExprNode*expr1, ExprNode*expr2) : ExprNode(se), op(op), expr1(expr1), expr2(expr2){};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class OneOpExpr : public ExprNode
{
private:
    int op;
    ExprNode *expr;
public:
    enum {SUB, NOT};
    OneOpExpr(SymbolEntry *se, int op, ExprNode* expr): ExprNode(se), op(op), expr(expr){};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

// 常量表达式
class Constant : public ExprNode
{
public:
    Constant(SymbolEntry *se) : ExprNode(se){}
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

// 语句结点类
class StmtNode : public Node
{};

class ExprStmtNode : public StmtNode
{//注意：该类由ExprStmt与ArrayIndices共享，二者的行为完全一致
private:
    std::vector<ExprNode*> exprList;
public:
    ExprStmtNode(){};
    void addNext(ExprNode* next);
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
    void initDimInSymTable(IdentifierSymbolEntry* se);
};

class Id : public ExprNode
{
private:
    ExprStmtNode* indices;
public:
    Id(SymbolEntry *se) : ExprNode(se), indices(nullptr){};
    SymbolEntry* getSymbolEntry() {return symbolEntry;}
    bool isArray();     //必须配合indices!=nullptr使用（a[]的情况）
    void addIndices(ExprStmtNode* idx) {indices = idx;}
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class EmptyStmt : public StmtNode
{
public:
    EmptyStmt(){};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

//++++++++++新增函数调用参数语句
class FuncCallParamsNode : public StmtNode
{
private:
    std::vector<ExprNode*> paramsList;
public:
    FuncCallParamsNode(){};
    void addNext(ExprNode* next);
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
    // 参数列表
    std::vector<ExprNode*> getParamsList() {return this->paramsList;};
    // 操作数列表
    std::vector<Operand*> getOperandList();
};

//++++++++++函数调用表达式
class FuncCallNode : public ExprNode
{
private:
    Id* funcId;
    FuncCallParamsNode* params;
public:
    FuncCallNode(SymbolEntry *se, Id* id, FuncCallParamsNode* params) : ExprNode(se), funcId(id), params(params){};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class CompoundStmt : public StmtNode
{
private:
    StmtNode *stmt;
public:
    CompoundStmt(StmtNode *stmt) : stmt(stmt) {};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class SeqNode : public StmtNode
{
private:
    std::vector<StmtNode*> stmtList;
public:
    SeqNode(){};
    void addNext(StmtNode* next);
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class InitValNode : public StmtNode
{
private:
    bool isconst;
    ExprNode* leafNode; //可能为空，即使是叶节点（考虑{}）
    std::vector<InitValNode*> innerList;//为空则为叶节点，这是唯一判断标准
public:
    InitValNode(bool isconst) : 
        isconst(isconst), leafNode(nullptr){};
    void addNext(InitValNode* next);
    void setLeafNode(ExprNode* leaf);
    bool isLeaf();
    bool isConst() const { return isconst; }
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class DefNode : public StmtNode
{
private:
    bool isConst;
    bool isArray;
    Id* id;
    Node* initVal;//对于非数组，是ExprNode；对于数组，是InitValNode
public:
    DefNode(Id* id, Node* initVal, bool isConst, bool isArray) : 
        isConst(isConst), isArray(isArray), id(id), initVal(initVal){};
    Id* getId() {return id;}
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class DeclStmt : public StmtNode
{
private:
    bool isConst;
    std::vector<DefNode*> defList;
public:
    DeclStmt(bool isConst) : isConst(isConst){};
    void addNext(DefNode* next);
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class IfStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
public:
    IfStmt(ExprNode *cond, StmtNode *thenStmt) : cond(cond), thenStmt(thenStmt){};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class IfElseStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
    StmtNode *elseStmt;
public:
    IfElseStmt(ExprNode *cond, StmtNode *thenStmt, StmtNode *elseStmt) : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt) {};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class WhileStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *bodyStmt;
    BasicBlock* condBlock;
    BasicBlock* endBlock;
public:
    WhileStmt(ExprNode *cond, StmtNode *bodyStmt) : cond(cond), bodyStmt(bodyStmt){};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
    BasicBlock* getCondBlock() {return this->condBlock;}
    BasicBlock* getEndBlock() {return this->endBlock;}
};

class BreakStmt : public StmtNode
{
public:
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class ContinueStmt : public StmtNode
{
public:
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

//返回语句
class ReturnStmt : public StmtNode
{
private:
    ExprNode *retValue;
    Type* retType;
public:
    ReturnStmt(ExprNode*retValue) : retValue(retValue) {};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class AssignStmt : public StmtNode
{
private:
    ExprNode *lval;
    ExprNode *expr;
public:
    AssignStmt(ExprNode *lval, ExprNode *expr) : lval(lval), expr(expr) {};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

// ++++++++++函数定义参数列表
class FuncDefParamsNode : public StmtNode
{
private:
    std::vector<Id*> paramsList;
public:
    FuncDefParamsNode() {};
    void addNext(Id* next);
    std::vector<Type*> getParamsType();
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class FunctionDef : public StmtNode
{
private:
    SymbolEntry *se;
    FuncDefParamsNode *params;
    StmtNode *stmt;
    StmtNode* voidAddRet = nullptr;
public:
    FunctionDef(SymbolEntry *se, FuncDefParamsNode *params, StmtNode *stmt) : se(se), params(params), stmt(stmt){};
    void output(int level);
    void typeCheck(Node** parentToChild);
    void genCode();
};

class Ast
{
private:
    Node* root;
public:
    Ast() {root = nullptr;}
    void setRoot(Node*n) {root = n;}
    void output();
    void typeCheck();
    void genCode(Unit *unit);
};

static std::stack<WhileStmt*> whileStack;

#endif
