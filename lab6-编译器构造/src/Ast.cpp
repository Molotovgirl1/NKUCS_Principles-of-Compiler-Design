#include "Ast.h"
#include <stack>
#include <string>
#include <iostream>
#include <assert.h>
#include "IRBuilder.h"
#include "Instruction.h"
#include "SymbolTable.h"
#include "Type.h"
#include "Unit.h"
#include <limits.h>

extern Unit unit;
extern MachineUnit mUnit;

extern FILE *yyout;
int Node::counter = 0;
IRBuilder *Node::builder = nullptr;

// 构造函数之类的代码区

Node::Node()
{
    seq = counter++;
    next = nullptr;
}

void Node::setNext(Node *node)
{
    Node *n = this;
    while (n->getNext())
    {
        n = n->getNext();
    }
    n->next = node;
}

BinaryExpr::BinaryExpr(SymbolEntry *se, int op, ExprNode *expr1, ExprNode *expr2) : ExprNode(se), op(op), expr1(expr1), expr2(expr2)
{
    this->dst = new Operand(se);
    this->type = se->getType();
    std::string op_type;
    if(op >= AND)
        isCond = true;
    if (op == BinaryExpr::AND || op == BinaryExpr::OR)
        isAnd_Or = true;
    Type *type1 = expr1->getType();
    Type *type2 = expr2->getType();
    switch (op)
    {
    case ADD:
        op_type = "+";
        break;
    case SUB:
        op_type = "-";
        break;
    case MUL:
        op_type = "*";
        break;
    case DIV:
        op_type = "/";
        break;
    case MOD:
        op_type = "%";
        break;
    case AND:
        op_type = "&&";
        break;
    case OR:
        op_type = "||";
        break;
    case LESS:
        op_type = "<";
        break;
    case LESSEQUAL:
        op_type = "<=";
        break;
    case GREATER:
        op_type = ">";
        break;
    case GREATEREQUAL:
        op_type = ">=";
        break;
    case EQUAL:
        op_type = "==";
        break;
    case NOTEQUAL:
        op_type = "!=";
        break;
    }
    // 前几个操作符是算术运算符，返回类型是int型，后面是逻辑运算符，返回类型是Bool型
    bool hasFloat = (type1->isFloat() || type2->isFloat());
    if (this->IsCond()) { //逻辑，转bool
        type = TypeSystem::boolType;
        // 对于AND和OR逻辑运算，如果操作数表达式不是bool型，需要进行隐式转换，转为bool型。
        if ( this->IsAnd_Or() ) {
            if (type1->isInt() && type1->getSize() == 32)
                this->expr1 = new ImplictCastExpr(expr1, ImplictCastExpr::ITB);
            else if (type1->isFloat() && type1->getSize() == 32)
                this->expr1 = new ImplictCastExpr(expr1, ImplictCastExpr::FTB);
            if (type2->isInt() && type2->getSize() == 32)
                this->expr2 = new ImplictCastExpr(expr2, ImplictCastExpr::ITB);
            else if (type2->isFloat() && type2->getSize() == 32)
                this->expr2 = new ImplictCastExpr(expr2, ImplictCastExpr::FTB);
        }
        else {
            if (type1->isBool() && type1->getSize() == 1)
                if (hasFloat)
                    this->expr1 = new ImplictCastExpr(expr1, ImplictCastExpr::BTF);
                else
                    this->expr1 = new ImplictCastExpr(expr1, ImplictCastExpr::BTI);
            else if (type1->isInt() && type1->getSize() == 32 && hasFloat)
                this->expr1 = new ImplictCastExpr(expr1, ImplictCastExpr::ITF);
            if (type2->isBool() && type2->getSize() == 1)
                if (hasFloat)
                    this->expr2 = new ImplictCastExpr(expr2, ImplictCastExpr::BTF);
                else
                    this->expr2 = new ImplictCastExpr(expr2, ImplictCastExpr::BTI);
            else if (type2->isInt() && hasFloat)
                this->expr2 = new ImplictCastExpr(expr2, ImplictCastExpr::ITF);
        }
    }
    else if (op != BinaryExpr::MOD) { 
        // 如果是MOD类型的话，两个操作数一定是int，不然它走不出语法分析
        // 这里不考虑bool类型做操作数的情况
        if (hasFloat)
            type = TypeSystem::floatType;
        else
            type = TypeSystem::intType;
        if (type1->isInt() && hasFloat)
            this->expr1 = new ImplictCastExpr(expr1, ImplictCastExpr::ITF);
        if (type2->isInt() && hasFloat)
            this->expr2 = new ImplictCastExpr(expr2, ImplictCastExpr::ITF);      
    }
    else
        if (hasFloat)
            type = TypeSystem::floatType;
        else
            type = TypeSystem::intType;
}

double BinaryExpr::getValue()
{
    // 这里有点坑，不能用double类型存储，不然196float过不了，有精度问题，只能用float
    float value = 0;
    float value1 = expr1->getValue();
    float value2 = expr2->getValue();
    switch (op) {
        case ADD:
            value = value1 + value2;
            break;
        case SUB:
            value = value1 - value2;
            break;
        case MUL:
            value = value1 * value2;
            break;
        case DIV:
            value = value1 / value2;
            break;
        case MOD:
            if (type->isFloat()) ;
                // do nothing, because float % float is not allowed
            else
                value = (int)value1 % (int)value2;
            break;
        case AND:
            value = value1 && value2;
            break;
        case OR:
            value = value1 || value2;
            break;
        case LESS:
            value = value1 < value2;
            break;
        case LESSEQUAL:
            value = value1 <= value2;
            break;
        case GREATER:
            value = value1 > value2;
            break;
        case GREATEREQUAL:
            value = value1 >= value2;
            break;
        case EQUAL:
            value = value1 == value2;
            break;
        case NOTEQUAL:
            value = value1 != value2;
            break;
    }
    return value;
}

UnaryExpr::UnaryExpr(SymbolEntry *se, int op, ExprNode *expr) 
    : ExprNode(se, UNARYEXPR), op(op), expr(expr)
{
    std::string op_str;
    switch (op)
    {
    case NOT:
        op_str = "!";
        break;
    case SUB:
        op_str = "-";
        break;
    }
    if (op_str == "!"){
        this->dst = new Operand(se);
        this->type = se->getType();
    }
    else if (op_str == "-"){
        this->dst = new Operand(se);
        this->type = se->getType();
        if (expr->getType()->isBool())
        {
            this->expr = new ImplictCastExpr(expr, ImplictCastExpr::BTI);
        }
    }
}

double UnaryExpr::getValue()
{
    float value = expr->getValue();
    if (op == UnaryExpr::NOT)
        value = !value;
    else if (op == UnaryExpr::SUB)
        value = -value;

    return value;
}

double Constant::getValue()
{
    double value = dynamic_cast<ConstantSymbolEntry* >(symbolEntry)->getValue();
    return value;
}

double Id::getValue()
{
    double value = dynamic_cast<IdentifierSymbolEntry* >(symbolEntry)->getValue();
    return value;
}

CallExpr::CallExpr(SymbolEntry *se, ExprNode *param) : ExprNode(se), param(param)
{
    // 统计实参数量
    int paramCnt = 0;
    dst = nullptr;
    // 获取参数个数
    for (ExprNode* temp = param; temp; temp = dynamic_cast<ExprNode*>(temp->getNext())) {
        params.push_back(temp);
        paramCnt++;
    }
    // 由于存在函数重载的情况，这里我们提前将重载的函数通过符号表项的next指针连接
    // 这里需要根据实参个数判断对应哪一个具体函数，找到其对应得符号表项
    Type *type;
    std::vector<Type *> FParams;
    for (SymbolEntry* tempSe = se; tempSe; tempSe = tempSe->getNext()) {
        type = tempSe->getType();
        FParams = dynamic_cast<FunctionType*>(type)->getParamsType();
        if (paramCnt == FParams.size()) {
            this->symbolEntry = tempSe;
            break;
        }
    }
    if (symbolEntry) {
        // 如果函数返回值类型不为空，需要存储返回结果
        this->type = dynamic_cast<FunctionType* >(type)->getRetType();
        if (this->type != TypeSystem::voidType) {
            SymbolEntry *se = new TemporarySymbolEntry(this->type, SymbolTable::getLabel());
            dst = new Operand(se);
        }
        // ExprNode *temp = param;
        // // 逐个比较形参列表和实参列表中每个参数的类型是否相同
        // if (params.size() < FParams.size())
        //     fprintf(stderr, "too few arguments to function %s %s\n", symbolEntry->toStr().c_str(), type->toStr().c_str());
        // else if (params.size() > FParams.size())
        //     fprintf(stderr, "too many arguments to function %s %s\n", symbolEntry->toStr().c_str(), type->toStr().c_str());
        auto it = FParams.begin();
        auto it2 = params.begin();
        for (; it != FParams.end(); it++, it2++)
        {
            if ((*it)->isFloat() && (*it2)->getType()->isInt())
                *it2 = new ImplictCastExpr(*it2, ImplictCastExpr::ITF);
            else if ((*it)->isInt() && (*it2)->getType()->isFloat())
                *it2 = new ImplictCastExpr(*it2, ImplictCastExpr::FTI);
            else if ((*it)->isInt() && (*it2)->getType()->isBool())
                *it2 = new ImplictCastExpr(*it2, ImplictCastExpr::BTI);
            else if ((*it)->isBool() && (*it2)->getType()->isInt())
                *it2 = new ImplictCastExpr(*it2, ImplictCastExpr::ITB);
            else if ((*it)->isFloat() && (*it2)->getType()->isBool())
                *it2 = new ImplictCastExpr(*it2, ImplictCastExpr::BTF);
            else if ((*it)->isBool() && (*it2)->getType()->isFloat())
                *it2 = new ImplictCastExpr(*it2, ImplictCastExpr::FTB);
            else if ((*it)->getKind() != (*it2)->getType()->getKind())
            {
                fprintf(stderr, "parameter's type %s can't convert to %s\n", (*it2)->getType()->toStr().c_str(), (*it)->toStr().c_str());
            }
        }
    }
    else{
        fprintf(stderr, "function is undefined\n");
    }
}


// -----------genCode代码区------------------

void Node::backPatch(std::vector<Instruction*>& list, BasicBlock* bb) {
    for (auto& inst : list) {
        BasicBlock* temp = inst->getParent();
        temp->addSucc(bb);
        bb->addPred(temp);
        if (inst->isCond())
            dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
        else if (inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}

std::vector<Instruction *> Node::merge(std::vector<Instruction *> &list1,
     std::vector<Instruction *> &list2)
{
    std::vector<Instruction *> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Ast::genCode(Unit *unit)
{
    IRBuilder *builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

void BinaryExpr::genCode()
{
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    // LAndExp
    if (op == AND)
    {
        if (this->isBr)
        {
            expr1->isBr=true;
            expr2->isBr=true;
        }
        // if the result of lhs is true, jump to the trueBB.
        BasicBlock *expr2BB = new BasicBlock(func); 
        expr1->genCode();
        backPatch(expr1->trueList(), expr2BB);
        builder->setInsertBB(expr2BB); // set the insert point to the trueBB so that intructions
                                    // generated by expr2 will be inserted into it.
        expr2->genCode();
        bb = builder->getInsertBB();//update the insertBB
        true_list = expr2->trueList();
        false_list = merge(expr1->falseList(), expr2->falseList());
    }
    // LOrExp
    else if (op == OR)
    {
        // Todo
        if (this->isBr)
        {
            expr1->isBr=true;
            expr2->isBr=true;
        }
        BasicBlock *expr2BB = new BasicBlock(func);
        expr1->genCode();
        backPatch(expr1->falseList(), expr2BB);
        builder->setInsertBB(expr2BB);
        expr2->genCode();
        bb = builder->getInsertBB();
        true_list = merge(expr1->trueList(), expr2->trueList());
        false_list = expr2->falseList();
    }
    // RelExp
    else if (op >= LESS && op <= NOTEQUAL)
    {
        // Todo
        expr1->setnotfir();
        expr2->setnotfir();
        expr1->genCode();
        expr2->genCode();
        int Cmp_opcode = INT_MIN;
        auto src1 = expr1->getOperand();
        auto src2 = expr2->getOperand();
        //生成比较指令   
        //浮点数类型，生成浮点类型指令
        if(src1->getType()->isFloat() || src2->getType()->isFloat()){
            switch (op)
            {
            case LESS:
                Cmp_opcode=CmpInstruction::FL;
                break;
            case LESSEQUAL:
                Cmp_opcode=CmpInstruction::FLE;
                break;
            case GREATER:
                Cmp_opcode=CmpInstruction::FG;
                break;
            case GREATEREQUAL:
                Cmp_opcode=CmpInstruction::FGE;
                break;
            case EQUAL:
                Cmp_opcode=CmpInstruction::FE;
                break;
            case NOTEQUAL:
                Cmp_opcode=CmpInstruction::FNE;
                break;
            default:
                break;
            }
        }
        else{
            switch(op){
            case LESS:
                Cmp_opcode=CmpInstruction::L;
                break;
            case LESSEQUAL:
                Cmp_opcode=CmpInstruction::LE;
                break;
            case GREATER:
                Cmp_opcode=CmpInstruction::G;
                break;
            case GREATEREQUAL:
                Cmp_opcode=CmpInstruction::GE;
                break;
            case EQUAL:
                Cmp_opcode=CmpInstruction::E;
                break;
            case NOTEQUAL:
                Cmp_opcode=CmpInstruction::NE;
                break;
            default:
                break;
            }
        }
        
        //int cmpOps[] = {CmpInstruction::L, CmpInstruction::LE, CmpInstruction::G, CmpInstruction::GE, CmpInstruction::E, CmpInstruction::NE};
        new CmpInstruction(Cmp_opcode, dst, src1, src2, bb);
        /* true和false未知，interB已知
        cmp
        br true, interB

        interB:
        b false
        */
        if (this->isBr)
        {
            // BasicBlock *interB;
            // interB = new BasicBlock(builder->getInsertBB()->getParent());
            // true_list.push_back(new CondBrInstruction(nullptr, interB, dst, builder->getInsertBB()));
            // false_list.push_back(new UncondBrInstruction(nullptr, interB));
            // 临时假块
            BasicBlock* truebb, * falsebb, * tempbb;
            truebb = new BasicBlock(func);
            falsebb = new BasicBlock(func);
            tempbb = new BasicBlock(func);
            true_list.push_back(std::move(new CondBrInstruction(truebb, tempbb, dst, bb)));
            false_list.push_back(std::move(new UncondBrInstruction(falsebb, tempbb)));
        }
    }
    else if (op >= ADD && op <= MOD)
    {
        expr1->genCode();
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        int instOP;
        //浮点数，生成浮点类型指令
        //不存在浮点类型的取模指令
        if(expr1->getType()->isFloat()||expr2->getType()->isFloat()){
            switch(op){
            case ADD:
                instOP=BinaryInstruction::FADD;
                break;
            case SUB:
                instOP=BinaryInstruction::FSUB;
                break;
            case MUL:
                instOP=BinaryInstruction::FMUL;
                break;
            case DIV:
                instOP=BinaryInstruction::FDIV;
                break;
            default:
                break;
            }
        }
        else{
            switch(op){
            case ADD:
                instOP=BinaryInstruction::ADD;
                break;
            case SUB:
                instOP=BinaryInstruction::SUB;
                break;
            case MUL:
                instOP=BinaryInstruction::MUL;
                break;
            case DIV:
                instOP=BinaryInstruction::DIV;
                break;
            case MOD:
                instOP=BinaryInstruction::MOD;
                break;
            default:
                break;
            }
        }
        //int opcodes[] = {BinaryInstruction::ADD, BinaryInstruction::SUB, BinaryInstruction::MUL, BinaryInstruction::DIV, BinaryInstruction::MOD};
        new BinaryInstruction(instOP, dst, src1, src2, bb);
    }
}

void UnaryExpr::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    expr->genCode();
    Operand *src = expr->getOperand();
    ConstantSymbolEntry *cse;
    int opcode = INT_MIN;
    switch (op)
    {
    case UnaryExpr::SUB:
        if (expr->getType()->isInt()) {
            opcode = UnaryInstruction::SUB;
            new UnaryInstruction(opcode, dst, src, bb);
        }
        else if (expr->getType()->isFloat()) {
            opcode = BinaryInstruction::FSUB;
            cse = new ConstantSymbolEntry(TypeSystem::floatType, 0);
            new BinaryInstruction(opcode, dst, new Operand(cse), src, bb);
        }
        break;
    case UnaryExpr::NOT:
        {   
            if (expr->getType()->isInt()) {
                opcode = CmpInstruction::E;
                cse = new ConstantSymbolEntry(TypeSystem::intType, 0);
                new CmpInstruction(opcode, dst, src, new Operand(cse), bb);
            }
            else if (expr->getType()->isFloat()) {
                opcode = CmpInstruction::FE;
                cse = new ConstantSymbolEntry(TypeSystem::floatType, 0);
                new CmpInstruction(opcode, dst, src, new Operand(cse), bb);
            }
            else
                new XorInstruction(dst, src, bb);
            if (isBr) {
                BasicBlock *interB;
                interB = new BasicBlock(builder->getInsertBB()->getParent());
                true_list.push_back(new CondBrInstruction(nullptr, interB, dst, builder->getInsertBB()));
                false_list.push_back(new UncondBrInstruction(nullptr, interB));
            }
        }
        break;
    case UnaryExpr::ADD:
        if (expr->getType()->isInt()) {
            opcode = UnaryInstruction::ADD;
            new UnaryInstruction(opcode, dst, src, bb);
            // new BinaryInstruction(opcode, dst, new Operand(cse), src, bb);
        }
        else if (expr->getType()->isFloat()) {
            opcode = UnaryInstruction::FADD;
            new UnaryInstruction(opcode, dst, src, bb);
            // new BinaryInstruction(opcode, dst, new Operand(cse), src, bb);
        }
        break;
    default:
        break;
    }
}

void CallExpr::genCode()
{
    std::vector<Operand *> rParams;
    BasicBlock* bb = builder->getInsertBB();
    for (auto param : params)
    {
        param->genCode();
        rParams.push_back(param->getOperand());
    }
    new CallInstruction(dst, symbolEntry, rParams, bb);
}

void Constant::genCode()
{
    // we don't need to generate code.
}

void Id::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry* >(symbolEntry);
    BasicBlock *bb = builder->getInsertBB();
    Operand *thisAddr = se->getAddr();
    Operand *addr = se->getAddr();
    Type* thisType = se->getType();
    std::vector<Operand *> offs;
    bool isType2 = false;
    Operand* base;
    if (thisType->isArray())
    {
        for (ExprNode *expr = index; expr; expr = dynamic_cast<ExprNode* >(expr->getNext()))
        {
            expr->genCode();
            offs.push_back(std::move(expr->getOperand()));
        }
        if (this->isPointer)
        {
            // 数组作为函数参数传递指针，取数组指针就行
            // 生成一条gep指令返回就行
            offs.push_back(new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)));
            new GepInstruction(dst, addr, offs, bb);
            return;
        }
        // if (((ArrayType *)thisType)->getBaseType()->isInt())
        //     addr = new Operand(new TemporarySymbolEntry(new PointerType(TypeSystem::intType), SymbolTable::getLabel()));
        // else if (((ArrayType *)thisType)->getBaseType()->isFloat())
        //     addr = new Operand(new TemporarySymbolEntry(new PointerType(TypeSystem::floatType), SymbolTable::getLabel()));
        // new GepInstruction(addr, thisAddr, offs, bb, isType2);
        goto typeJudge;
    }
    else if (thisType->isPtr())
    {
        isType2 = true;
        // ExprNode *tmp = index;
        if (index == nullptr)
        {
            // 如果数组标识符没有索引，他应该是作为参数传递的，取数组指针就行
            new LoadInstruction(dst, addr, bb);
            return;
        }
        base = new Operand(new TemporarySymbolEntry(((PointerType *)(addr->getType()))->getType(), SymbolTable::getLabel()));
        new LoadInstruction(base, addr, bb);
        for (ExprNode *expr = index; expr; expr = dynamic_cast<ExprNode* >(expr->getNext()))
        {
            expr->genCode();
            offs.push_back(std::move(expr->getOperand()));
        }
        thisType = dynamic_cast<PointerType* >(thisType)->getType();
        thisAddr = base;
        typeJudge:
        if (dynamic_cast<ArrayType* >(thisType)->getBaseType()->isInt())
            addr = new Operand(new TemporarySymbolEntry(new PointerType(TypeSystem::intType), SymbolTable::getLabel()));
        else if (dynamic_cast<ArrayType* >(thisType)->getBaseType()->isFloat())
            addr = new Operand(new TemporarySymbolEntry(new PointerType(TypeSystem::floatType), SymbolTable::getLabel()));
        new GepInstruction(addr, thisAddr, offs, bb, isType2);
    }
    new LoadInstruction(dst, addr, bb);
}

void ImplictCastExpr::genCode()
{
    BasicBlock* bb = builder->getInsertBB();
    Operand* src = expr->getOperand();
    expr->genCode();
    // bool转int
    if (op == BTI) 
        //零扩展指令，将一种类型的变量拓展为另一种类型的变量，高位补0
        new ZextInstruction(dst, src, true, bb);
    // int转bool
    else if (op == ITB) {
        Operand* tmp = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
        new CmpInstruction(CmpInstruction::NE, dst, src, tmp, bb);
    }
    else if (op == FTI)
        new F2IInstruction(dst, src, bb);
    else if (op == ITF)
        new I2FInstruction(dst, src, bb);
    else if (op == FTB) {
        Operand* tmp = new Operand(new ConstantSymbolEntry(TypeSystem::floatType, 0));
        new CmpInstruction(CmpInstruction::FNE, dst, src, tmp, bb);
    }
    else if (op == BTF) {
        //位拓展为int,再将int转为float
        Operand *internal_op = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
        new ZextInstruction(internal_op, src, true, bb);
        new I2FInstruction(dst, internal_op, bb);
    }
    if (this->isBr) {
        //如果是条件语句，需要分别开辟true和false块
        BasicBlock* truebb, * falsebb, * tempbb;
        Function* func = bb->getParent();
        //临时假块
        truebb = new BasicBlock(func);
        falsebb = new BasicBlock(func);
        tempbb = new BasicBlock(func);
        true_list.push_back(std::move(new CondBrInstruction(truebb, tempbb, dst, bb)));
        false_list.push_back(std::move(new UncondBrInstruction(falsebb, tempbb)));
    }
}

void CompoundStmt::genCode()
{
    // Todo
    if (stmt) {
        stmt->genCode();
    }
}

void SeqNode::genCode()
{
    // Todo
    stmt1->genCode();
    stmt2->genCode();
}

void DeclStmt::genCode()
{
    SymbolEntry* TempSe = id->getSymPtr();
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(TempSe);
    if (se->isGlobal()) {
        SymbolEntry* addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        Operand* addr = new Operand(addr_se);
        se->setAddr(addr);
        unit.insertGlobal(se);
        if (se->getType()->isArray() && exprArray) {
            int size = se->getType()->getSize() / 32;
            double *arrayValue = new double[size];
            for (int i = 0; i < size; i++) {
                if (exprArray[i])
                    arrayValue[i] = exprArray[i]->getValue();
                else
                    arrayValue[i] = 0;
            }
            se->setArrayValue(arrayValue);
        }
    }
    else    //对于局部变量或者参数 要先分配空间AllocaInstruction
    {
        BasicBlock* bb = builder->getInsertBB();
        Function* func = builder->getInsertBB()->getParent();
        BasicBlock* entry = func->getEntry();
        Type* thisType = se->getType();
        Type* ptrType = new PointerType(thisType);
        Operand *addr = new Operand(new TemporarySymbolEntry(ptrType, SymbolTable::getLabel()));
        Instruction *alloca = new AllocaInstruction(addr, se); // allocate space for local id in function stack.
        entry->insertFront(alloca);               // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);                        // set the addr operand in symbol entry so that we can use it in subsequent code generation.
        if (expr || exprArray)//如果有初始值 需要再插入store
        {
            if (thisType->isArray()) // 数组的情况
            {
                Type *eleType = dynamic_cast<ArrayType* >(thisType)->getBaseType();
                Type *baseType;
                if (eleType->isFloat())
                    baseType = TypeSystem::floatType;
                else 
                    baseType = TypeSystem::intType;
                std::vector<int> indexs = dynamic_cast<ArrayType* >(thisType)->getIndexs();
                int size = thisType->getSize() / 32;
                std::vector<Operand *> offs;
                for (int j = 0; j < indexs.size(); j++) {
                    Operand* zero = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
                    offs.push_back(std::move(zero));
                }
                indexs = dynamic_cast<ArrayType* >(thisType)->getIndexs();
                // 因为数组初始化可能会用到很多零，这里我们先准备一个，然后之后就不用频繁load了
                Operand *zeroReg = new Operand(new TemporarySymbolEntry(baseType, SymbolTable::getLabel()));
                Operand *zero = new Operand(new ConstantSymbolEntry(baseType, 0));
                if(eleType->isFloat())
                    new BinaryInstruction(BinaryInstruction::FADD, zeroReg, zero, zero, bb);
                else
                    new BinaryInstruction(BinaryInstruction::ADD, zeroReg, zero, zero, bb);
                PointerType* ptrBaseType = new PointerType(new ArrayType({}, baseType));
                Operand *ele_addr = new Operand(new TemporarySymbolEntry(ptrBaseType, SymbolTable::getLabel()));
                Operand *step = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 1));
                new GepInstruction(ele_addr, se->getAddr(), offs, bb);
                for (int i = 0; i < size; i++) {
                    if (exprArray[i]) {
                        exprArray[i]->genCode();
                        new StoreInstruction(ele_addr, exprArray[i]->getOperand(), bb);
                    }
                    else
                        new StoreInstruction(ele_addr, zeroReg, bb);
                    if (i != size - 1) {
                        PointerType* ptrEleType = new PointerType(new ArrayType({}, eleType));
                        Operand *next_addr = new Operand(new TemporarySymbolEntry(ptrEleType, SymbolTable::getLabel()));
                        new GepInstruction(next_addr, ele_addr, {step}, bb, true);
                        ele_addr = next_addr;
                    }
                }
            }
            else {
                expr->genCode();
                new StoreInstruction(addr, expr->getOperand(), bb);
            }
        }
        if (se->isParam())//如果是参数，则还需要store。
        {
            func->addParam(se->getArgAddr());
            new StoreInstruction(addr, se->getArgAddr(), bb);
        }
    }
    //参数时需要使用next去下一个参数
    if (this->getNext()) {
        this->getNext()->genCode();
    }
}

void DeclStmt::setInitArray(ExprNode **exprArray)
{
    // 能走到这一步，id就是个数组
    Type *idType = id->getSymPtr()->getType();
    ArrayType* arrType = dynamic_cast<ArrayType* >(idType);
    Type *idBaseType = arrType->getBaseType();
    this->exprArray = exprArray;
    int size = idType->getSize() / 32;
    for (int i = 0; i < size; i++) {
        if (this->exprArray[i]) {
            // 这里考虑这一种就行，因为sysy里面整形数组初始元素只能是整数，但是浮点数不是
            if (this->exprArray[i]->getType()->isInt() && idBaseType->isFloat())
                this->exprArray[i] = new ImplictCastExpr(this->exprArray[i], ImplictCastExpr::ITF);
        }
    }
}

void BlankStmt::genCode()
{

}

void IfStmt::genCode()
{
    Function* func = builder->getInsertBB()->getParent();

    BasicBlock* then_bb = new BasicBlock(func);
    BasicBlock* end_bb = new BasicBlock(func);

    cond->genCode();
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), end_bb);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);
}

void IfElseStmt::genCode()
{
    // Todo
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();

    BasicBlock* then_bb = new BasicBlock(func);
    BasicBlock* else_bb = new BasicBlock(func);
    BasicBlock* end_bb = new BasicBlock(func);

    // 生成cond
    cond->genCode();
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), else_bb);

    // 生成then
    builder->setInsertBB(then_bb);
    if (this->thenStmt)
        thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    // 生成else
    builder->setInsertBB(else_bb);
    if (this->elseStmt)
        this->elseStmt->genCode();
    else_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, else_bb);

    builder->setInsertBB(end_bb);
}

void WhileStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Function* func = bb->getParent();
    BasicBlock* cond_bb = new BasicBlock(func);
    BasicBlock* then_bb = new BasicBlock(func);
    BasicBlock* end_bb = new BasicBlock(func);
    this->cond_bb = cond_bb;
    this->end_bb = end_bb;

    new UncondBrInstruction(cond_bb, bb);//无条件跳转到condbb
    builder->setInsertBB(cond_bb);
    cond->genCode();
    backPatch(cond->trueList(), then_bb);//cond为真 去执行函数体
    backPatch(cond->falseList(), end_bb);//cond为假 直接结束
    //循环体指令插入到循环体块
    builder->setInsertBB(then_bb);
    if(this->stmt){
        stmt->genCode();
    }
    then_bb = builder->getInsertBB();//生成代码的时候可能更改插入块 要再次获取
    new UncondBrInstruction(cond_bb, then_bb);//当前块要跳转到cond执行

    builder->setInsertBB(end_bb);
}

void BreakStmt::genCode()
{
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    //取当前所在的whileStmt节点的end块，无条件跳转到那里
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_end_bb(), bb);
    BasicBlock* break_next_bb = new BasicBlock(func);
    builder->setInsertBB(break_next_bb);
}

void ContinueStmt::genCode()
{
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    //取当前所在的whileStmt节点的cond块，无条件跳转到那里
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_cond_bb(), bb);
    BasicBlock* continue_next_bb = new BasicBlock(func);
    builder->setInsertBB(continue_next_bb);
}

void ReturnStmt::genCode()
{
    // Todo
    BasicBlock* bb = builder->getInsertBB();
    Operand *src = nullptr;
    if (retValue) {
        retValue->genCode();
        src = retValue->getOperand();
    }
    else {
        src = nullptr;
    }
    new RetInstruction(src, bb);
}

void AssignStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry* >(lval->getSymPtr());
    if (this->expr)
        this->expr->genCode();
    // 后面的基本和Id::genCode()相同，但是不能直接用Id::genCode()，因为Id::genCode()会生成一条Load指令
    // 这里不需要那个load
    // 反正基本上是抄一遍
    Operand *addr = se->getAddr();
    Operand *thisAddr = se->getAddr();
    Operand *src = expr->getOperand();
    Type* thisType = se->getType();
    std::vector<Operand *> offs;
    bool isType2 = false;
    Operand* base;

    // 如果lval的原始类型是int或constInt或float或constFloat
    // if (lval->getOriginType() == TypeSystem::intType 
    //     || lval->getOriginType() == TypeSystem::constIntType 
    //     || lval->getOriginType() == TypeSystem::floatType 
    //     || lval->getOriginType() == TypeSystem::constFloatType) {
    //     addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymPtr())->getAddr();
    // }
    // else  {
    //     lval->genCode();
    //     addr = lval->getOperand();
    // }

    /***
     * We haven't implemented array yet, the lval can only be ID. So we just store the result of the `expr` to the addr of the id.
     * If you want to implement array, you have to caculate the address first and then store the result into it.
     */
    ExprNode *index = dynamic_cast<Id* >(lval)->getIndex();
    if (thisType->isArray()) {
        // 先算地址
        for (ExprNode *expr = index; expr; expr = dynamic_cast<ExprNode* >(expr->getNext()))
        {
            expr->genCode();
            offs.push_back(std::move(expr->getOperand()));
        }
        goto typeJudge;
    }
    else if (thisType->isPtr()) {
        isType2 = true;
        base = new Operand(new TemporarySymbolEntry(((PointerType *)(addr->getType()))->getType(), SymbolTable::getLabel()));
        new LoadInstruction(base, addr, bb);
        for (ExprNode *expr = index; expr; expr = dynamic_cast<ExprNode* >(expr->getNext()))
        {
            expr->genCode();
            offs.push_back(std::move(expr->getOperand()));
        }
        thisType = dynamic_cast<PointerType* >(thisType)->getType();
        thisAddr = base;
        typeJudge:
        if (dynamic_cast<ArrayType* >(thisType)->getBaseType()->isInt())
            addr = new Operand(new TemporarySymbolEntry(new PointerType(TypeSystem::intType), SymbolTable::getLabel()));
        else if (dynamic_cast<ArrayType* >(thisType)->getBaseType()->isFloat())
            addr = new Operand(new TemporarySymbolEntry(new PointerType(TypeSystem::floatType), SymbolTable::getLabel()));
        new GepInstruction(addr, thisAddr, offs, bb, isType2);
    }
    new StoreInstruction(addr, src, bb);
}

void ExprStmt::genCode()
{
    expr->genCode();
}

void FunctionDef::genCode()
{
    Function *func = new Function(builder->getUnit(), se);
    BasicBlock *entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.
    builder->setInsertBB(entry);

    if (decl)
        decl->genCode();
    // function中的stmt节点是用compoundstmt进行初始化的
    if (stmt)
        stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors for each basic block.
     * Todo
     */
    auto block = func->begin();
    while (block != func->end()) {
        //获取该块的最后一条指令
        for(Instruction* i = (*block)->begin(); i != (*block)->rbegin(); i = i->getNext()){
            //移除基本块中间的跳转指令
            if (i->isCond() || i->isUncond())
                (*block)->remove(i);
        }
        Instruction* last = (*block)->rbegin();
        FunctionType* retType = dynamic_cast<FunctionType*>(se->getType());
        // 如果是条件指令
        if (last->isCond()) {
            BasicBlock* truebranch = dynamic_cast<CondBrInstruction*>(last)->getTrueBranch();
            BasicBlock* falsebranch = dynamic_cast<CondBrInstruction*>(last)->getFalseBranch();
            //相应branch为空，就插入ret指令
            if (truebranch->empty()) 
                new RetInstruction(nullptr, truebranch);
            else if (falsebranch->empty()) 
                new RetInstruction(nullptr, falsebranch);
        }
        // 如果是非条件指令
        else if (last->isUncond())
        {
            // 获得跳转的dst
            BasicBlock* dst = dynamic_cast<UncondBrInstruction*>(last)->getBranch();
            if (dst->empty())
            {// 相应dst为空，就根据函数返回类型插入ret指令
                if (retType->getRetType() == TypeSystem::intType){
                    SymbolEntry* tmp = new ConstantSymbolEntry(TypeSystem::intType, 0);
                    new RetInstruction(new Operand(tmp), dst);
                }
                else if (retType->getRetType() == TypeSystem::voidType)
                    new RetInstruction(nullptr, dst);
            }
        }
        //最后一条语句不是返回以及跳转
        else if (!last->isRet()) {
            //将void的末尾插入ret void
            if (retType->getRetType() == TypeSystem::voidType)
                new RetInstruction(nullptr, *block);
        }
        block++;
    }
}

// typeCheck代码区

void Ast::typeCheck()
{
    if (root != nullptr)
        return root->typeCheck();
}

void BinaryExpr::typeCheck()
{
}

void UnaryExpr::typeCheck()
{
}

void CallExpr::typeCheck()
{
}

void Constant::typeCheck()
{
}

void Id::typeCheck()
{
}

void CompoundStmt::typeCheck()
{
}

void SeqNode::typeCheck()
{
}

void DeclStmt::typeCheck()
{
}

void IfStmt::typeCheck()
{
}

void IfElseStmt::typeCheck()
{
}

void WhileStmt::typeCheck()
{
}

void ReturnStmt::typeCheck()
{
}

void AssignStmt::typeCheck()
{
}

void ExprStmt::typeCheck()
{
}

void FunctionDef::typeCheck()
{
}

// output代码区

void Ast::output()
{
    fprintf(yyout, "program\n");
    if (root != nullptr)
        root->output(4);
}

void ExprNode::output(int level)
{
    std::string name, type;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cconst string\ttype:%s\t%s\n", level, ' ', type.c_str(), name.c_str());
}

void BinaryExpr::output(int level)
{
    std::string op_str;
    switch (op)
    {
    case ADD:
        op_str = "add";
        break;
    case SUB:
        op_str = "sub";
        break;
    case MUL:
        op_str = "mul";
        break;
    case DIV:
        op_str = "div";
        break;
    case MOD:
        op_str = "mod";
        break;
    case AND:
        op_str = "and";
        break;
    case OR:
        op_str = "or";
        break;
    case LESS:
        op_str = "less";
        break;
    case LESSEQUAL:
        op_str = "lessequal";
        break;
    case GREATER:
        op_str = "greater";
        break;
    case GREATEREQUAL:
        op_str = "greaterequal";
        break;
    case EQUAL:
        op_str = "equal";
        break;
    case NOTEQUAL:
        op_str = "notequal";
        break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\ttype: %s\n", level, ' ',
            op_str.c_str(), type->toStr().c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

void UnaryExpr::output(int level)
{
    std::string op_str;
    switch (op)
    {
    case NOT:
        op_str = "not";
        break;
    case SUB:
        op_str = "minus";
        break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\ttype: %s\n", level, ' ',
            op_str.c_str(), type->toStr().c_str());
    expr->output(level + 4);
}

void CallExpr::output(int level)
{
    if (symbolEntry)
    {
        std::string name = symbolEntry->toStr();
        std::string type = symbolEntry->getType()->toStr();
        int scope = dynamic_cast<IdentifierSymbolEntry *>(symbolEntry)->getScope();
        fprintf(yyout, "%*cCallExpr\tfunction name: %s\tscope: %d\ttype: %s\n", level, ' ', name.c_str(), scope, type.c_str());
        // 打印参数信息
        Node* temp = param;
        while (1) {
            if(temp==nullptr){
                break;
            }
            temp->output(level + 4);
            temp = temp->getNext();
        }
    }
}

void Constant::output(int level)
{
    std::string type = symbolEntry->getType()->toStr();
    std::string value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

void Id::output(int level)
{
    std::string name = symbolEntry->toStr();
    std::string type = symbolEntry->getType()->toStr();
    int scope = dynamic_cast<IdentifierSymbolEntry *>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ', name.c_str(), scope, type.c_str());
}

double ImplictCastExpr::getValue()
{
    double value = expr->getValue();
    switch (op)
    {
    case ITB:
    case FTB:
        value = (!!value);
        break;
    case FTI:
        value = (int)value;
        break;
    case BTI:
    case ITF:
    case BTF:
    default:
        break;
    }
    return value;
}

void ImplictCastExpr::output(int level)
{
    fprintf(yyout, "%*cImplictCastExpr\ttype: %s to %s\n", level, ' ', expr->getType()->toStr().c_str(), type->toStr().c_str());
    this->expr->output(level + 4);
}

void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    stmt->output(level + 4);
}

void SeqNode::output(int level)
{
    stmt1->output(level);
    stmt2->output(level);
}

void DeclStmt::output(int level)
{
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    id->output(level + 4);
    if (expr)
    {
        expr->output(level + 4);
    }
    if (this->getNext())
    {
        dynamic_cast<DeclStmt* >(this->getNext())->output(level);
    }
}

void BlankStmt::output(int level)
{
    fprintf(yyout, "%*cBlankStmt\n", level, ' ');
}

void IfStmt::output(int level)
{
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void IfElseStmt::output(int level)
{
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}

void WhileStmt::output(int level)
{
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    stmt->output(level + 4);
}
void BreakStmt::output(int level)
{
    fprintf(yyout, "%*cBreakStmt\n", level, ' ');
}

void ContinueStmt::output(int level)
{
    fprintf(yyout, "%*cContinueStmt\n", level, ' ');
}

void ReturnStmt::output(int level)
{
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    if (retValue != nullptr)
    {
        retValue->output(level + 4);
    }
}

void AssignStmt::output(int level)
{
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void ExprStmt::output(int level)
{
    fprintf(yyout, "%*cExprStmt\n", level, ' ');
    expr->output(level + 4);
}

void FunctionDef::output(int level)
{
    std::string name = se->toStr();
    std::string type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ',
            name.c_str(), type.c_str());
    if (decl)
    {
        decl->output(level + 4);
    }
    stmt->output(level + 4);
}
