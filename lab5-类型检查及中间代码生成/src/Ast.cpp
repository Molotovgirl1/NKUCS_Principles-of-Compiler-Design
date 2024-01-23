#include <iostream>
#include <string>
#include <assert.h>
#include "Ast.h"
#include "SymbolTable.h"
#include "Type.h"
#include "Unit.h"
#include "Instruction.h"
#include "IRBuilder.h"
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
class Type;

extern FILE *yyout;
int Node::counter = 0;
IRBuilder* Node::builder = nullptr;
Type* returnType = nullptr; //返回类型
bool funcReturned = false;  //函数返回值
int inIteration = 0;//在while迭代中
int genBr = 0;

//++++++++My part新增类型转换
/*
  将布尔类型扩展为整型；
  将整型转换为浮点型；
  将浮点型转换为整型；
*/
Operand* Node::typeCast(Type* targetType, Operand* operand) {
    // 首先判断是否真的需要类型转化
    if(!TypeSystem::needCast(operand->getType(), targetType)) {
        return operand;
    }
    BasicBlock *bb = builder->getInsertBB();
    Function *func = bb->getParent();
    Operand* retOperand = new Operand(new TemporarySymbolEntry(targetType, SymbolTable::getLabel()));
    // 先实现bool扩展为int
    if(operand->getType()->isBool() && targetType->isInt()) {
        // 插入一条符号扩展指令
        new ZextInstruction(operand, retOperand, bb);
    }
    // 实现 int 到 float 的转换
    else if(operand->getType()->isInt() && targetType->isFloat()) {
        // 插入一条类型转化指令
        new IntFloatCastInstructionn(IntFloatCastInstructionn::I2F, operand, retOperand, bb);
    }
    // 实现 float 到 int 的转换
    else if(operand->getType()->isFloat() && targetType->isInt()) {
        // 插入一条类型转化指令
        new IntFloatCastInstructionn(IntFloatCastInstructionn::F2I, operand, retOperand, bb);
    }
    return retOperand;
}

Node::Node()
{
    seq = counter++;
}

void Ast::output()
{
    fprintf(yyout, "program\n");
    if(root != nullptr)
        root->output(4);
}

void Node::backPatch(std::vector<Instruction*> &list, BasicBlock*bb)
{
    for(auto &inst:list)
    {
        if(inst->isCond())
            dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
        else if(inst->isUncond()){
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
        }
    }
}

std::vector<Instruction*> Node::merge(std::vector<Instruction*> &list1, std::vector<Instruction*> &list2)
{
    std::vector<Instruction*> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

//     代码生成
void Ast::genCode(Unit *unit)
{
    IRBuilder *builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

/*
  *********** 函数定义
*/
void FunctionDef::genCode()
{
    Unit *unit = builder->getUnit();
    Function *func = new Function(unit, se);
    BasicBlock *entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.
    builder->setInsertBB(entry);
    // 参数不为空时，生成参数的代码
    if(params!=nullptr){
        params->genCode();
    }
    stmt->genCode();
    // 返回值的代码
    if(this->voidAddRet != nullptr) {
        voidAddRet->genCode();
    }

    /*
     * 构建控制流图
     * Construct control flow graph. You need do set successors and predecessors for each basic block.
     * Todo
     前驱和后继
    */
    // 遍历Function中所有的BasicBlock，在各个BasicBlock之间建立控制流关系
    for (auto block = func->begin(); block != func->end(); block++) {
        // 清除ret之后的全部指令
        Instruction* index = (*block)->begin();
        while(index != (*block)->end()) {
            if(index->isRet()) {
                while(index != (*block)->rbegin()) {
                    (*block)->remove(index->getNext());
                }
                break;
            }
            index = index->getNext();
        }
        // 获取该块的最后一条指令
        Instruction* last = (*block)->rbegin();
        // 对于有条件的跳转指令，需要对其true分支和false分支都设置控制流关系
        if (last->isCond()) {
            BasicBlock *trueBlock = dynamic_cast<CondBrInstruction*>(last)->getTrueBranch();
            BasicBlock *falseBlock = dynamic_cast<CondBrInstruction*>(last)->getFalseBranch();
            (*block)->addSucc(trueBlock);
            (*block)->addSucc(falseBlock);
            trueBlock->addPred(*block);
            falseBlock->addPred(*block);
        } 
        // 对于无条件的跳转指令，只需要对其目标基本块设置控制流关系
        if (last->isUncond()) {
            BasicBlock* dstBlock = dynamic_cast<UncondBrInstruction*>(last)->getBranch();
            (*block)->addSucc(dstBlock);
            dstBlock->addPred(*block);
        }
    }
}
//++++++ My part二元运算
void BinaryExpr::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Function *func = bb->getParent();
    Type* maxType = TypeSystem::getMaxType(expr1->getSymPtr()->getType(), expr2->getSymPtr()->getType());
    if (op == AND)
    {
        BasicBlock *trueBB = new BasicBlock(func);  // if the result of lhs is true, jump to the trueBB.
       // 创建一个基本块trueBB, 它是第二个子表达式生成的指令需要插入的位置，
        genBr = 1;
        expr1->genCode();
        //生成第一个子表达式的中间代码，在第一个子表达式生成中间代码的过程中，生成的跳转指
        //令的目标基本块尚不能确定，因此会将其插入到子表达式结点的true_list 和false_list 中。
        backPatch(expr1->trueList(), trueBB);
        //已经能确定true_list 中跳转指令的目的基本块为trueBB，因此进行回填
        builder->setInsertBB(trueBB);               // set the insert point to the trueBB so that intructions generated by expr2 will be inserted into it.
        expr2->genCode();
        //设置第二个子表达式的插入点为trueBB，然后生成其中间代码。最后，因为当前仍不
        // 能确定子表达式二的true_list 的目的基本块，因此我们将其插入到当前结点的true_list 中，
        true_list = expr2->trueList();
        false_list = merge(expr1->falseList(), expr2->falseList());
        //不能知道两个子表达式的false_list 的跳转基本块，便只能将其插入到当前结点的false_list
        //中，让父结点回填当前结点的true_list 和false_list。
    }
    else if(op == OR)
    {//仿照and
        BasicBlock *falseBB = new BasicBlock(func);
        genBr = 1;
        expr1->genCode();
        backPatch(expr1->falseList(), falseBB);
        builder->setInsertBB(falseBB);
        expr2->genCode();
        true_list = merge(expr1->trueList(), expr2->trueList());
        false_list = expr2->falseList();
    }
    else if(op >= LESS && op <= NEQ)//LESS, LESSEQ, GREAT, GREATEQ, EQ, NEQ
    {//仿照op >= ADD && op <= SUB
    /*
    *首先通过builder 得到后续生成的指令要插入的基本块bb
    *生成子表达式的中间代码，通过getOperand 函数得到子表达式的目的操作数
    *生成相应的二元运算指令并插入到基本块bb 中。
    */
        genBr--;
        expr1->genCode();
        expr2->genCode();
        genBr++;
        //得到子表达式的目的操作数
        Operand *src1 = typeCast(maxType, expr1->getOperand());
        Operand *src2 = typeCast(maxType, expr2->getOperand());
        int opcode;
        //生成相应的二元运算指令
        switch (op)
        {
        case LESS:
            opcode = CmpInstruction::L;
            break;
        case LESSEQ:
            opcode = CmpInstruction::LE;
            break;
        case GREAT:
            opcode = CmpInstruction::G;
            break;
        case GREATEQ:
            opcode = CmpInstruction::GE;
            break;
        case EQ:
            opcode = CmpInstruction::E;
            break;
        case NEQ:
            opcode = CmpInstruction::NE;
            break;
        }
        if(maxType->isFloat()) {
            new FCmpInstruction(opcode, dst, src1, src2, bb);//生成新的inst，之后push，
        }
        else {
            new CmpInstruction(opcode, dst, src1, src2, bb);
        }

        if(genBr > 0){
            // 跳转目标block
            BasicBlock* trueBlock, *falseBlock, *mergeBlock;
            trueBlock = new BasicBlock(func);
            falseBlock = new BasicBlock(func);
            mergeBlock = new BasicBlock(func);
            true_list.push_back(new CondBrInstruction(trueBlock, falseBlock, dst, bb));
            false_list.push_back(new UncondBrInstruction(mergeBlock, falseBlock));
        }
    }
    else if(op >= ADD && op <= MOD)//SUB, MUL, DIV, MOD, AND
    {   //通过builder 得到后续生成的指令要插入的基本块bb，在函数头
        expr1->genCode();
        //生成子表达式的中间代码
        expr2->genCode();
        //得到子表达式的目的操作数
        Operand *src1 = typeCast(maxType, expr1->getOperand());
        Operand *src2 = typeCast(maxType, expr2->getOperand());
        //通过getOperand 函数得到子表达式的目的操作数，设置指令的操作码，
        //最后生成相应的二元运算指令并插入到基本块bb 中 ->down
        int opcode;
        switch (op)
        {
        case ADD:
            opcode = BinaryInstruction::ADD;
            break;
        case SUB:
            opcode = BinaryInstruction::SUB;
            break;
        case MUL:
            opcode = BinaryInstruction::MUL;
            break;
        case DIV:
            opcode = BinaryInstruction::DIV;
            break;
        case MOD:
            opcode = BinaryInstruction::MOD;
            break;
        }
        if(maxType->isFloat()) {
            new FBinaryInstruction(opcode, dst, src1, src2, bb);
        }
        else {
            new BinaryInstruction(opcode, dst, src1, src2, bb);
        }
    }


}
//+++++My part单目运算
void OneOpExpr::genCode()
{
    //通过builder 得到后续生成的指令要插入的基本块bb
    BasicBlock *bb = builder->getInsertBB();
    Function *func = bb->getParent();
    /*
    * 取相反数的操作可能会涉及到int float和bool
    * 对于int和float指令，运算所需要的type即为本身的type
    * 对于bool，运算所需要的type转换为int类型
    */
    if (op == SUB)
    {
        expr->genCode();
        //生成操作数
        Operand *src1;
        Operand *src2;
        //根据表达式类型生成不同类型的减法指令
        if(expr->getSymPtr()->getType()->isBool()) {
            //将布尔值 false 视为整数 0
            src1 = new Operand(new ConstantSymbolEntry(TypeSystem::constIntType, 0));
            //将表达式 expr 的操作数进行类型转换，转换为整数类型
            src2 = typeCast(TypeSystem::intType, expr->getOperand());
            int opcode = BinaryInstruction::SUB;
            //生成减法指令
            new BinaryInstruction(opcode, dst, src1, src2, bb);
        }
        else if(expr->getSymPtr()->getType()->isInt()){
            //创建了一个表示整数 0 的常量操作数 src1
            src1 = new Operand(new ConstantSymbolEntry(TypeSystem::constIntType, 0));
            src2 = typeCast(TypeSystem::intType, expr->getOperand());
            int opcode = BinaryInstruction::SUB;
            new BinaryInstruction(opcode, dst, src1, src2, bb);
        }
        else if(expr->getSymPtr()->getType()->isFloat()) {
            src1 = new Operand(new ConstantSymbolEntry(TypeSystem::constFloatType, 0));
            src2 = typeCast(TypeSystem::floatType, expr->getOperand());
            int opcode = FBinaryInstruction::SUB;
            new FBinaryInstruction(opcode, dst, src1, src2, bb);
        }
    }
    else if(op == NOT)
    {
        genBr--;
        expr->genCode();
        genBr++;
        Operand *src1 = new Operand(new ConstantSymbolEntry(TypeSystem::constIntType, 0));
        Operand *src2 = typeCast(TypeSystem::intType, expr->getOperand());
        //比较 src1 和 src2 是否相等
        new CmpInstruction(CmpInstruction::E, dst, src1, src2, bb);
        //后续代码生成中处理逻辑非的条件分支
        if(genBr > 0) {
            // 跳转目标block
            BasicBlock* trueBlock, *falseBlock, *mergeBlock;
            trueBlock = new BasicBlock(func);
            falseBlock = new BasicBlock(func);
            mergeBlock = new BasicBlock(func);
            true_list.push_back(new CondBrInstruction(trueBlock, falseBlock, dst, bb));
            false_list.push_back(new UncondBrInstruction(mergeBlock, falseBlock));
        }
    }


}

void Constant::genCode()
{
    // we don't need to generate code.
}
//++++++My part变量
void Id::genCode()
{
    if(getType()->isConst()){
        return;
    }
    BasicBlock *bb = builder->getInsertBB();
    //获取标识符的地址
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    dst = new Operand(new TemporarySymbolEntry(dst->getType(), SymbolTable::getLabel()));
    //生成加载指令
    new LoadInstruction(dst, addr, bb);
}
/*
  ***********If语句
*/
void IfStmt::genCode()
{
    Function *func;
    BasicBlock *then_bb, *end_bb;
    //init
    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    
    genBr = 1;
    cond->genCode();
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), end_bb);
    //生成cond 结点的中间代码，
    //cond 为真时将跳转到基本块then_bb，
    //cond 为假时将跳转到基本块end_bb，我们进行回填
    //then分支
    builder->setInsertBB(then_bb);
    //因为生成thenStmt 结点中间代码的过程中可能改变指令的插入点，因此更新插入点
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);
}

/*
  ***********If-Else语句
*/
void IfElseStmt::genCode()//仿照ifstmt
{
    Function *func;
    BasicBlock *then_bb, *else_bb, *end_bb;
    //init
    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    //生成跳转指令
    genBr = 1;
    cond->genCode();
    // true ->then
    // false->else
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), else_bb);

    // 先处理then分支
    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    // 再处理else分支，相比于if新增的
    builder->setInsertBB(else_bb);
    elseStmt->genCode();
    else_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, else_bb);

    builder->setInsertBB(end_bb);
}

/*
  ***********  复合语句
*/
void CompoundStmt::genCode()
{   // Todo
    stmt->genCode();
}

/*
  *********** 顺序语句
*/
void SeqNode::genCode()//分支遍历
{   // Todo
    for(auto stmt : stmtList){
        stmt->genCode();
    }
}

/*
  ***********定义语句
*/
void DeclStmt::genCode()
{
    for(auto stmt : defList){
        stmt->genCode();
    }
}

/*
  ***********返回语句
*/
void ReturnStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    // 如果有返回值
    if(retValue != nullptr) {
        // 生成返回值的代码
        retValue->genCode();
        // 类型转换
        Operand* operand = typeCast(this->retType, retValue->getOperand());
        // 创建一个带有返回值的返回指令
        new RetInstruction(operand, bb);
    }
    else {
        // 创建一个不带返回值的返回指令
        new RetInstruction(nullptr, bb);
    }
}

/*
  *********** 赋值语句
*/
void AssignStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    //生成右侧表达式的代码
    expr->genCode();
    // 获取左侧标识符的地址
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymPtr())->getAddr();
    // 类型转换
    Operand *src = typeCast(dynamic_cast<PointerType*>(addr->getType())->getValueType(), expr->getOperand());
    new StoreInstruction(addr, src, bb);
}

/*
  ***********While语句
  使用到了头文件中全局定义的栈
*/
void WhileStmt::genCode()
{
    // 将当前的whileStmt压栈
    whileStack.push(this);
    Function* func = builder->getInsertBB()->getParent();
    //stmt_bb（循环体）、cond_bb（条件判断）和end_bb（循环结束）
    BasicBlock* stmt_bb, *cond_bb, *end_bb, *bb = builder->getInsertBB();
    stmt_bb = new BasicBlock(func);
    cond_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    //将条件判断基本块、循环结束基本块保存到当前 WhileStmt 对象的属性中
    this->condBlock = cond_bb;
    this->endBlock = end_bb;

    // 先从当前的bb跳转到cond_bb进行条件判断
    new UncondBrInstruction(cond_bb, bb);

    // 调整插入点到cond_bb，对条件判断部分生成中间代码
    builder->setInsertBB(cond_bb);
    //生成条件判断的跳转指令
    genBr = 1;
    cond->genCode();
    // true -> stmt
    // flase-> end
    backPatch(cond->trueList(), stmt_bb);
    backPatch(cond->falseList(), end_bb);

    // 调整插入点到stmt_bb，对循环体部分生成中间代码
    builder->setInsertBB(stmt_bb);
    bodyStmt->genCode();
    // 循环体完成之后，增加一句无条件跳转到cond_bb
    stmt_bb = builder->getInsertBB();
    new UncondBrInstruction(cond_bb, stmt_bb);

    // 重新调整插入点到end_bb
    builder->setInsertBB(end_bb);

    // 将当前的whileStmt出栈
    whileStack.pop();
}

/*
  *********** 函数定义参数列表
*/
void FuncDefParamsNode::genCode()
{
    Function *func = builder->getInsertBB()->getParent();
    BasicBlock *entry = func->getEntry();
    // 遍历参数列表中的每个参数
    for(auto id : paramsList){
        // 将参数添加到函数的参数列表
        func->insertParam(id->getOperand());
        // 获取参数对应的符号表条目
        IdentifierSymbolEntry* se = dynamic_cast<IdentifierSymbolEntry*>(id->getSymbolEntry());
        // 创建参数类型的指针、地址，分配空间并保存
        Type *type = new PointerType(id->getType());
        SymbolEntry *addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        Operand* addr = new Operand(addr_se);
        Instruction *alloca = new AllocaInstruction(addr, se);// allocate space for local id in function stack.
        entry->insertFront(alloca);                           // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);
        Operand *src = id->getOperand();
        // 创建一个存储指令，将参数的值存储到其地址所指向的内存位置中
        new StoreInstruction(addr, src, entry);
    }
}

/*
  *********** Continue语句
*/
void ContinueStmt::genCode()
{
    assert(whileStack.size()!=0);
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    // 首先获取当前所在的while
    WhileStmt* whileStmt = whileStack.top();
    // 获取条件判断block
    BasicBlock* cond_bb = whileStmt->getCondBlock();
    // 在当前基本块中生成一条跳转到条件判断的语句
    new UncondBrInstruction(cond_bb, bb);
    // 声明一个新的基本块用来插入后续的指令
    BasicBlock* nextBlock = new BasicBlock(func);
    builder->setInsertBB(nextBlock);
    
}

/*
  *********** Break语句
*/
void BreakStmt::genCode()
{
    assert(whileStack.size()!=0);
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    // 首先获取当前所在的while
    WhileStmt* whileStmt = whileStack.top();
    // 获取条件判断block
    BasicBlock* end_bb = whileStmt->getEndBlock();
    // 在当前基本块中生成一条跳转到条件判断的语句
    new UncondBrInstruction(end_bb, bb);
    // 声明一个新的基本块用来插入后续的指令
    BasicBlock* nextBlock = new BasicBlock(func);
    builder->setInsertBB(nextBlock);
}


void InitValNode::genCode()
{}
//+++++My part变量声明
void DefNode::genCode()
{
    Operand *addr;
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(id->getSymPtr());
    if(se->isGlobal())
    {
        //创建一个新的符号表条目用于存储全局变量的地址
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        //将声明插入到编译单元的符号表中
        this->builder->getUnit()->insertDecl(se);
    }
    else if(se->isLocal())
    {
        //获取当前函数和入口基本块
        Function *func = builder->getInsertBB()->getParent();
        BasicBlock *entry = func->getEntry();
        Instruction *alloca;
        SymbolEntry *addr_se;
        Type *type;
        //创建新的地址符号表条目和类型
        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        //生成 alloca 指令
        alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
        //将 alloca 指令插入到函数的入口基本块的开头。
        entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
        //设置局部变量的地址
        se->setAddr(addr);                                          // set the addr operand in symbol entry so that we can use it in subsequent code generation.
    }
    //add array instructions here
    //检查是否存在初始化值
    if(initVal!=nullptr){
        BasicBlock *bb = builder->getInsertBB();
        initVal->genCode();
        //进行类型转换
        Operand *src = typeCast(se->getType(), dynamic_cast<ExprNode *>(initVal)->getOperand());
        //生成存储指令,将初始化值存储到数组的首地址
        new StoreInstruction(addr, src, bb);
    }
}

/*
  *********** 函数调用参数
*/
void FuncCallParamsNode::genCode()
{
    for(auto expr : paramsList){
        expr->genCode();
    }
}

std::vector<Operand*> FuncCallParamsNode::getOperandList()
{
    std::vector<Operand*> result;
    for(auto param : paramsList){
        result.push_back(param->getOperand());
    }
    return result;
}

/*
  ***********函数调用语句
*/
void FuncCallNode::genCode()
{
    //找到对应function的符号表项
    IdentifierSymbolEntry* actualSE = dynamic_cast<IdentifierSymbolEntry*>(funcId->getSymPtr());
    if(actualSE->isLibFunc()){//若为库函数，则输出declare语句
        builder->getUnit()->insertDecl(actualSE);
    }
    //输出call语句
    //TODO: 内联函数
    BasicBlock *bb = builder->getInsertBB();
    //void 型函数不能返回
    if(params==nullptr){
        std::vector<Operand*> emptyList;
        new CallInstruction(dst, emptyList, dynamic_cast<IdentifierSymbolEntry*>(funcId->getSymPtr()), bb);
    }
    else{
        // 生成计算各个实参的中间代码
        params->genCode();
        // 完成实参形参之间的类型转换
        IdentifierSymbolEntry* funcSe = dynamic_cast<IdentifierSymbolEntry*>(funcId->getSymPtr());
        std::vector<Type*> paramsType = dynamic_cast<FunctionType*>(funcSe->getType())->getParamsType();
        std::vector<Operand*> passParams = params->getOperandList();
        std::vector<Operand*> realParams;
        for(int i = 0; i < passParams.size(); i++) {
            realParams.push_back(typeCast(paramsType[i], passParams[i]));
        }
        new CallInstruction(dst, realParams, dynamic_cast<IdentifierSymbolEntry*>(funcId->getSymPtr()), bb);
    }
}

/*
  *********** 表达式语句
*/
void ExprStmtNode::genCode()
{
    for(auto expr : exprList){
        expr->genCode();
    }
}

void EmptyStmt::genCode()
{
    
}


//    类型检查
void Ast::typeCheck()
{
    if(root != nullptr)
        root->typeCheck(nullptr);
}

/*
  *********** 检查函数定义 是否有返回值以及返回值类型是否匹配
*/
void FunctionDef::typeCheck(Node** parentToChild)
{
    // 获取函数返回值类型
    returnType = ((FunctionType*)se->getType())->getRetType();
    // 函数是否返回
    funcReturned = false;
    stmt->typeCheck(nullptr);
    // 非void类型的函数 需要有返回值
    if(!funcReturned && !returnType->isVoid()){
        fprintf(stderr, "根据函数定义，有 %s 类型返回值, 但无返回\n", returnType->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    // void类型没写return 需要补上
    if(!funcReturned && returnType->isVoid()) {
        this->voidAddRet = new ReturnStmt(nullptr);
    }
    returnType = nullptr;
}
//+++++++My part二元表达式的类型检查
void BinaryExpr::typeCheck(Node** parentToChild)
{
    expr1->typeCheck((Node**)&(this->expr1));
    expr2->typeCheck((Node**)&(this->expr2));
    //检查是否void函数返回值参与运算
    //判断表达式1是不是函数
    Type* realTypeLeft = expr1->getType()->isFunc() ? 
        ((FunctionType*)expr1->getType())->getRetType() : 
        expr1->getType();
    //不能用于计算的类型
    if(!realTypeLeft->calculatable()){
        fprintf(stderr, "类型 %s 不可计算!\n", expr1->getType()->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    //判断表达式2是不是函数
    Type* realTypeRight = expr2->getType()->isFunc() ? 
        ((FunctionType*)expr2->getType())->getRetType() : 
        expr2->getType();
    //不能用于计算的类型
    if(!realTypeRight->calculatable()){
        fprintf(stderr, "类型 %s 不可计算!\n", expr2->getType()->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    // 如果父节点不需要这个值，直接返回
    if(parentToChild==nullptr){
        return;
    }
    //左右子树均为常数，计算常量值，替换节点
    if(realTypeLeft->isConst() && realTypeRight->isConst()){
        SymbolEntry *se;
        // 如果父节点结果的目标类型为bool
        if(this->getType()->isBool()) {
            bool val = 0;
            //获取左子树的值
            float leftValue = expr1->getSymPtr()->isConstant() ? 
                ((ConstantSymbolEntry*)(expr1->getSymPtr()))->getValue() : 
                ((IdentifierSymbolEntry*)(expr1->getSymPtr()))->value;
            //获取右子树的值
            float rightValue = expr2->getSymPtr()->isConstant() ? 
                ((ConstantSymbolEntry*)(expr2->getSymPtr()))->getValue() : 
                ((IdentifierSymbolEntry*)(expr2->getSymPtr()))->value;
            switch(op)
            {
            case AND:
                val = leftValue && rightValue;
            break;
            case OR:
                val = leftValue || rightValue;
            break;
            case LESS:
                val = leftValue < rightValue;
            break;
            case LESSEQ:
                val = leftValue <= rightValue;
            break;
            case GREAT:
                val = leftValue > rightValue;
            break;
            case GREATEQ:
                val = leftValue >= rightValue;
            break;
            case EQ:
                val = leftValue == rightValue;
            break;
            case NEQ:
                val = leftValue != rightValue;
            break;
            }
            //生成了一个常量bool类型的符号条目
            se = new ConstantSymbolEntry(TypeSystem::constBoolType, val);
        }
        // 如果该节点结果的目标类型为int
        else if(this->getType()->isInt()){
            int val = 0;
            //获取左子树的值
            int leftValue = expr1->getSymPtr()->isConstant() ? 
                ((ConstantSymbolEntry*)(expr1->getSymPtr()))->getValue() : //字面值常量
                ((IdentifierSymbolEntry*)(expr1->getSymPtr()))->value;  //符号常量
            int rightValue = expr2->getSymPtr()->isConstant() ? 
                ((ConstantSymbolEntry*)(expr2->getSymPtr()))->getValue() : 
                ((IdentifierSymbolEntry*)(expr2->getSymPtr()))->value;
            switch (op) 
            {
            case ADD:
                val = leftValue + rightValue;
            break;
            case SUB:
                val = leftValue - rightValue;
            break;
            case MUL:
                val = leftValue * rightValue;
            break;
            case DIV:
                val = leftValue / rightValue;
            break;
            case MOD:
                val = leftValue % rightValue;
            break;
            }
            //生成了一个常量int类型的符号条目
            se = new ConstantSymbolEntry(TypeSystem::constIntType, val);
        }
        // 如果该节点结果的目标类型为float
        else{
            float val = 0;
            float leftValue = expr1->getSymPtr()->isConstant() ? 
                ((ConstantSymbolEntry*)(expr1->getSymPtr()))->getValue() : 
                ((IdentifierSymbolEntry*)(expr1->getSymPtr()))->value;
            float rightValue = expr2->getSymPtr()->isConstant() ? 
                ((ConstantSymbolEntry*)(expr2->getSymPtr()))->getValue() : 
                ((IdentifierSymbolEntry*)(expr2->getSymPtr()))->value;
            switch (op) 
            {
            case ADD:
                val = leftValue + rightValue;
            break;
            case SUB:
                val = leftValue - rightValue;
            break;
            case MUL:
                val = leftValue * rightValue;
            break;
            case DIV:
                val = leftValue / rightValue;
            break;
            }
            //生成了一个常量float类型的符号条目
            se = new ConstantSymbolEntry(TypeSystem::constFloatType, val);
        }
        //创建常量新节点并设置为左右子树的父节点
        Constant* newNode = new Constant(se);
        *parentToChild = newNode;
    }
    // 调整 && 和 || 运算符的两个操作数
    // 操作数类型不为 bool，或者se是一个常量bool
    // 增加一个和1的EQ判断

    //为了确保逻辑运算符的操作数都是布尔类型.将不是布尔类型的转换为布尔类型
    if(op == AND || op == OR) {
         // 则说明此时的情况为 a || 1 或者 a && a + b
        if(!expr1->getSymPtr()->getType()->isBool() || expr1->getSymPtr()->isConstant()) {
            Constant* zeroNode = new Constant(new ConstantSymbolEntry(TypeSystem::constIntType, 0));
            TemporarySymbolEntry* tmpSe = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
            //0!=expr1->true,else false
            BinaryExpr* newCond = new BinaryExpr(tmpSe, BinaryExpr::NEQ, zeroNode, expr1);
            //将新的二元表达式赋值给 expr1
            expr1 = newCond;
        }
        if(!expr2->getSymPtr()->getType()->isBool() || expr2->getSymPtr()->isConstant()) {
            Constant* zeroNode = new Constant(new ConstantSymbolEntry(TypeSystem::constIntType, 0));
            TemporarySymbolEntry* tmpSe = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
            BinaryExpr* newCond = new BinaryExpr(tmpSe, BinaryExpr::NEQ, zeroNode, expr2);
            expr2 = newCond;
        }
    }
}

void Constant::typeCheck(Node** parentToChild){}

//++++++My part变量的类型检查
void Id::typeCheck(Node** parentToChild)
{
    // 如果是一个普通变量就什么也不做
    // 如果是数组 那就无能为力了
    // 由于在语法解析阶段已经判断了标识符先定义再使用
    // 所以如果维度信息还未初始化则说明当前是数组定义阶段
    if(isArray() && indices!=nullptr){
        // 检查indices，若不为自然数则报错
        indices->typeCheck(nullptr);
        if(((IdentifierSymbolEntry*)getSymPtr())->arrayDimension.empty()){
            indices->initDimInSymTable((IdentifierSymbolEntry*)getSymPtr());
        }
        // 读取常量数组 这个不打算做了
        else if(getType()->isConst()){
            //TODO: 将常量数组+全常量下标的数组元素访问替换为字面值常量节点Constant
            //STEP：1.遍历indices下的exprList(私有域)，查看是否有非常量节点。若有，直接返回
            //STEP: 2.若全部为常量下标，替换
        }
    }
}

/*
  *********** If语句的检查
*/
void IfStmt::typeCheck(Node** parentToChild)
{
    cond->typeCheck((Node**)&(this->cond));
    /*
      如果条件表达式的类型不是布尔类型（isBool() 返回 false）
      或者条件表达式是一个常量（isConstant() 返回 true）。
      则创建一个新的条件表达式，
      使其与常量 0 进行不等于比较（BinaryExpr::NEQ）。
      这样可以确保条件表达式的结果始终为布尔类型。
    */
    if(!cond->getSymPtr()->getType()->isBool() || cond->getSymPtr()->isConstant()) {
        Constant* zeroNode = new Constant(new ConstantSymbolEntry(TypeSystem::constIntType, 0));
        TemporarySymbolEntry* tmpSe = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        BinaryExpr* newCond = new BinaryExpr(tmpSe, BinaryExpr::NEQ, zeroNode, cond);
        cond = newCond;
    }
    //then语句非空
    if(thenStmt!=nullptr) {
        thenStmt->typeCheck((Node**)&(this->thenStmt));
    }
    //then语句为空
    else {
        thenStmt = new EmptyStmt();
    }
}

/*
  *********** If-Else语句的检查
*/
void IfElseStmt::typeCheck(Node** parentToChild)
{
    cond->typeCheck((Node**)&(this->cond));
    /*
      如果条件表达式的类型不是布尔类型（isBool() 返回 false）
      或者条件表达式是一个常量（isConstant() 返回 true）。
      则创建一个新的条件表达式，
      使其与常量 0 进行不等于比较（BinaryExpr::NEQ）。
      这样可以确保条件表达式的结果始终为布尔类型。
    */
    if(!cond->getSymPtr()->getType()->isBool() || cond->getSymPtr()->isConstant()) {
        Constant* zeroNode = new Constant(new ConstantSymbolEntry(TypeSystem::constIntType, 0));
        TemporarySymbolEntry* tmpSe = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        BinaryExpr* newCond = new BinaryExpr(tmpSe, BinaryExpr::NEQ, zeroNode, cond);
        cond = newCond;
    }
    if(thenStmt!=nullptr) {
        thenStmt->typeCheck((Node**)&(this->thenStmt));
    }
    else {
        thenStmt = new EmptyStmt();
    }
    //else语句非空
    if(elseStmt!=nullptr){
        elseStmt->typeCheck((Node**)&(this->elseStmt));
    }
    //else语句为空
    else {
        elseStmt = new EmptyStmt();
    }
}

/*
  ***********复合语句的检查
*/
void CompoundStmt::typeCheck(Node** parentToChild)
{
    //对非空的语句进行检查
    if(stmt!=nullptr){
        stmt->typeCheck(nullptr);
    }
    else {
        stmt = new EmptyStmt();
    }
}

void SeqNode::typeCheck(Node** parentToChild)
{
    for(int i = 0;i<(int)stmtList.size();++i){
        stmtList[i]->typeCheck((Node**)&(stmtList[i]));
    }
}

void DeclStmt::typeCheck(Node** parentToChild)
{
    for(int i = 0;i<(int)defList.size();++i){
        defList[i]->typeCheck(nullptr);
    }
}

/*
  *********** 返回语句的检查
*/
void ReturnStmt::typeCheck(Node** parentToChild)
{
    // 返回类型为空
    if(returnType == nullptr){//not in a function
        fprintf(stderr, "不在函数中 不能ret\n");
        exit(EXIT_FAILURE);
    }
    // void函数返回了具体值
    else if(returnType->isVoid() && retValue!=nullptr){//returned a value in void()
        fprintf(stderr, "void 函数 返回value\n");
        exit(EXIT_FAILURE);
    }
    //非void函数返回了空值
    else if(!returnType->isVoid() && retValue==nullptr){//expected returned value, but returned nothing
        fprintf(stderr, "函数为 %s 类型，但无返回\n", returnType->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    //非void函数、非空返回值
    if(!returnType->isVoid()){
        //对返回值进行类型检查
        retValue->typeCheck((Node**)&(retValue));
    }
    this->retType = returnType;
    funcReturned = true;
}

/*
  *********** 赋值语句的检查
*/
void AssignStmt::typeCheck(Node** parentToChild)
{
    //左值类型检查
    lval->typeCheck(nullptr);
    //表达式类型检查
    expr->typeCheck((Node**)&(this->expr));
    //左值为常量
    if(lval->getType()->isConst()) {
        fprintf(stderr, "Unable to assign value to const variable %s\n", lval->getSymPtr()->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    //表达式返回空
    if(expr->getType()->isFunc() && ((FunctionType*)(expr->getType()))->getRetType()->isVoid()){//返回值为void的函数做运算数
        fprintf(stderr, "需要value值, 但函数类型 %s returns nothing\n", expr->getType()->toStr().c_str());
        exit(EXIT_FAILURE);
    }
}

void FuncDefParamsNode::typeCheck(Node** parentToChild){}

/*
  ***********Continue的检查
*/
void ContinueStmt::typeCheck(Node** parentToChild)
{
    // 如果不在while语句里
    if(!inIteration){
        fprintf(stderr, "continue statement outside iterations\n");
        exit(EXIT_FAILURE);
    }
}

/*
  ***********Break的检查
*/
void BreakStmt::typeCheck(Node** parentToChild)
{
    // 如果不在while语句里
    if(!inIteration){
        fprintf(stderr, "break statement outside iterations\n");
        exit(EXIT_FAILURE);
    }
}

/*
  *********** while的检查
*/
void WhileStmt::typeCheck(Node** parentToChild)
{
    cond->typeCheck((Node**)&(this->cond));
    /*
      条件表达式的类型不是布尔类型或者是常量
      则创建一个新的条件表达式
      使其与常量 0 进行不等于比较（BinaryExpr::NEQ）
      这样可以确保条件表达式的结果始终为布尔类型。
    */
    if(!cond->getSymPtr()->getType()->isBool() || cond->getSymPtr()->isConstant()) {
        Constant* zeroNode = new Constant(new ConstantSymbolEntry(TypeSystem::constIntType, 0));
        TemporarySymbolEntry* tmpSe = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        BinaryExpr* newCond = new BinaryExpr(tmpSe, BinaryExpr::NEQ, zeroNode, cond);
        cond = newCond;
    }
    // 对while非空语句进行检查
    if(bodyStmt!=nullptr) {
        //迭代器加一  此时可以出现continue break
        inIteration++;
        bodyStmt->typeCheck((Node**)&(this->bodyStmt));
        //迭代器减一  此时不可以出现continue break
        inIteration--;
    }
    else {
        bodyStmt = new EmptyStmt();
    }
}

void InitValNode::typeCheck(Node** parentToChild)
{
    
}

//+++++++My part变量声明中的类型检查
void DefNode::typeCheck(Node** parentToChild)
{
    id->typeCheck(nullptr);
    // 不赋初值，直接返回
    if(initVal==nullptr){
        return;
    }
    //如果赋初值，则对初值进行类型检查
    initVal->typeCheck((Node**)&(initVal));
    //判断是不是数组
    if(!id->getType()->isArray()){
        //右边可能出现函数：int a = f(),并且函数的返回值不能用于计算,void
        if(((ExprNode*)initVal)->getType()->isFunc() && 
            (!((FunctionType*)(((ExprNode*)initVal)->getType()))->getRetType()->calculatable())){
            fprintf(stderr, "需要value值, 但函数类型 %s return nothing\n", ((ExprNode*)initVal)->getType()->toStr().c_str());
            exit(EXIT_FAILURE);
        }
    }
    //如果变量是常量类型
    if(id->getType()->isConst()){
        //如果初始化的值不是常量，则报错
        if(!isArray) {
            if(!((ExprNode*)initVal)->getType()->isConst()) {
                fprintf(stderr, "用变量给常量赋值\n");
                exit(EXIT_FAILURE);
            }
        }
        else{
            if(!((InitValNode*)initVal)->isConst()) {
                fprintf(stderr, "用变量给常量赋值\n");
                exit(EXIT_FAILURE);
            }
        }
        // 接下来就是常量计算的工作了
        // 数组初始化值 暂时不打算做了
        if(id->getType()->isArray()){
            //TODO: initialize elements in symbol table
        }
        // 常量初始化值
        else{
            //创建一个变量的符号表条目
            IdentifierSymbolEntry* se = (IdentifierSymbolEntry*)id->getSymPtr();
            //赋初始值
            se->value = ((ConstantSymbolEntry*)((ExprNode*)initVal)->getSymPtr())->getValue();
        }   
    }
    // 如果是全局变量，也要根据需要赋值
    if(dynamic_cast<IdentifierSymbolEntry*>(id->getSymPtr())->isGlobal()) {
        // 对于初始化值不为空的，要进行初始化赋值
        if(initVal != nullptr) {
            // 只允许使用常量对全局变量进行赋值
            if(!((ExprNode*)initVal)->getType()->isConst()) {
                //如果赋的初值不是常量
                fprintf(stderr, "not allow to initialize global variable with not const value\n");
                exit(EXIT_FAILURE);
            }
            //创建一个变量的符号表条目并赋值
            IdentifierSymbolEntry* se = (IdentifierSymbolEntry*)id->getSymPtr();
            se->value = ((ConstantSymbolEntry*)((ExprNode*)initVal)->getSymPtr())->getValue();
        }
    }

}

/*
  ***********函数调用参数的检查
*/
void FuncCallParamsNode::typeCheck(Node** parentToChild)
{
    // 对每一个参数进行检查
    for(ExprNode* param : paramsList) {
        param->typeCheck((Node**)&param);
    }
}

/*
  ***********函数调用的检查
*/
void FuncCallNode::typeCheck(Node** parentToChild)//函数参数
{
    std::vector<Type*> funcParamsType = (dynamic_cast<FunctionType*>(this->funcId->getSymPtr()->getType()))->getParamsType();
    // 首先对于无参的进行检查
    if(this->params==nullptr && funcParamsType.size() != 0){
        fprintf(stderr, "function %s 参数数不匹配\n",this->funcId->getSymPtr()->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    else if(this->params==nullptr) {
        return;
    }
    // 先对FuncCallParamsNode进行类型检查，主要是完成常量计算
    this->params->typeCheck(nullptr); 
    std::vector<ExprNode*> funcCallParams = this->params->getParamsList();
    // 如果数量不一致直接报错
    if(funcCallParams.size() != funcParamsType.size()) {
        fprintf(stderr, "function %s  参数数不匹配\n",this->funcId->getSymPtr()->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    // 然后进行类型匹配
    // 依次匹配类型
    for(int i = 0; i < funcParamsType.size(); i++){
        Type* needType = funcParamsType[i];
        Type* giveType = funcCallParams[i]->getSymPtr()->getType();
        // 暂时不考虑类型转化的问题 所有的类型转化均到IR生成再做
        // 除了void类型都可以进行转化
        if(!needType->calculatable() && giveType->calculatable()
         ||needType->calculatable() && !giveType->calculatable()){
            fprintf(stderr, "function %s  参数类型不匹配\n",this->funcId->getSymPtr()->toStr().c_str());
            exit(EXIT_FAILURE);
        }
        // 检查数组是否匹配
        if(!needType->isArray() && giveType->isArray()
         ||needType->isArray() && !giveType->isArray()){
            fprintf(stderr, "function %s 参数类型不匹配\n",this->funcId->getSymPtr()->toStr().c_str());
            exit(EXIT_FAILURE);
        }
        //检查数组维度是否匹配
        if(needType->isArray() && giveType->isArray()){

        }
    }
}
//++++++My part表达式类型检查
void ExprStmtNode::typeCheck(Node** parentToChild)
{
    //遍历表达式列表,对每个表达式节点进行类型检查
    for(int i = 0;i<(int)exprList.size();++i){
        exprList[i]->typeCheck((Node**)&(exprList[i]));
    }
}

void EmptyStmt::typeCheck(Node** parentToChild){}

//+++++My part单目运算类型检查
void OneOpExpr::typeCheck(Node** parentToChild)
{
    expr->typeCheck((Node**)&(this->expr));
    //检查是否void函数返回值参与运算
    Type* realType = expr->getType()->isFunc() ? 
        ((FunctionType*)expr->getType())->getRetType() : 
        expr->getType();
    //void
    if(!realType->calculatable()){
        fprintf(stderr, "type %s 不可用于计算!\n", expr->getType()->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    //推断父节点类型,float->float,其他->int
    if(realType->isAnyFloat()) {
        this->setType(TypeSystem::floatType);
    }
    else{
        this->setType(TypeSystem::intType);
    }
    // 如果是not运算->bool
    if(op == NOT) {
        this->setType(TypeSystem::boolType);
    }
    //如果父节点不需要这个值，直接返回
    if(parentToChild==nullptr){
        return;
    }
    //孩子节点为常数，计算常量值，替换节点
    if(realType->isConst()){
        SymbolEntry *se;
        double val = 0;
        //获取孩子节点的值
        int initValue = expr->getSymPtr()->isConstant() ? 
            ((ConstantSymbolEntry*)(expr->getSymPtr()))->getValue() : 
            ((IdentifierSymbolEntry*)(expr->getSymPtr()))->value;
        switch (op) 
        {
        case SUB:
            val = -initValue;
        break;
        case NOT:
            val = !initValue;
        break;
        }
        //给父节点创建对应类型的常量符号条目
        if(this->getType()->isInt()){
            se = new ConstantSymbolEntry(TypeSystem::constIntType, val);
        }
        else{//float or bool
            se = new ConstantSymbolEntry(TypeSystem::constFloatType, val);
        }
        //创建新节点并设父节点
        Constant* newNode = new Constant(se);
        *parentToChild = newNode;
    }
}
//-----------------END
Type* ExprNode::getType()
{
    return symbolEntry->getType();
}

void ExprNode::setType(Type* type)
{
    symbolEntry->setType(type);
}

void BinaryExpr::output(int level)
{
    std::string op_str;
    switch(op)
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
        case LESSEQ:
            op_str = "lesseq";
            break;
        case GREAT:
            op_str = "great";
            break;
        case GREATEQ:
            op_str = "greateq";
            break;
        case EQ:
            op_str = "eq";
            break;
        case NEQ:
            op_str = "neq";
            break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\ttype: %s\n", level, ' ', op_str.c_str(), symbolEntry->getType()->toStr().c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

void OneOpExpr::output(int level) {
    std::string op_str;
    switch (op) {
        case NOT:
            op_str = "not";
            break;
        case SUB:
            op_str = "minus";
            break;
    }
    fprintf(yyout, "%*cOneOpExpr\top: %s\ttype: %s\n", level, ' ', op_str.c_str(), symbolEntry->getType()->toStr().c_str());
    expr->output(level + 4);
}

void Constant::output(int level)
{
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

bool Id::isArray()
{
    return getType()->isArray();
}

void Id::output(int level)
{
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());
    if(isArray() && indices!=nullptr){
        fprintf(yyout, "%*cArrayIndices\n", level+4, ' ');
        indices->output(level+8);
    }
}

void EmptyStmt::output(int level)
{
    fprintf(yyout, "%*cEmptyStmt\n", level, ' ');
}

void ExprStmtNode::addNext(ExprNode* next)
{
    exprList.push_back(next);
}

void ExprStmtNode::output(int level)
{
    fprintf(yyout, "%*cExprStmtNode\n", level, ' ');
    for(auto expr : exprList)
    {
        expr->output(level+4);
    }
}

void ExprStmtNode::initDimInSymTable(IdentifierSymbolEntry* se)
{
    for(auto expr :exprList){
        // 既不是字面值常量，也不是常量表达式
        if(!(expr->getSymPtr()->isConstant() || expr->getType()->isConst())){
            fprintf(stderr, "array dimensions must be constant! %d %d\n", expr->getSymPtr()->isConstant(), expr->getType()->isConst());
            fprintf(stderr, "%d %d\n", (int)((ConstantSymbolEntry*)(expr->getSymPtr()))->getValue(), (int)((IdentifierSymbolEntry*)(expr->getSymPtr()))->value);
            exit(EXIT_FAILURE);
        }
        // 字面值常量，值存在ConstantSymbolEntry中
        if(expr->getSymPtr()->isConstant()){
            se->arrayDimension.push_back((int)((ConstantSymbolEntry*)(expr->getSymPtr()))->getValue());
        }
        // 常量表达式，值存在IdentifierSymbolEntry中
        else if(expr->getType()->isConst()){
            se->arrayDimension.push_back((int)((IdentifierSymbolEntry*)(expr->getSymPtr()))->value);
        }
    }
}

void FuncCallNode::output(int level)
{
    std::string name, type;
    int scope;
    SymbolEntry* funcEntry = funcId->getSymbolEntry();
    name = funcEntry->toStr();
    type = funcEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(funcEntry)->getScope();
    fprintf(yyout, "%*cFuncCallNode\tfuncName: %s\t funcType: %s\tscope: %d\n", 
            level, ' ', name.c_str(), type.c_str(), scope);
    if(params!=nullptr){
        params->output(level+4);
    }
    else{
        fprintf(yyout, "%*cFuncCallParamsNode NULL\n", level+4, ' ');
    }
}

void FuncCallParamsNode::addNext(ExprNode* next)
{
    paramsList.push_back(next);
}

void FuncCallParamsNode::output(int level)
{
    fprintf(yyout, "%*cFuncCallParamsNode\n", level, ' ');
    for(auto param : paramsList){
        param->output(level+4);
    }
}

void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    if(stmt == nullptr){
        fprintf(yyout, "%*cNull Stmt\n", level+4, ' ');
    }
    else{
        stmt->output(level + 4);
    }
}

void SeqNode::addNext(StmtNode* next)
{
    stmtList.push_back(next);
}

void SeqNode::output(int level)
{
    fprintf(yyout, "%*cSequence\n", level, ' ');
    for(auto stmt : stmtList)
    {
        stmt->output(level + 4);
    }
}

void DeclStmt::addNext(DefNode* next)
{
    defList.push_back(next);
}

void DeclStmt::output(int level)
{
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    for(auto def : defList){
        def->output(level+4);
    }
}

void DefNode::output(int level)
{
    std::string constStr = isConst ? "true" : "false";
    std::string arrayStr = isArray ? "true" : "false";
    fprintf(yyout, "%*cDefNode\tisConst:%s\tisArray:%s\n", level, ' ', constStr.c_str(), arrayStr.c_str());
    id->output(level+4);
    if(initVal == nullptr){
        fprintf(yyout, "%*cnull\n", level+4, ' ');
    }
    else{
        initVal->output(level+4);
    }
}

void InitValNode::addNext(InitValNode* next)
{
    innerList.push_back(next);
}

void InitValNode::output(int level)
{
    std::string constStr = isconst ? "true" : "false";
    fprintf(yyout, "%*cInitValNode\tisConst:%s\n", level, ' ', constStr.c_str());
    for(auto child : innerList)
    {
        child->output(level+4);
    }
    if(leafNode!=nullptr){
        leafNode->output(level+4);
    }
}

void InitValNode::setLeafNode(ExprNode* leaf)
{
    leafNode = leaf;
}

bool InitValNode::isLeaf()
{
    return innerList.empty();
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
    cond->output(level+4);
    bodyStmt->output(level+4);
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
    if(retValue!=nullptr) retValue->output(level + 4);
}

void AssignStmt::output(int level)
{
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void FuncDefParamsNode::addNext(Id* next)
{
    paramsList.push_back(next);
}

std::vector<Type*> FuncDefParamsNode::getParamsType()
{
    std::vector<Type*> typeArray;
    for(auto param : paramsList){
        typeArray.push_back(param->getType());
    }
    return typeArray;
}

//++++++++++函数定义参数
void FuncDefParamsNode::output(int level)
{
    fprintf(yyout, "%*cFuncDefParamsNode\n", level, ' ');
    for(auto param : paramsList){
        param->output(level+4);
    }
}

void FunctionDef::output(int level)
{
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ', 
            name.c_str(), type.c_str());
    if(params!=nullptr){//有参数
        params->output(level+4);
    }
    else{//无参数
        fprintf(yyout, "%*cFuncDefParamsNode NULL\n", level+4, ' ');
    }
    stmt->output(level + 4);
    if(this->voidAddRet != nullptr) {
        voidAddRet->output(level + 4);
    }
}
