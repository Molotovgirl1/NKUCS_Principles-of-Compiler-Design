#include "Instruction.h"
#include "BasicBlock.h"
#include <iostream>
#include <cmath>
#include <assert.h>
#include <string>
#include <limits.h>
#include "Function.h"
#include "Type.h"
#include "MachineCode.h"
using namespace std;
extern FILE *yyout;

Instruction::Instruction(unsigned instType, BasicBlock *insert_bb)
{
    prev = next = this;
    opcode = -1;
    this->instType = instType;
    if (insert_bb != nullptr)
    {
        insert_bb->insertBack(this);
        parent = insert_bb;
    }
}

Instruction::~Instruction()
{
    parent->remove(this);
}

BasicBlock *Instruction::getParent()
{
    return parent;
}

void Instruction::setParent(BasicBlock *bb)
{
    parent = bb;
}

void Instruction::setNext(Instruction *inst)
{
    next = inst;
}

void Instruction::setPrev(Instruction *inst)
{
    prev = inst;
}

Instruction *Instruction::getNext()
{
    return next;
}

Instruction *Instruction::getPrev()
{
    return prev;
}

AllocaInstruction::AllocaInstruction(Operand *dst, SymbolEntry *se, BasicBlock *insert_bb)
     : Instruction(ALLOCA, insert_bb)
{
    operands.push_back(std::move(dst));
    dst->setDef(this);
    this->se = se;
}

AllocaInstruction::~AllocaInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
}

// 动态分配
void AllocaInstruction::output() const
{
    string dst = operands[0]->toStr();
    string type;
    if (se->getType()->isInt()) {
        type = se->getType()->toStr();
        fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(),
            type.c_str());
    }
    else if (se->getType()->isArray()) {
        type = se->getType()->toStr();
        fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(),
            type.c_str());
    }
}

LoadInstruction::LoadInstruction(Operand *dst, Operand *src_addr, BasicBlock *insert_bb) : Instruction(LOAD, insert_bb)
{
    operands.push_back(std::move(dst));
    operands.push_back(std::move(src_addr));
    dst->setDef(this);
    src_addr->addUse(this);
}

LoadInstruction::~LoadInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void LoadInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string src_type;
    std::string dst_type;
    dst_type = operands[0]->getType()->toStr();
    src_type = operands[1]->getType()->toStr();
    fprintf(yyout, "  %s = load %s, %s %s, align 4\n", dst.c_str(), dst_type.c_str(), src_type.c_str(), src.c_str());
}

StoreInstruction::StoreInstruction(Operand *dst_addr, Operand *src, BasicBlock *insert_bb, int paramno) : Instruction(STORE, insert_bb)
{
    operands.push_back(std::move(dst_addr));
    operands.push_back(std::move(src));
    dst_addr->addUse(this);
    src->addUse(this);
    this->paramno = paramno;
}

StoreInstruction::~StoreInstruction()
{
    operands[0]->removeUse(this);
    operands[1]->removeUse(this);
}

void StoreInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string dst_type = operands[0]->getType()->toStr();
    std::string src_type = operands[1]->getType()->toStr();

    fprintf(yyout, "  store %s %s, %s %s, align 4\n", src_type.c_str(), src.c_str(), dst_type.c_str(), dst.c_str());
}

BinaryInstruction::BinaryInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb) : Instruction(BINARY, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(std::move(dst));
    operands.push_back(std::move(src1));
    operands.push_back(std::move(src2));
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}

BinaryInstruction::~BinaryInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void BinaryInstruction::output() const
{
    std::string op;
    std::string dst = operands[0]->toStr();
    std::string src1 = operands[1]->toStr();
    std::string src2 = operands[2]->toStr();
    std::string type = operands[0]->getType()->toStr();
    switch (opcode)
    {
    case ADD:
        op = "add";
        break;
    case SUB:
        op = "sub";
        break;
    case MUL:
        op = "mul";
        break;
    case DIV:
        op = "sdiv";
        break;
    case FADD:
        op = "fadd" ;
        break;
    case FSUB:
        op =  "fsub";
        break;
    case FMUL:
        op = "fmul";
        break;
    case FDIV:
        op = "fdiv" ;
        break;
    case MOD:
        op = "srem";
        break;
    default:
        break;
    }
    fprintf(yyout, "  %s = %s %s %s, %s\n", dst.c_str(), op.c_str(), type.c_str(), src1.c_str(), src2.c_str());
}

UnaryInstruction::UnaryInstruction(unsigned opcode, Operand *dst, Operand *src, BasicBlock *insert_bb)
     : Instruction(UNARY, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(std::move(dst));
    operands.push_back(std::move(src));
    dst->setDef(this);
    src->addUse(this);
}

UnaryInstruction::~UnaryInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void UnaryInstruction::output() const
{
    string s1 = operands[0]->toStr();
    string s2 = operands[1]->toStr();
    string type = operands[1]->getType()->toStr();
    string op;
    if(opcode == UnaryInstruction::ADD){
        op = "add";
        fprintf(yyout, "  %s = %s %s 0, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str());
    }
    else if(opcode == UnaryInstruction::SUB){
        op = "sub";
        fprintf(yyout, "  %s = %s %s 0, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str());
    }
    else if(opcode == UnaryInstruction::NOT){
        op = "icmp ne";
        fprintf(yyout, "  %s = %s %s %s, 0\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str());
    }
    else if(opcode == UnaryInstruction::FADD) {
        op = "fadd";
        fprintf(yyout, "  %s = %s %s 0.0, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str());
    }
    else if(opcode == UnaryInstruction::FSUB) {
        op = "fsub";
        fprintf(yyout, "  %s = %s %s 0.0, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str());
    }
    else if(opcode == UnaryInstruction::FNOT) {
        op = "fcmp une";
        fprintf(yyout, "  %s = %s %s %s, 0.0\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str());
    }
}

CmpInstruction::CmpInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb) : Instruction(CMP, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(std::move(dst));
    operands.push_back(std::move(src1));
    operands.push_back(std::move(src2));
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
    //floatVersion = (src1->getType()->isFloat() || src2->getType()->isFloat());
}

CmpInstruction::~CmpInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void CmpInstruction::output() const
{
    std::string op;
    std::string dst = operands[0]->toStr();
    std::string src1 = operands[1]->toStr();
    std::string src2 = operands[2]->toStr();
    std::string type = operands[1]->getType()->toStr();
    std::string cmp;
    switch (opcode)
    {
    case E:
        op = "eq";
        cmp="icmp";
        break;
    case NE:
        op = "ne";
        cmp="icmp";
        break;
    case L:
        op = "slt";
        cmp="icmp";
        break;
    case LE:
        op = "sle";
        cmp="icmp";
        break;
    case G:
        op = "sgt";
        cmp="icmp";
        break;
    case GE:
        op = "sge";
        cmp="icmp";
        break;
    case FE:
        op = "oeq";
        cmp="fcmp";
        break;
    case FNE:
        op = "une";
        cmp="fcmp";
        break;
    case FL:
        op = "olt";
        cmp="fcmp";
        break;
    case FLE:
        op = "ole";
        cmp="fcmp";
        break;
    case FG:
        op = "ogt";
        cmp="fcmp";
        break;
    case FGE:
        op = "oge";
        cmp="fcmp";
        break;
    default:
        op = "";
        cmp="icmp";
        break;
    }
    fprintf(yyout, "  %s = %s %s %s %s, %s\n", dst.c_str(), cmp.c_str(), op.c_str(), type.c_str(), src1.c_str(), src2.c_str());
}

UncondBrInstruction::UncondBrInstruction(BasicBlock *to, BasicBlock *insert_bb) : Instruction(UNCOND, insert_bb)
{
    branch = to;
}

void UncondBrInstruction::output() const
{
    fprintf(yyout, "  br label %%B%d\n", branch->getNo());
}

void UncondBrInstruction::setBranch(BasicBlock *bb)
{
    branch = bb;
}

BasicBlock *UncondBrInstruction::getBranch()
{
    return branch;
}

CondBrInstruction::CondBrInstruction(BasicBlock *true_branch, BasicBlock *false_branch, Operand *cond, BasicBlock *insert_bb) : Instruction(COND, insert_bb)
{
    this->true_branch = true_branch;
    this->false_branch = false_branch;
    cond->addUse(this);
    operands.push_back(std::move(cond));
}

CondBrInstruction::~CondBrInstruction()
{
    operands[0]->removeUse(this);
}

void CondBrInstruction::output() const
{
    std::string cond = operands[0]->toStr();
    std::string type = operands[0]->getType()->toStr();
    int true_label = true_branch->getNo();
    int false_label = false_branch->getNo();
    fprintf(yyout, "  br %s %s, label %%B%d, label %%B%d\n", type.c_str(), cond.c_str(), true_label, false_label);
}

void CondBrInstruction::setFalseBranch(BasicBlock *bb)
{
    false_branch = bb;
}

BasicBlock *CondBrInstruction::getFalseBranch()
{
    return false_branch;
}

void CondBrInstruction::setTrueBranch(BasicBlock *bb)
{
    true_branch = bb;
}

BasicBlock *CondBrInstruction::getTrueBranch()
{
    return true_branch;
}

CallInstruction::CallInstruction(Operand *dst, SymbolEntry *func, std::vector<Operand *> params, BasicBlock *insert_bb) : Instruction(CALL, insert_bb)
{
    operands.push_back(std::move(dst));
    if (dst != nullptr)
    {
        dst->setDef(this);
    }
    for (auto operand : params)
    {
        operands.push_back(std::move(operand));
        operand->addUse(this);
    }
    this->func = func;
}

CallInstruction::~CallInstruction() {}

void CallInstruction::output() const
{
    fprintf(yyout, "  ");
    if (operands[0] != nullptr)
    {
        fprintf(yyout, "%s = ", operands[0]->toStr().c_str());
    }
    fprintf(yyout, "call %s %s(", ((FunctionType *)(func->getType()))->getRetType()->toStr().c_str(), func->toStr().c_str());
    for (long unsigned int i = 1; i < operands.size(); i++)
    {
        if (i != 1)
        {
            fprintf(yyout, ", ");
        }
        fprintf(yyout, "%s %s", operands[i]->getType()->toStr().c_str(), operands[i]->toStr().c_str());
    }
    fprintf(yyout, ")\n");
}

RetInstruction::RetInstruction(Operand *src, BasicBlock *insert_bb) : Instruction(RET, insert_bb)
{
    if (src != nullptr)
    {
        operands.push_back(std::move(src));
        src->addUse(this);
    }
}

RetInstruction::~RetInstruction()
{
    if (!operands.empty())
        operands[0]->removeUse(this);
}

void RetInstruction::output() const
{
    if (operands.empty())
    {
        fprintf(yyout, "  ret void\n");
    }
    else
    {
        std::string ret = operands[0]->toStr();
        std::string type = operands[0]->getType()->toStr();
        fprintf(yyout, "  ret %s %s\n", type.c_str(), ret.c_str());
    }
}

XorInstruction::XorInstruction(Operand *dst, Operand *src, BasicBlock *insert_bb) : Instruction(XOR, insert_bb)
{
    dst->setDef(this);
    src->addUse(this);
    operands.push_back(dst);
    operands.push_back(src);
}

void XorInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    fprintf(yyout, "  %s = xor i1 %s, true\n", dst.c_str(), src.c_str());
}

XorInstruction::~XorInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

ZextInstruction::ZextInstruction(Operand *dst, Operand *src, bool b2i, BasicBlock *insert_bb) : Instruction(ZEXT, insert_bb)
{
    this->b2i = b2i;
    dst->setDef(this);
    src->addUse(this);
    operands.push_back(std::move(dst));
    operands.push_back(std::move(src));
}

void ZextInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    if (b2i) {
        fprintf(yyout, "  %s = zext i1 %s to i32\n", dst.c_str(), src.c_str());
    }
    else {
        fprintf(yyout, "  %s = zext i32 %s to i1\n", dst.c_str(), src.c_str());
    }
}

ZextInstruction::~ZextInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

GepInstruction::GepInstruction(Operand *dst, Operand *base, std::vector<Operand *> offs, BasicBlock *insert_bb, bool type2) : Instruction(GEP, insert_bb), type2(type2)
{
    operands.push_back(dst);
    operands.push_back(base);
    dst->setDef(this);
    base->addUse(this);
    for (auto off : offs)
    {
        operands.push_back(off);
        off->addUse(this);
    }
}

void GepInstruction::output() const
{
    Operand *dst = operands[0];
    Operand *base = operands[1];
    std::string arrType = base->getType()->toStr();
    if (!type2) {
        fprintf(yyout, "  %s = getelementptr inbounds %s, %s %s, i32 0",
                dst->toStr().c_str(), arrType.substr(0, arrType.size() - 1).c_str(),
                arrType.c_str(), base->toStr().c_str());
    }
    else {
        fprintf(yyout, "  %s = getelementptr inbounds %s, %s %s",
                dst->toStr().c_str(), arrType.substr(0, arrType.size() - 1).c_str(),
                arrType.c_str(), base->toStr().c_str());
    }
    for (unsigned long int i = 2; i < operands.size(); i++) {
        fprintf(yyout, ", i32 %s", operands[i]->toStr().c_str());
    }
    fprintf(yyout, "\n");
}

GepInstruction::~GepInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

F2IInstruction::F2IInstruction(Operand *dst, Operand *src, BasicBlock *insert_bb) : Instruction(FPTSI, insert_bb)
{
    operands.push_back(std::move(dst));
    operands.push_back(std::move(src));
    dst->setDef(this);
    src->addUse(this);
}

void F2IInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    fprintf(yyout, "  %s = fptosi float %s to i32\n", dst.c_str(), src.c_str());
}

I2FInstruction::I2FInstruction(Operand *dst, Operand *src, BasicBlock *insert_bb) : Instruction(SITFP, insert_bb)
{
    operands.push_back(std::move(dst));
    operands.push_back(std::move(src));
    dst->setDef(this);
    src->addUse(this);
}

void I2FInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    fprintf(yyout, "  %s = sitofp i32 %s to float\n", dst.c_str(), src.c_str());
}

//生成操作数码
MachineOperand *Instruction::genMachineOperand(Operand *ope, AsmBuilder *builder = nullptr)
{
    auto se = ope->getEntry();  // 获取操作数的符号表条目

    MachineOperand *mope = nullptr;  // 机器操作数指针

    if (se->isConstant()) {  // 如果符号表条目是常量
        if (se->getType()->isFloat()) {  // 如果常量是浮点数
            // 将浮点数转换为无符号32位整数
            float value = (float)dynamic_cast<ConstantSymbolEntry *>(se)->getValue();
            uint32_t v = reinterpret_cast<uint32_t &>(value);
            mope = new MachineOperand(MachineOperand::IMM, v);  // 创建立即数类型的机器操作数
        } else {
            mope = new MachineOperand(MachineOperand::IMM, dynamic_cast<ConstantSymbolEntry *>(se)->getValue());  // 创建立即数类型的机器操作数
        }
    } else if (se->isTemporary()) {  // 如果符号表条目是临时变量
        if (((TemporarySymbolEntry *)se)->isParam()) {  // 如果临时变量是函数参数
            int argNum = dynamic_cast<TemporarySymbolEntry *>(se)->getArgNum();  // 获取函数参数的序号
            if (se->getType()->isFloat()) {  // 如果参数是浮点数类型
                if (argNum < 16 && argNum >= 0) {  // 如果参数可以使用寄存器
                    mope = new MachineOperand(MachineOperand::REG, argNum, true);  // 创建寄存器类型的机器操作数
                } else {  // 否则需要从栈中加载，生成虚拟寄存器
                    mope = new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel(), true);  // 创建虚拟寄存器类型的机器操作数
                    auto cur_block = builder->getBlock();
                    // 创建LoadMInstruction指令来从栈中加载参数到虚拟寄存器
                    auto cur_inst = new LoadMInstruction(cur_block, LoadMInstruction::VLDR, new MachineOperand(*mope), new MachineOperand(MachineOperand::REG, 11),
                                                        new MachineOperand(MachineOperand::IMM, 4 * -(argNum + 1)));
                    cur_block->InsertInst(cur_inst);
                    cur_block->addUInst(cur_inst);
                }
            } else {
                if (argNum < 4 && argNum >= 0) {  // 如果参数可以使用寄存器
                    mope = new MachineOperand(MachineOperand::REG, argNum);  // 创建寄存器类型的机器操作数
                } else {  // 否则需要从栈中加载，生成虚拟寄存器
                    mope = new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel());  // 创建虚拟寄存器类型的机器操作数
                    auto cur_block = builder->getBlock();
                    // 创建LoadMInstruction指令来从栈中加载参数到虚拟寄存器
                    auto cur_inst = new LoadMInstruction(cur_block, LoadMInstruction::LDR, new MachineOperand(*mope), new MachineOperand(MachineOperand::REG, 11),
                                                        new MachineOperand(MachineOperand::IMM, 4 * -(argNum + 1)));
                    cur_block->InsertInst(cur_inst);
                    cur_block->addUInst(cur_inst);
                }
            }
        } else {
            if (se->getType()->isFloat())  // 如果是浮点数类型的临时变量
                mope = new MachineOperand(MachineOperand::VREG, dynamic_cast<TemporarySymbolEntry *>(se)->getLabel(), true);  // 创建虚拟寄存器类型的机器操作数
            else
                mope = new MachineOperand(MachineOperand::VREG, dynamic_cast<TemporarySymbolEntry *>(se)->getLabel());  // 创建虚拟寄存器类型的机器操作数
        }
    } else if (se->isVariable()) {  // 如果符号表条目是变量
        auto id_se = dynamic_cast<IdentifierSymbolEntry *>(se);
        if (id_se->isGlobal())  // 如果是全局变量
            mope = new MachineOperand(id_se->toStr().c_str() + 1);  // 创建标签类型的机器操作数，去掉前面的@
        else
            exit(0);  // 否则退出程序
    }

    return mope;  // 返回生成的机器操作数指针
}


MachineOperand *Instruction::genMachineReg(int reg, bool fpu = false)
{
    return new MachineOperand(MachineOperand::REG, reg, fpu);
}

MachineOperand *Instruction::genMachineVReg(bool fpu = false)
{
    return new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel(), fpu);
}

MachineOperand *Instruction::genMachineImm(int val)
{
    return new MachineOperand(MachineOperand::IMM, val);
}

MachineOperand *Instruction::genMachineLabel(int block_no)
{
    std::ostringstream buf;
    buf << ".L" << block_no;
    std::string label = buf.str();
    return new MachineOperand(label);
}

// 为函数分配栈空间
void AllocaInstruction::genMachineCode(AsmBuilder *builder)
{
    /* HINT:
     * Allocate stack space for local variabel
     * Store frame offset in symbol entry */
    auto cur_func = builder->getFunction();  // 获取当前函数
    int offset = cur_func->AllocSpace(se->getType()->getSize() / TypeSystem::intType->getSize() * 4);  // 分配栈空间
    dynamic_cast<TemporarySymbolEntry *>(operands[0]->getEntry())->setOffset(-offset);  // 将帧偏移量存储在符号表条目中
}


void LoadInstruction::genMachineCode(AsmBuilder *builder)
{
    /* HINT:
     * Generate machine code for loading a value from memory to a register
     * Supports loading global and local operands, as well as operands from temporary variables */
    
    auto cur_block = builder->getBlock();  // 获取当前基本块
    MachineInstruction *cur_inst = nullptr;
    bool floatVersion = operands[0]->getType()->isFloat();  // 判断是否为浮点数类型
    int ldrOp = floatVersion ? LoadMInstruction::VLDR : LoadMInstruction::LDR;  // 根据浮点数类型选择相应的加载指令
    auto isg = reinterpret_cast<IdentifierSymbolEntry *>(operands[1]->getEntry())->isGlobal();  // 判断是否为全局操作数

    // Load global operand
    if (operands[1]->getEntry()->isVariable() && isg)
    {
        auto dst = genMachineOperand(operands[0]);  // 生成目标寄存器
        auto internal_reg1 = genMachineVReg();  // 生成内部虚拟寄存器
        auto internal_reg2 = new MachineOperand(*internal_reg1);  // 复制内部虚拟寄存器
        auto src = genMachineOperand(operands[1]);  // 生成源操作数

        // example: load r0, addr_a
        cur_inst = new LoadMInstruction(cur_block, LoadMInstruction::LDR, internal_reg1, src);  // 加载全局操作数地址到内部虚拟寄存器
        cur_block->InsertInst(cur_inst);

        // example: load r1, [r0]
        cur_inst = new LoadMInstruction(cur_block, ldrOp, dst, internal_reg2);  // 从内部虚拟寄存器加载值到目标寄存器
        cur_block->InsertInst(cur_inst);
    }
    // Load local operand
    else if (operands[1]->getEntry()->isTemporary() && operands[1]->getDef() && operands[1]->getDef()->isAlloc())
    {
        // example: load r1, [r0, #4]
        auto dst = genMachineOperand(operands[0]);  // 生成目标寄存器
        auto src1 = genMachineReg(11);  // 生成源寄存器
        int offset = dynamic_cast<TemporarySymbolEntry *>(operands[1]->getEntry())->getOffset();  // 获取帧偏移量

        if (AsmBuilder::judge(offset) || offset > -255)  // 判断是否为合法立即数
        {
            cur_inst = new LoadMInstruction(cur_block, ldrOp, dst, src1, genMachineImm(offset));  // 直接加载帧偏移量
            cur_block->InsertInst(cur_inst);
        }
        else
        {
            // 低16位用mov，高16位用两个add
            auto internal_reg = genMachineVReg();  // 生成内部虚拟寄存器
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, internal_reg, genMachineImm(offset & 0xffff)));  // 将低16位帧偏移量加载到内部虚拟寄存器
            if (offset & 0xff0000)
                cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg, internal_reg, genMachineImm(offset & 0xff0000)));  // 将高16位帧偏移量加载到内部虚拟寄存器
            if (offset & 0xff000000)
                cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg, internal_reg, genMachineImm(offset & 0xff000000)));  // 将高16位帧偏移量加载到内部虚拟寄存器
            cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg, internal_reg, src1));  // 将源寄存器与内部虚拟寄存器相加
            cur_block->InsertInst(new LoadMInstruction(cur_block, ldrOp, dst, internal_reg));  // 从内部虚拟寄存器加载值到目标寄存器
        }
    }
    // Load operand from temporary variable
    else {
        // example: load r1, [r0]
        auto dst = genMachineOperand(operands[0]);  // 生成目标寄存器
        auto src = genMachineOperand(operands[1]);  // 生成源操作数
        cur_inst = new LoadMInstruction(cur_block, ldrOp, dst, src);  // 从源操作数加载值到目标寄存器
        cur_block->InsertInst(cur_inst);
    }
}

void StoreInstruction::genMachineCode(AsmBuilder *builder)
{
    auto cur_block = builder->getBlock();  // 获取当前基本块
    MachineInstruction *cur_inst = nullptr;
    
    // 生成源操作数的机器操作数
    auto src = genMachineOperand(operands[1], builder);
    
    // 检查源操作数是否为浮点类型
    bool floatVersion = operands[1]->getType()->isFloat();
    
    // 根据类型确定存储指令的操作码
    int strOp = floatVersion ? StoreMInstruction::VSTR : StoreMInstruction::STR;
    
    // 检查源操作数是否为立即数
    if (src->isImm()) {
        int value=src->getVal(); 
        
        // 生成一个临时虚拟寄存器
        auto internal_r = genMachineVReg();
        
        // 如果立即数可以直接加载到寄存器中
        if (AsmBuilder::judge(value)) {
            // 生成一个移动指令，将立即数加载到虚拟寄存器中
            cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, src);
            cur_block->InsertInst(cur_inst);
        }
        else {
            // 如果立即数需要拆分，使用DeuLegal函数，将一个大的立即数拆分成多个小的立即数，
            DeuLegal(value,internal_r,cur_block);
        }
        
        // 更新源操作数为虚拟寄存器
        src = new MachineOperand(*internal_r);
        
        // 如果源操作数是浮点数，生成一个移动指令进行转换
        if (floatVersion) {
            auto internal_reg = genMachineVReg(true);
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, internal_reg, src));
            src = new MachineOperand(*internal_reg);
        }
    }
    
    // 存储全局操作数
    bool isg = reinterpret_cast<IdentifierSymbolEntry *>(operands[0]->getEntry())->isGlobal();
    
    if (operands[0]->getEntry()->isVariable() && isg) {
        // 生成目标操作数的机器操作数
        auto dst = genMachineOperand(operands[0]);
        
        // 生成内部虚拟寄存器
        auto internal_reg1 = genMachineVReg();
        auto internal_reg2 = new MachineOperand(*internal_reg1);
        
        // 生成一个加载指令，将地址加载到internal_reg1中
        cur_inst = new LoadMInstruction(cur_block, LoadMInstruction::LDR, internal_reg1, dst);
        cur_block->InsertInst(cur_inst);
        
        // 生成一个存储指令，将源操作数存储到[internal_reg1]中
        cur_inst = new StoreMInstruction(cur_block, strOp, src, internal_reg2);
        cur_block->InsertInst(cur_inst);
    }
    // 存储局部操作数
    else if (operands[0]->getEntry()->isTemporary() && operands[0]->getDef() && operands[0]->getDef()->isAlloc()) {
        // 生成目标虚拟寄存器
        auto dst = genMachineReg(11);
        
        // 获取临时变量的偏移量
        int offset = dynamic_cast<TemporarySymbolEntry *>(operands[0]->getEntry())->getOffset();
        
        // 检查偏移量是否可以直接加载到寄存器中，或者是否在范围[-255, 255]内
        if (AsmBuilder::judge(offset) || offset > -255) {
            // 生成一个带有偏移量的存储指令
            cur_inst = new StoreMInstruction(cur_block, strOp, src, dst, genMachineImm(offset));
            cur_block->InsertInst(cur_inst);
        }
        else {
            auto internal_reg = genMachineVReg();
            
            // 将偏移量的低16位加载到internal_reg中
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, internal_reg, genMachineImm(offset & 0xffff)));
            
            // 检查偏移量是否超出低16位范围
            if (offset & 0xff0000)
                cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg, internal_reg, genMachineImm(offset & 0xff0000)));
            if (offset & 0xff000000)
                cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg, internal_reg, genMachineImm(offset & 0xff000000)));
            
            // 将目标寄存器和internal_reg中的偏移量相加
            cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg, internal_reg, dst));
            
            // 生成一个存储指令，将源操作数存储到[internal_reg]中
            cur_block->InsertInst(new StoreMInstruction(cur_block, strOp, src, internal_reg));
        }
    }
    // 从临时变量加载操作数
    else {
        // 生成目标操作数的机器操作数
        auto dst = genMachineOperand(operands[0]);
        
        // 生成一个存储指令，将源操作数存储到目标操作数中
        cur_inst = new StoreMInstruction(cur_block, strOp, src, dst);
        cur_block->InsertInst(cur_inst);
    }
}


void BinaryInstruction::genMachineCode(AsmBuilder *builder)
{
    auto cur_block = builder->getBlock(); // 获取当前基本块
    auto dst = genMachineOperand(operands[0]); // 生成目标操作数的机器操作数
    auto src1 = genMachineOperand(operands[1]); // 生成源操作数1的机器操作数
    auto src2 = genMachineOperand(operands[2]); // 生成源操作数2的机器操作数
    
    MachineInstruction *cur_inst = nullptr; // 当前指令，默认为空指针

    if (src1->isImm()) { // 如果源操作数1是立即数
        int value = src1->getVal(); // 获取立即数值
        auto internal_r = genMachineVReg(); // 生成一个虚拟寄存器
        if (AsmBuilder::judge(value)) { // 如果可以用mov指令将立即数加载到寄存器中（4字节符号整数），就直接使用mov指令
            auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, src1);
            cur_block->InsertInst(cur_inst);
        }
        else { // 否则，用DeuLegal算法生成一系列指令来计算这个立即数
            DeuLegal(value,internal_r,cur_block);
        }
        src1 = new MachineOperand(*internal_r); // 把源操作数1更新为虚拟寄存器的机器操作数

        if (this->opcode>=BinaryInstruction::FADD&&this->opcode<=BinaryInstruction::FDIV) { // 如果是浮点数运算，需要把源操作数1放到V寄存器里
            auto internal_reg = genMachineVReg(true); // 生成一个V寄存器
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, internal_reg, src1)); // 使用vmov指令将源操作数1值从普通寄存器传递到V寄存器中
            src1 = new MachineOperand(*internal_reg); // 把源操作数1更新为V寄存器的机器操作数
        }
    }

    if (src2->isImm()) { // 如果源操作数2是立即数
        if (this->opcode >= BinaryInstruction::FADD && this->opcode <= BinaryInstruction::FDIV) { // 如果是浮点数运算
            int value = src2->getVal(); // 获取立即数值
            auto internal_r = genMachineVReg(); // 生成一个虚拟寄存器
            if (AsmBuilder::judge(value)) { // 如果可以用mov指令将立即数加载到寄存器中（4字节符号整数），就直接使用mov指令
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, src2);
                cur_block->InsertInst(cur_inst);
            }
            else { // 否则，用DeuLegal算法生成一系列指令来计算这个立即数
                DeuLegal(value,internal_r,cur_block);
            }
            src2 = new MachineOperand(*internal_r); // 把源操作数2更新为虚拟寄存器的机器操作数

            auto internal_reg = genMachineVReg(true); // 生成一个V寄存器
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, internal_reg, src2)); // 使用vmov指令将源操作数2值从普通寄存器传递到V寄存器中
            src2 = new MachineOperand(*internal_reg); // 把源操作数2更新为V寄存器的机器操作数
        }
        else if (opcode == MUL || opcode == DIV || opcode == MOD || !AsmBuilder::judge(src2->getVal())) { // 如果是整数运算，按需把源操作数2放到寄存器里
            int value=src2->getVal(); // 获取立即数值
            auto internal_r = genMachineVReg(); // 生成一个虚拟寄存器
            if (AsmBuilder::judge(value)) { // 如果可以用mov指令将立即数加载到寄存器中（4字节符号整数），就直接使用mov指令
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, src2);
                cur_block->InsertInst(cur_inst);
            }
            else { // 否则，用DeuLegal算法生成一系列指令来计算这个立即数
                DeuLegal(value,internal_r,cur_block);
            }
            src2 = new MachineOperand(*internal_r); // 把源操作数2更新为虚拟寄存器的机器操作数
        }
    }

    int thisOpcode = INT_MIN; // 初始值设为int型最小值
    switch (opcode) { // 根据二元运算符类型设置指令类型
    case ADD:
        thisOpcode = BinaryMInstruction::ADD;
        break;
    case SUB:
        thisOpcode = BinaryMInstruction::SUB;
        break;
    case MUL:
        thisOpcode = BinaryMInstruction::MUL;
        break;
    case DIV:
        thisOpcode = BinaryMInstruction::DIV;
        break;
    case AND:
        thisOpcode = BinaryMInstruction::AND;
        break;
    case OR:
        thisOpcode = BinaryMInstruction::OR;
        break;
    case MOD: // 如果是取模运算，需要分步骤进行计算
        break;
    case FADD:
        thisOpcode = BinaryMInstruction::VADD;
        break;
    case FSUB:
        thisOpcode = BinaryMInstruction::VSUB;
        break;
    case FMUL:
        thisOpcode = BinaryMInstruction::VMUL;
        break;
    case FDIV:
        thisOpcode = BinaryMInstruction::VDIV;
        break;
    default:
        break;
    }

    if (opcode != MOD) // 如果不是取模运算，直接生成一条二元运算指令
        cur_inst = new BinaryMInstruction(cur_block, thisOpcode, dst, src1, src2);
    else { // 如果是取模运算，需要分三步进行计算：c = a / b; c = c * b; c = a - c;
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, dst, src1, src2); // c = a / b
        cur_block->InsertInst(cur_inst);
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, new MachineOperand(*dst),
             new MachineOperand(*dst), new MachineOperand(*src2)); // c = c * b
        cur_block->InsertInst(cur_inst);
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::SUB, new MachineOperand(*dst),
             new MachineOperand(*src1), new MachineOperand(*dst)); // c = a - c
    }

    cur_block->InsertInst(cur_inst); // 把指令加到当前基本块的末尾
}


void UnaryInstruction::genMachineCode(AsmBuilder *builder)
{
    auto cur_block = builder->getBlock();
    MachineInstruction *cur_inst = nullptr;
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    MachineOperand *imm0 = nullptr;;

    // 如果是浮点数，那么imm0为0.0，否则为0
    if (operands[0]->getType()->isFloat())
    {
        auto internal_r = genMachineVReg();

        // 将立即数0.0加载到虚拟寄存器中
        Operand* zero = new Operand(new ConstantSymbolEntry(TypeSystem::floatType, 0.0));
        auto MachineZero = genMachineOperand(zero);
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, MachineZero));

        // 将立即数0.0加载到另一个虚拟寄存器中
        auto internal_reg = genMachineVReg(true);
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, internal_reg, MachineZero));

        imm0 = new MachineOperand(*internal_reg);
    }
    else
    {
        auto internal_reg = genMachineVReg();

        // 将立即数0加载到虚拟寄存器中
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, internal_reg, genMachineImm(0)));

        imm0 = new MachineOperand(*internal_reg);
    }

    // 如果src是浮点数，那么要先把它放到寄存器里
    if (src->isImm()) {
        // 浮点数 opcode为FADD和FSUB
        if (this->opcode == FADD || this->opcode == FSUB) {
            int value = src->getVal();
            auto internal_r = genMachineVReg();

            if (AsmBuilder::judge(value)) {
                // 将立即数加载到虚拟寄存器中
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, src);
                cur_block->InsertInst(cur_inst);
            }
            else {
                DeuLegal(value, internal_r, cur_block);
            }

            src = new MachineOperand(*internal_r);

            // 将src加载到另一个虚拟寄存器中
            auto internal_reg = genMachineVReg(true);
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, internal_reg, src));
            src = new MachineOperand(*internal_reg);
        }
        else if(!AsmBuilder::judge(src->getVal())) {
            // 如果是整数，且不是立即数，那么要先把它放到寄存器里
            int value = src->getVal();
            auto internal_r = genMachineVReg();
            DeuLegal(value, internal_r, cur_block);
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, src));
            src = new MachineOperand(*internal_r);
        }
    }

    int op = INT_MIN;
    switch (opcode)
    {
    case ADD:
        op = BinaryMInstruction::ADD;
        cur_inst = new BinaryMInstruction(cur_block, op, dst, imm0, src);
        cur_block->InsertInst(cur_inst);
        break;
    case SUB:
        op = BinaryMInstruction::SUB;
        cur_inst = new BinaryMInstruction(cur_block, op, dst, imm0, src);
        cur_block->InsertInst(cur_inst);
        break;
    case NOT: // 这里没用到，NOT用的是XOR指令
        if (src->isImm())
        {
            auto internal_reg = genMachineVReg();
            
            // 将立即数加载到虚拟寄存器中
            cur_inst = new LoadMInstruction(cur_block, LoadMInstruction::LDR ,internal_reg, src);
            cur_block->InsertInst(cur_inst);
            src = new MachineOperand(*internal_reg);
        }

        // 将src与imm0进行比较
        cur_inst = new CmpMInstruction(cur_block, CmpMInstruction::CMP, src, new MachineOperand(*imm0));
        cur_block->InsertInst(cur_inst);
        break;
    }
}


void CmpInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    auto cur_block = builder->getBlock();
    auto src1 = genMachineOperand(operands[1]);
    auto src2 = genMachineOperand(operands[2]);

    if (src1->isImm())
    {
        // 如果操作数1是立即数，则将其先存入虚拟寄存器中
        int value = src1->getVal(); 

        // 检查立即数是否能直接放入指令，如果不能则调用DeuLegal函数进行处理
        auto internal_r = genMachineVReg();
        if (AsmBuilder::judge(value)) {
            auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, src1);
            cur_block->InsertInst(cur_inst);
        }
        else {
            DeuLegal(value,internal_r,cur_block);
        }
        src1 = new MachineOperand(*internal_r);

        // 如果操作码在FE和FG之间，需要将立即数加载到另一个虚拟寄存器中
        if (this->opcode >= FE && this->opcode <= FG) {
            auto internal_reg = genMachineVReg(true);
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, internal_reg, src1));
            src1 = new MachineOperand(*internal_reg);
        }
    }

    if (src2->isImm())
    {
        // 如果操作数2是立即数，则将其先存入虚拟寄存器中
        int value = src2->getVal(); 
        auto internal_r = genMachineVReg();
        if (AsmBuilder::judge(value)) {
            auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, src2);
            cur_block->InsertInst(cur_inst);
        }
        else {
            DeuLegal(value,internal_r,cur_block);
        }
        src2 = new MachineOperand(*internal_r);

        // 如果操作码在FE和FG之间，需要将立即数加载到另一个虚拟寄存器中
        if (this->opcode >= FE && this->opcode <= FG) {
            auto internal_reg = genMachineVReg(true);
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, internal_reg, src2));
            src2 = new MachineOperand(*internal_reg);
        }
    }

    cur_block->setCmpCond(opcode); //记录条件码

    if (this->opcode >= FE && this->opcode <= FG) {
        // 如果操作码在FE和FG之间，生成VCMP和VMRS指令
        cur_block->InsertInst(new CmpMInstruction(cur_block, CmpMInstruction::VCMP, src1, src2));
        cur_block->InsertInst(new VmrsMInstruction(cur_block));
        // VMRS指令将状态寄存器FPSCR的值保存到变量var里，目前只知道这个指令用于跟在VCMP后面传递flag
    }
    else {
        // 如果不是浮点运算，直接生成普通CMP指令
        cur_block->InsertInst(new CmpMInstruction(cur_block, CmpMInstruction::CMP, src1, src2));
    }

    // 这里借助builder向br指令传递条件
    auto dst = genMachineOperand(operands[0]);
    int OP_TRUE = INT_MIN;
    int OP_FALSE = INT_MIN;

    // 根据操作码设置OP_TRUE和OP_FALSE的值
    if(opcode == E || opcode == FE){
        OP_TRUE = CmpMInstruction::EQ;
        OP_FALSE = CmpMInstruction::NE;
    }
    else if(opcode == NE || opcode == FNE){
        OP_TRUE = CmpMInstruction::NE;
        OP_FALSE = CmpMInstruction::EQ;
    }
    else if(opcode == L || opcode == FL){
        OP_TRUE = CmpMInstruction::LT;
        OP_FALSE = CmpMInstruction::GE;
    }
    else if(opcode == LE || opcode == FLE){
        OP_TRUE = CmpMInstruction::LE;
        OP_FALSE = CmpMInstruction::GT;
    }
    else if(opcode == G || opcode == FG){
        OP_TRUE = CmpMInstruction::GT;
        OP_FALSE = CmpMInstruction::LE;
    }
    else if(opcode == GE || opcode == FGE){
        OP_TRUE = CmpMInstruction::GE;
        OP_FALSE = CmpMInstruction::LT;
    }

    MachineOperand* One = genMachineImm(1);
    MachineOperand* Zero = genMachineImm(0);

    // 根据条件生成MOV指令，将1或0赋值给dst
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst, One, OP_TRUE));
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst, Zero, OP_FALSE));

    // 将OP_TRUE作为条件设置到builder中
    builder->setCond(OP_TRUE);
}


//+++++++控制流指令的翻译:UncondBrInstruction(无条件分支指令)
void UncondBrInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    // 直接生成一条指令就行
    //通过builder获取当前的代码块
    auto cur_block = builder->getBlock();
    //使用genMachineLabel生成与分支号相关的机器操作数
    MachineOperand* dst = genMachineLabel(branch->getNo());
    //通过当前代码块的InsertInst方法插入一条无条件分支指令
    cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::B, dst));
}
//+++++++控制流指令的翻译:condBrInstruction(条件分支指令)
void CondBrInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    // 该指令跟在cmp指令后面
    // 生成两条指令
    //获取当前代码块以及条件
    auto cur_block = builder->getBlock();
    auto Cond = builder->getCond();
    //通过genMachineLabel生成真分支号的目标操作数
    auto dst = genMachineLabel(true_branch->getNo());
    //在当前代码块插入该条件分支指令,条件成立则跳转到相应的代码块
    cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::B, dst, Cond));//条件跳转
    //生成假分支号的目标操作数
    dst = genMachineLabel(false_branch->getNo());
    //在当前代码块插入一条无条件分支指令，条件不成立则跳转到相应的代码块
    cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::B, dst));
}
//+++++++控制流指令的翻译:RetInstruction
void RetInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    /* HINT:
     * 1. Generate mov instruction to save return value in r0
     * 2. Restore callee saved registers and sp, fp
     * 3. Generate bx instruction */
     //获取当前基本块
    auto cur_bb = builder->getBlock();
    // 检查是否有返回值
    if (operands.size() > 0) {
        //生成返回值的机器代码
        auto ret_value = genMachineOperand(operands[0]);
        //如果返回值为立即数
        if (ret_value->isImm()){
            //获取立即数的值
            int value=ret_value->getVal(); 
            //生成一个虚拟寄存器的活跃区间，用于存储立即数
            auto internal_r = genMachineVReg();
            //判断是否可以使用 MOV 指令将立即数加载到虚拟寄存器中
            if (AsmBuilder::judge(value)) {
                //生成相应的 MovMInstruction 指令，将立即数加载到虚拟寄存器中
                auto cur_inst = new MovMInstruction(cur_bb, MovMInstruction::MOV, internal_r, ret_value);//将立即数加载到虚拟寄存器中
                cur_bb->InsertInst(cur_inst);
            }
            else
                DeuLegal(value,internal_r,cur_bb);
            //将返回值更新为新生成的虚拟寄存器的活跃区间
            ret_value = new MachineOperand(*internal_r);
            //internal_reg1 = new MachineOperand(*internal_r);
        }
        //判断返回值的类型是否为浮点数类型
        if (operands[0]->getType()->isFloat()) {
            //判断 ret_value 是否是浮点寄存器
            if (ret_value->isFReg())
                //使用 VMOV32 指令将浮点寄存器的值移动到通用寄存器 r0
                cur_bb->InsertInst(new MovMInstruction(cur_bb, MovMInstruction::VMOV32, genMachineReg(0, true), ret_value));
            else // 同样的，这种情况是返回立即数，把立即数放到r0寄存器里了
                cur_bb->InsertInst(new MovMInstruction(cur_bb, MovMInstruction::VMOV, genMachineReg(0, true), ret_value));
        }
        else
            //使用 MOV 指令将返回值移动到通用寄存器 r0 中
            cur_bb->InsertInst(new MovMInstruction(cur_bb, MovMInstruction::MOV, genMachineReg(0), ret_value));
    }
    auto sp = genMachineReg(13);
    // 释放栈空间，通过一条 MOV 指令将栈指针 sp 设置为帧指针 fp
    auto cur_inst = new MovMInstruction(cur_bb, MovMInstruction::MOV, sp, genMachineReg(11));
    cur_bb->InsertInst(cur_inst);
    // 恢复保存的寄存器
    //创建一个 VPOP 操作的 StackMInstrcuton 对象，并将其插入到当前基本块中
    auto curr_inst = new StackMInstrcuton(cur_bb, StackMInstrcuton::VPOP, {});
    cur_bb->InsertInst(curr_inst);
    //使用 addUInst 将该指令添加到基本块的未命名指令列表中
    cur_bb->addUInst(curr_inst);
    //创建一个 POP 操作的 StackMInstrcuton 对象，并将其插入到当前基本块中
    curr_inst = new StackMInstrcuton(cur_bb, StackMInstrcuton::POP, {});
    cur_bb->InsertInst(curr_inst);
    //使用 addUInst 将该指令添加到基本块的未命名指令列表中
    cur_bb->addUInst(curr_inst);
    // 使用 BX 指令跳转到保存在寄存器 r14 中的返回地址
    cur_bb->InsertInst(new BranchMInstruction(cur_bb, BranchMInstruction::BX, genMachineReg(14)));
}

//+++++++函数调用的翻译:CallInstruction
void CallInstruction::genMachineCode(AsmBuilder *builder)
{
    auto cur_block = builder->getBlock();
    long unsigned int i;
    int sum = 0;
    //将非浮点数类型的参数传递到通用寄存器 r0-r3 
    for (i = 1; i <= operands.size() - 1 && sum < 4; i++) {
        if (operands[i]->getType()->isFloat())
            continue;
        //对于非浮点数类型的参数，生成该参数的机器操作数
        auto param = genMachineOperand(operands[i]);
        // 如果参数是立即数
        if (param->isImm()) {
            int value=param->getVal(); 
            //生成虚拟寄存器
            auto internal_r = genMachineVReg();
            //判断是否可以使用 MOV 指令将立即数加载到虚拟寄存器中
            if (AsmBuilder::judge(value)) {
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, param);//将立即数加载到虚拟寄存器中
                cur_block->InsertInst(cur_inst);
            }
            else {
                DeuLegal(value,internal_r,cur_block);
            }
            param = new MachineOperand(*internal_r);
        }
        // 用mov指令把参数放到对应寄存器里
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, genMachineReg(sum), param));
        sum++;
    }
    int intLastPos = i;
    // 将浮点数类型的参数传递到浮点寄存器 s0-s15 中
    sum = 0;
    //循环遍历函数调用的参数
    for (i = 1; i <= operands.size() - 1 && sum < 16; i++) {
        if (!operands[i]->getType()->isFloat())
            continue;
        //生成该参数的机器操作数
        auto param = genMachineOperand(operands[i]);
        //如果参数是立即数
        if (param->isImm()) {
            int value=param->getVal(); 
            // 生成一个虚拟寄存器，然后判断是否可以使用 MOV 指令将立即数加载到虚拟寄存器中
            auto internal_r = genMachineVReg();
            if (AsmBuilder::judge(value)) {
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, param);//将立即数加载到虚拟寄存器中
                cur_block->InsertInst(cur_inst);
            }
            else {
                DeuLegal(value,internal_r,cur_block);
            }
            //参数更新为生成的虚拟寄存器
            param = new MachineOperand(*internal_r);
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, genMachineReg(sum, true), param));
        }
        else {
            // 用mov指令把参数放到对应寄存器里
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV32, genMachineReg(sum, true), param));
        }
        sum++;
    }
    int floatLastPos = i;
    int param_size_in_stack = 0;
    // 将函数调用的参数从后向前推入栈中
    //从后向前的循环遍历函数调用的参数
    for (long unsigned int i = operands.size() - 1; i >= 1; i--) {
        //检查每个参数类型以及其在浮点数和整数参数中的位置
        //如果参数是浮点数类型且其位置在浮点数参数的最后位置之前
        if (operands[i]->getType()->isFloat() && i < floatLastPos)
            continue;
        //如果参数是整数类型且其位置在整数参数的最后位置之前
        if (!operands[i]->getType()->isFloat() && i < intLastPos)
            continue;//不处理该参数
        //生成该参数的机器操作数
        auto param = genMachineOperand(operands[i]);
        //如果参数是浮点寄存器类型
        if (param->isFReg())
            //使用 VPUSH 操作将参数推入栈中
            cur_block->InsertInst(new StackMInstrcuton(cur_block, StackMInstrcuton::VPUSH, {param}));
        else {
            //如果是立即数
            if (param->isImm()) {
                int value=param->getVal(); 
                //生成虚拟寄存器
                auto internal_r = genMachineVReg();
                //将立即数加载到虚拟寄存器中
                if (AsmBuilder::judge(value)) {
                    auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, param);//将立即数加载到虚拟寄存器中
                    cur_block->InsertInst(cur_inst);
                }
                else {
                    DeuLegal(value, internal_r, cur_block);
                }
                param = new MachineOperand(*internal_r);
            }
            //使用 PUSH 操作将虚拟寄存器推入栈中
            cur_block->InsertInst(new StackMInstrcuton(cur_block, StackMInstrcuton::PUSH, {param}));
        }
        //更新参数在栈中所占的空间大小
        param_size_in_stack += 4;
    }
    // 生成bl指令，调用目标函数
    cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::BL, new MachineOperand(func->toStr().c_str())));
    // 生成add指令释放栈空间
    //如果栈上有参数
    if (param_size_in_stack > 0) {
        //获取栈指针寄存器（SP）
        auto sp = genMachineReg(13);
        //生成参数在栈上所占的空间大小
        auto stack_size = genMachineImm(param_size_in_stack);
        //参数大小是立即数
        if (AsmBuilder::judge(param_size_in_stack))
            //生成 ADD 指令，将栈指针向上调整，释放栈空间
            cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, sp, sp, stack_size));
        else{
            //生成虚拟寄存器，将立即数加载到虚拟寄存器中，然后再生成 ADD 指令
            auto ret_value = genMachineOperand(operands[0]);
        //处理返回值
        //检查返回值是否为立即数
        if (ret_value->isImm()){
            //获取其值并生成一个虚拟寄存器
            int value=stack_size->getVal(); 
            auto internal_r = genMachineVReg();
            //判断是否可以使用 MOV 指令将立即数加载到虚拟寄存器中
            if (AsmBuilder::judge(value)) {
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, stack_size);//将立即数加载到虚拟寄存器中
                cur_block->InsertInst(cur_inst);
            }
            else 
                DeuLegal(value,internal_r,cur_block);
            cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, sp, sp, new MachineOperand(*internal_r)));
        }
        }
    }
    //处理返回值不为立即数的情况
    if (operands[0]) {
        //浮点数类型，使用 VMOV32 指令将浮点寄存器的值移动到相应的位置
        if (operands[0]->getType()->isFloat())
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV32, genMachineOperand(operands[0]), genMachineReg(0, true)));
        else
        //不是浮点数类型，使用 MOV 指令将通用寄存器的值移动到相应的位置
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, genMachineOperand(operands[0]), genMachineReg(0)));
    }
}

void XorInstruction::genMachineCode(AsmBuilder *builder)
{
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    cur_block->InsertInst(new CmpMInstruction(cur_block, CmpMInstruction::CMP, src, genMachineImm(0)));
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst, genMachineImm(1), CmpMInstruction::EQ));
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst, genMachineImm(0), CmpMInstruction::NE));
    builder->setCond(CmpMInstruction::EQ);
}

void ZextInstruction::genMachineCode(AsmBuilder *builder)
{
    // 生成一条mov指令得了
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src));
}

void GepInstruction::genMachineCode(AsmBuilder *builder)
{
    // type2表示是不是通过传参传过来的数组指针，为true表示是，否则表示局部变量或者全局变量
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;
    auto dst = genMachineOperand(operands[0]);
    // 这里就是对于局部变量或者全局变量，要先把它们地址放到一个临时寄存器里，
    // 而函数参数，其实operand[1]就存的有地址
    auto base = type2 ? genMachineOperand(operands[1]) : genMachineVReg();
    // 全局变量，先load
    if (operands[1]->getEntry()->isVariable() && dynamic_cast<IdentifierSymbolEntry *>(operands[1]->getEntry())->isGlobal())
    {
        auto src = genMachineOperand(operands[1]);//获取数组的首地址 addr
        cur_block->InsertInst(new LoadMInstruction(cur_block, LoadMInstruction::LDR, base, src));
        base = new MachineOperand(*base);
    }
    else if (type2 != 1) // 局部变量
    {
        // 偏移都是负数
        int offset = ((TemporarySymbolEntry *)operands[1]->getEntry())->getOffset();
        auto off = genMachineImm(offset);
        if (AsmBuilder::judge(offset) || offset > -255)//如果偏移量在-255~255之间，直接用add指令
        {
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, base, genMachineReg(11), off);
            cur_block->InsertInst(cur_inst);
            base = new MachineOperand(*base);
        }
        else
        {
            int value = off->getVal(); 
            auto internal_r = genMachineVReg();
            if (AsmBuilder::judge(value)) {
                cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, off);//将立即数加载到虚拟寄存器中
                cur_block->InsertInst(cur_inst);
            }
            else {
                DeuLegal(value,internal_r,cur_block);
            }
            //src = new MachineOperand(*internal_reg);
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, base, genMachineReg(11), new MachineOperand(*internal_r));
            cur_block->InsertInst(cur_inst);
            base = new MachineOperand(*base);
        }
    }
    // 计算地址偏移量
    Type *arrType = dynamic_cast<PointerType* >(operands[1]->getType())->getType();
    std::vector<int> indexs = dynamic_cast<ArrayType* >(arrType)->getIndexs();
    std::vector<int> imms; // a[2][i][0]===>{0, 2}
    for (unsigned long int i = 2; i < operands.size(); i++) {
        if (operands[i]->getEntry()->isConstant()) {
            // 为了省代码，所有的立即数一起算，这里先跳过
            imms.push_back(i);
            continue;
        }
        unsigned int step = 4;
        for (unsigned long int j = i - (type2 ? 2 : 1); j < indexs.size(); j++) {
            step *= indexs[j];
        }
        auto off = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, LoadMInstruction::LDR, off, genMachineImm(step));
        cur_block->InsertInst(cur_inst);
        auto internal_reg1 = genMachineVReg();
        auto src1 = genMachineOperand(operands[i]);
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, internal_reg1, src1, off);
        cur_block->InsertInst(cur_inst);
        auto internal_reg2 = genMachineVReg();
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg2, new MachineOperand(*base), new MachineOperand(*internal_reg1));
        cur_block->InsertInst(cur_inst);
        base = new MachineOperand(*internal_reg2);
    }
    int off = 0;
    for (auto index : imms) {
        int imm = ((ConstantSymbolEntry *)operands[index]->getEntry())->getValue();
        unsigned int step = 4;
        for (unsigned long int j = index - (type2 ? 2 : 1); j < indexs.size(); j++) {
            step *= indexs[j];
        }
        off += (imm * step);
    }
    if (off > 0) {
        auto internal_reg1 = genMachineImm(off);
        if (!AsmBuilder::judge(off)) {
            int value=internal_reg1->getVal(); 
            auto internal_r = genMachineVReg();
            if (AsmBuilder::judge(value)) {
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_r, internal_reg1);//将立即数加载到虚拟寄存器中
                cur_block->InsertInst(cur_inst);
            }
            else {
                DeuLegal(value,internal_r,cur_block);
            }
            //src = new MachineOperand(*internal_reg);
            internal_reg1 = new MachineOperand(*internal_r);
        }
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, new MachineOperand(*base), new MachineOperand(*base), new MachineOperand(*internal_reg1));
        cur_block->InsertInst(cur_inst);
    }
    // for (unsigned long int i = 2; i < operands.size(); i++)
    // {
    //     unsigned int step = 4;
    //     for (unsigned long int j = i - (type2 ? 2 : 1); j < indexs.size(); j++)
    //     {
    //         step *= indexs[j];
    //     }
    //     auto off = genMachineVReg();
    //     cur_block->InsertInst(new LoadMInstruction(cur_block, off, genMachineImm(step)));
    //     auto internal_reg1 = genMachineVReg();
    //     auto src1 = genMachineOperand(operands[i]);
    //     if (src1->isImm())
    //     {
    //         auto internal_reg = genMachineVReg();
    //         cur_block->InsertInst(new LoadMInstruction(cur_block, internal_reg, src1));
    //         src1 = new MachineOperand(*internal_reg);
    //     }
    //     cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, internal_reg1, src1, off));
    //     auto internal_reg2 = genMachineVReg();
    //     cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg2, new MachineOperand(*base), new MachineOperand(*internal_reg1)));
    //     base = new MachineOperand(*internal_reg2);
    // }
    cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, base);
    cur_block->InsertInst(cur_inst);
}

void F2IInstruction::genMachineCode(AsmBuilder *builder)
{ // 浮点转int
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    if (src->isImm())
    { // 按理说立即数其实可以不用这条指令的，我们直接强制类型转化一下就行
        int value=src->getVal();
        auto internal_reg = genMachineVReg();
            if (AsmBuilder::judge(value)) {
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_reg, src);//将立即数加载到虚拟寄存器中
                cur_block->InsertInst(cur_inst);
            }
            else
                DeuLegal(value,internal_reg,cur_block);
        src = new MachineOperand(*internal_reg);
    }
    if (src->isFReg())
    { // 如果src本来就是个浮点寄存器
        //vcvt 两个寄存器都必须是浮点寄存器。
        auto internal_reg = genMachineVReg(true);
        ////007db5d0:   vcvt.u32.f64 s15, d7 ;s15==0 
        cur_block->InsertInst(new VcvtMInstruction(cur_block, VcvtMInstruction::FTS, internal_reg, src));
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, dst, internal_reg));
    }
    else
    {
        // 这种情况可能是浮点立即数转int
        auto internal_reg = genMachineVReg(true);
        //VMOV用于将立即数插入到一个单精度/ 双精度的寄存器中。
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, internal_reg, src));
        cur_block->InsertInst(new VcvtMInstruction(cur_block, VcvtMInstruction::FTS, internal_reg, internal_reg));
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, dst, internal_reg));
    }
}

void I2FInstruction::genMachineCode(AsmBuilder *builder)
{
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    if (src->isImm())
    {
        int value=src->getVal();
        auto internal_reg = genMachineVReg();
            if (AsmBuilder::judge(value)) {
                auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, internal_reg, src);//将立即数加载到虚拟寄存器中
                cur_block->InsertInst(cur_inst);
            }
            else
                DeuLegal(value,internal_reg,cur_block);
        src = new MachineOperand(*internal_reg);
    }
    assert(dst->isFReg());
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::VMOV, dst, src));
    cur_block->InsertInst(new VcvtMInstruction(cur_block, VcvtMInstruction::STF, dst, dst));
}

void Instruction::DeuLegal(int imm,MachineOperand* internal_reg,MachineBlock* cur_block){
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, internal_reg, genMachineImm(imm & 0xffff)));
        if (imm & 0xff0000)
            cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg, internal_reg, genMachineImm(imm & 0xff0000)));
        if (imm & 0xff000000)
            cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, internal_reg, internal_reg, genMachineImm(imm & 0xff000000)));
}