#include "MachineCode.h"
#include "AsmBuilder.h"
#include "Type.h"
#include <iostream>
extern FILE *yyout;

MachineOperand::MachineOperand(int tp, int val, bool fpu)
{
    this->type = tp;
    this->fpu = fpu;
    if (tp == MachineOperand::IMM)
        this->val = val;
    else
        this->reg_no = val;
}

MachineOperand::MachineOperand(std::string label)
{
    this->type = MachineOperand::LABEL;
    this->label = label;
}

bool MachineOperand::operator==(const MachineOperand &a) const
{
    if (this->type != a.type)
        return false;
    if (this->fpu != a.fpu)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

bool MachineOperand::operator<(const MachineOperand &a) const
{
    if (this->type == a.type)
    {
        if (this->type == IMM)
            return this->val < a.val;
        return this->reg_no < a.reg_no;
    }
    return this->type < a.type;

    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

void MachineOperand::PrintReg()
{
    switch (reg_no)
    {
    case 11:
        fprintf(yyout, "fp");
        break;
    case 13:
        fprintf(yyout, "sp");
        break;
    case 14:
        fprintf(yyout, "lr");
        break;
    case 15:
        fprintf(yyout, "pc");
        break;
    default:
        if (fpu) // 浮点寄存器用s
            fprintf(yyout, "s%d", reg_no);
        else
            fprintf(yyout, "r%d", reg_no);
        break;
    }
}

void MachineOperand::output()
{
    /* HINT：print operand
     * Example:
     * immediate num 1 -> print #1;
     * register 1 -> print r1;
     * lable addr_a -> print addr_a; */

    auto parent = this->getParent(); // 获取父指令

    switch (this->type)
    {
    case IMM:
        fprintf(yyout, "#%d", this->val); // 如果操作数类型是立即数，输出"#立即数的值"
        break;
    case VREG:
    {
        if (fpu) // 如果是浮点虚拟寄存器，打印"f"，用于调试
            fprintf(yyout, "f%d", this->reg_no);
        else
            fprintf(yyout, "v%d", this->reg_no); // 输出向量寄存器
    }
    break;
    case REG:
        PrintReg(); // 输出寄存器
        break;
    case LABEL:
        if (this->label.substr(0, 2) == ".L") // 如果标签以".L"开头，表示是一个标签
            fprintf(yyout, "%s", this->label.c_str()); // 输出标签名
        else if (this->label.substr(0, 1) == "@") // 如果标签以"@"开头，表示是一个函数
            fprintf(yyout, "%s", this->label.c_str() + 1); // 输出函数名
        else // 否则，表示是一个变量
            fprintf(yyout, "addr_%s_%d", this->label.c_str(),
                    parent->getParent()->getParent()->getParent()->getLtorgNum()); // 输出变量名
    default:
        break;
    }
}


void MachineInstruction::PrintCond()
{
    switch (cond)
    {
    case EQ:
        fprintf(yyout, "eq");
        break;
    case NE:
        fprintf(yyout, "ne");
        break;
    case LT:
        fprintf(yyout, "lt");
        break;
    case LE:
        fprintf(yyout, "le");
        break;
    case GT:
        fprintf(yyout, "gt");
        break;
    case GE:
        fprintf(yyout, "ge");
        break;
    default:
        break;
    }
}

BinaryMInstruction::BinaryMInstruction(
    MachineBlock *p, int op,
    MachineOperand *dst, MachineOperand *src1, MachineOperand *src2,
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::BINARY;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    src2->setParent(this);
}

void BinaryMInstruction::output()
{
    // TODO:
    // Complete other instructions
    switch (this->op)
    {
    case BinaryMInstruction::ADD:
        fprintf(yyout, "\tadd ");
        break;
    case BinaryMInstruction::SUB:
        fprintf(yyout, "\tsub ");
        break;
    case BinaryMInstruction::MUL:
        fprintf(yyout, "\tmul ");
        break;
    case BinaryMInstruction::DIV:
        fprintf(yyout, "\tsdiv ");
        break;
    case BinaryMInstruction::AND:
        fprintf(yyout, "\tand ");
        break;
    case BinaryMInstruction::OR:
        fprintf(yyout, "\torr ");
        break;
    case BinaryMInstruction::VADD:
        fprintf(yyout, "\tvadd.f32 ");
        break;
    case BinaryMInstruction::VSUB:
        fprintf(yyout, "\tvsub.f32 ");
        break;
    case BinaryMInstruction::VMUL:
        fprintf(yyout, "\tvmul.f32 ");
        break;
    case BinaryMInstruction::VDIV:
        fprintf(yyout, "\tvdiv.f32 ");
        break;
    default:
        break;
    }
    this->PrintCond();
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[1]->output();
    fprintf(yyout, "\n");
}

// LoadMInstruction类的构造函数
LoadMInstruction::LoadMInstruction(MachineBlock *p, int op,
                                   MachineOperand *dst, MachineOperand *src1, MachineOperand *src2,
                                   int cond)
{
    this->parent = p; // 设置父指针为给定的基本块指针
    this->type = MachineInstruction::LOAD; // 设置指令类型为加载操作
    this->op = op; // 设置操作码
    this->cond = cond; // 设置条件码
    this->def_list.push_back(dst); // 将目标操作数添加到def_list中
    this->use_list.push_back(src1); // 将第一个源操作数添加到use_list中
    dst->setParent(this); // 设置目标操作数的父指令为当前指令
    src1->setParent(this); // 设置第一个源操作数的父指令为当前指令
    
    if (src2)
    {
        this->use_list.push_back(src2); // 如果存在第二个源操作数，将其添加到use_list中
        src2->setParent(this); // 设置第二个源操作数的父指令为当前指令
    }
}

// LoadMInstruction类的输出函数
void LoadMInstruction::output()
{
    switch (op)
    {
    case LDR:
        fprintf(yyout, "\tldr "); // 如果操作码是LDR，输出"ldr "
        break;
    case VLDR:
        fprintf(yyout, "\tvldr.32 "); // 如果操作码是VLDR，输出"vldr.32 "
        break;
    default:
        break;
    }
    
    this->def_list[0]->output(); // 输出目标操作数
    
    fprintf(yyout, ", ");
    
    // 如果源操作数1是立即数，输出"=立即数的值"
    if (this->use_list[0]->isImm())
    {
        fprintf(yyout, "=%d\n", this->use_list[0]->getVal());
        return;
    }

    // 加载地址
    if (this->use_list[0]->isReg() || this->use_list[0]->isVReg())
        fprintf(yyout, "["); // 如果源操作数1是寄存器或向量寄存器，输出"["

    this->use_list[0]->output(); // 输出源操作数1
    
    if (this->use_list.size() > 1)
    {
        fprintf(yyout, ", ");
        this->use_list[1]->output(); // 如果存在源操作数2，输出源操作数2
    }

    if (this->use_list[0]->isReg() || this->use_list[0]->isVReg())
        fprintf(yyout, "]"); // 如果源操作数1是寄存器或向量寄存器，输出"]"
        
    fprintf(yyout, "\n"); // 输出换行符
}


StoreMInstruction::StoreMInstruction(MachineBlock *p, int op,
                                     MachineOperand *src, MachineOperand *dst, MachineOperand *off,
                                     int cond)
{
    // 设置指令所属的基本块
    this->parent = p;
    // 设置指令类型为存储操作
    this->type = MachineInstruction::STORE;
    // 设置指令的操作码
    this->op = op;
    // 设置指令的条件码
    this->cond = cond;
    // 将源操作数和目标操作数添加到use_list中
    use_list.push_back(src);
    use_list.push_back(dst);
    // 设置源操作数和目标操作数的父指令为当前指令
    src->setParent(this);
    dst->setParent(this);
    // 如果有偏移地址操作数，将其添加到use_list中，并设置其父指令为当前指令
    if (off != nullptr)
    {
        use_list.push_back(off);
        off->setParent(this);
    }
}

void StoreMInstruction::output()
{
    // 根据操作码选择合适的指令助记符进行输出
    switch (op)
    {
    case STR:
        fprintf(yyout, "\tstr ");
        break;
    case VSTR:
        fprintf(yyout, "\tvstr.32 ");
        break;
    default:
        break;
    }
    // 输出源操作数
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    // 输出目标操作数
    // 如果目标操作数是寄存器或向量寄存器，则需要加上方括号
    if (this->use_list[1]->isReg() || this->use_list[1]->isVReg())
        fprintf(yyout, "[");
    this->use_list[1]->output();
    // 如果有偏移地址操作数，则输出它
    if (this->use_list.size() > 2)
    {
        fprintf(yyout, ", ");
        this->use_list[2]->output();
    }
    // 如果目标操作数是寄存器或向量寄存器，则需要加上方括号
    if (this->use_list[1]->isReg() || this->use_list[1]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");   // 输出换行符
}

MovMInstruction::MovMInstruction(MachineBlock *p, int op,
                                 MachineOperand *dst, MachineOperand *src,
                                 int cond)
{
    // 设置指令所属的基本块
    this->parent = p;
    // 设置指令类型为移动操作
    this->type = MachineInstruction::MOV;
    // 设置指令的操作码
    this->op = op;
    // 设置指令的条件码
    this->cond = cond;
    // 将目标操作数和源操作数添加到def_list和use_list中
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    // 设置目标操作数和源操作数的父指令为当前指令
    dst->setParent(this);
    src->setParent(this);
}

void MovMInstruction::output()
{
    // 根据操作码选择合适的指令助记符进行输出
    switch (op)
    {
    case MOV:
        fprintf(yyout, "\tmov");
        break;
    case VMOV:
        fprintf(yyout, "\tvmov");
        break;
    case VMOV32:
        fprintf(yyout, "\tvmov.f32");
        break;
    case MVN: // 这个暂时用不到
    default:
        break;
    }
    PrintCond();   // 输出条件码
    fprintf(yyout, " ");
    def_list[0]->output();   // 输出目标操作数
    fprintf(yyout, ", ");
    use_list[0]->output();   // 输出源操作数
    fprintf(yyout, "\n");   // 输出换行符
}


BranchMInstruction::BranchMInstruction(MachineBlock *p, int op,
                                       MachineOperand *dst,
                                       int cond)
{
    // TODO
    this->parent = p;   // 设置指令所属的基本块
    this->type = MachineInstruction::BRANCH;   // 设置指令类型为分支操作
    this->op = op;   // 设置指令的操作码
    this->cond = cond;   // 设置指令的条件码
    this->use_list.push_back(dst);   // 将dst添加到use_list中
    dst->setParent(this);   // 设置dst的父指令为当前指令
}

void BranchMInstruction::output()
{
    // TODO
    switch (op)
    {
    case B:
        fprintf(yyout, "\tb");   // 输出B指令助记符
        break;
    case BX:
        fprintf(yyout, "\tbx");   // 输出BX指令助记符
        break;
    case BL:
        fprintf(yyout, "\tbl");   // 输出BL指令助记符
        break;
    default:
        break;
    }
    PrintCond();   // 输出条件码
    fprintf(yyout, " ");   // 输出空格
    use_list[0]->output();   // 输出目标操作数
    fprintf(yyout, "\n");   // 输出换行符
}


CmpMInstruction::CmpMInstruction(MachineBlock *p, int op,
                                 MachineOperand *src1, MachineOperand *src2,
                                 int cond)
{
    // TODO
    this->parent = p;   // 设置指令所属的基本块
    this->type = MachineInstruction::CMP;   // 设置指令类型为比较操作
    this->op = op;   // 设置指令的操作码
    this->cond = cond;   // 设置指令的条件码
    p->setCmpCond(cond);   // 将条件码设置到基本块中
    use_list.push_back(src1);   // 将src1添加到use_list中
    use_list.push_back(src2);   // 将src2添加到use_list中
    src1->setParent(this);   // 设置src1的父指令为当前指令
    src2->setParent(this);   // 设置src2的父指令为当前指令
}

void CmpMInstruction::output()
{
    // TODO
    switch (op)
    {
    case CMP:
        fprintf(yyout, "\tcmp ");   // 输出CMP指令助记符
        break;
    case VCMP:
        fprintf(yyout, "\tvcmp.f32 ");   // 输出VCMP指令助记符
        break;
    default:
        break;
    }
    use_list[0]->output();   // 输出第一个操作数
    fprintf(yyout, ", ");   // 输出逗号分隔符
    use_list[1]->output();   // 输出第二个操作数
    fprintf(yyout, "\n");   // 输出换行符
}

StackMInstrcuton::StackMInstrcuton(MachineBlock *p, int op,
                                   std::vector<MachineOperand *> srcs,
                                   int cond)
{
    // TODO
    this->parent = p;   // 设置指令所属的基本块
    this->type = MachineInstruction::STACK;   // 设置指令类型为栈操作
    this->op = op;   // 设置指令的操作码
    this->cond = cond;   // 设置指令的条件码
    for (auto operand : srcs)
    {
        use_list.push_back(operand);   // 将srcs中的操作数添加到use_list中
        operand->setParent(this);   // 设置操作数的父指令为当前指令
    }
}

void StackMInstrcuton::output()
{
    // TODO
    switch (op)
    {
    case POP:
        fprintf(yyout, "\tpop ");   // 输出POP指令助记符
        break;
    case PUSH:
        fprintf(yyout, "\tpush ");   // 输出PUSH指令助记符
        break;
    case VPOP:
        fprintf(yyout, "\tvpop ");   // 输出VPOP指令助记符
        break;
    case VPUSH:
        fprintf(yyout, "\tvpush ");   // 输出VPUSH指令助记符
        break;
    default:
        break;
    }
    fprintf(yyout, "{");   // 输出左括号
    for (long unsigned int i = 0; i < use_list.size(); i++)
    {
        if (i != 0)
        {
            fprintf(yyout, ", ");   // 输出逗号分隔符
        }
        use_list[i]->output();   // 输出操作数
    }
    fprintf(yyout, "}\n");   // 输出右括号和换行符
}

VcvtMInstruction::VcvtMInstruction(MachineBlock *p, int op, MachineOperand *dst, MachineOperand *src, int cond)
{
    this->parent = p;
    this->type = MachineInstruction::VCVT;
    this->op = op;
    this->cond = cond;
    def_list.push_back(dst);
    use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
}

void VcvtMInstruction::output()
{
    switch (op)
    {
    case FTS:
        fprintf(yyout, "\tvcvt.s32.f32 ");   // 输出指令助记符
        break;
    case STF:
        fprintf(yyout, "\tvcvt.f32.s32 ");
        break;
    default:
        break;
    }
    def_list[0]->output();   // 输出目标操作数
    fprintf(yyout, ", ");
    use_list[0]->output();   // 输出源操作数
    fprintf(yyout, "\n");   // 换行
}


VmrsMInstruction::VmrsMInstruction(MachineBlock *p, int cond)
{
    this->parent = p;
    this->type = MachineInstruction::VMRS;
    this->op = -1;
    this->cond = cond;
}

void VmrsMInstruction::output()
{
    // 跟在vcmp后边传flag
    fprintf(yyout, "\tvmrs APSR_nzcv, FPSCR\n");
}

MachineFunction::MachineFunction(MachineUnit *p, SymbolEntry *sym_ptr)
{
    this->parent = p;
    this->sym_ptr = sym_ptr;
    this->stack_size = 0;
    addSavedRegs(11);
    addSavedRegs(14);
};

void MachineBlock::insertBefore(MachineInstruction *insertee, MachineInstruction *pin)
{
    auto it = std::find(inst_list.begin(), inst_list.end(), pin);
    inst_list.insert(it, insertee);
}

void MachineBlock::insertAfter(MachineInstruction *insertee, MachineInstruction *pin)
{
    auto it = std::find(inst_list.begin(), inst_list.end(), pin);
    it++;
    inst_list.insert(it, insertee);
}

void MachineBlock::backPatch(std::vector<MachineOperand *> saved_regs)
{
    std::vector<MachineOperand *> rregs;   // 存储通用寄存器
    std::vector<MachineOperand *> fregs;   // 存储浮点寄存器

    // 将saved_regs中的寄存器按类型分别存入rregs和fregs
    for (auto reg : saved_regs)
    {
        if (reg->isFReg())
            fregs.push_back(reg);
        else
            rregs.push_back(reg);
    }

    // 遍历未确定指令列表unsure_insts
    for (auto inst : unsure_insts) {
        if (inst->isStack()) {   // 如果是栈操作指令
            if (inst->isVPOP()) {   // 如果是VPOP指令
                if (fregs.empty())   // 如果fregs为空，将这条指令删除
                    eraseInst(inst);
                else
                    dynamic_cast<StackMInstrcuton* >(inst)->setRegs(fregs);   // 设置该指令的寄存器为fregs
            }
            else if (inst->isPOP())   // 如果是POP指令
                dynamic_cast<StackMInstrcuton* >(inst)->setRegs(rregs);   // 设置该指令的寄存器为rregs
        }
        else if (inst->isLoad())   // 如果是Load指令
            dynamic_cast<LoadMInstruction* >(inst)->setOff(saved_regs.size() * 4);   // 设置该指令的偏移量为saved_regs.size() * 4
    }
}


void MachineBlock::output()
{   
    auto parent = this->getParent();
    fprintf(yyout, ".L%d:\n", this->no);   // 打印基本块的标签

    unsigned long long int inst_num = 0;   // 记录已输出的指令数量

    for (auto iter : inst_list) {
        iter->output();   // 输出指令
        inst_num++;

        if (iter->isRet()) // 如果当前指令是函数返回语句
        {
            fprintf(yyout, "\n.LTORG\n");   // 打印文字池的开始标记
            parent->getParent()->printGlobal();   // 输出全局变量
        }
        else if (inst_num >= 256)
        { // 每隔256条指令，打印一个文字池
            inst_num = 0;
            int ltorg_num = parent->getParent()->getLtorgNum();
            fprintf(yyout, "\tb .LT%d\n", ltorg_num);   // 跳转到文字池
            fprintf(yyout, "\n.LTORG\n");   // 打印文字池的开始标记
            parent->getParent()->printGlobal();   // 输出全局变量
            fprintf(yyout, ".LT%d:\n", ltorg_num);   // 打印文字池的标签
        }
    }
}


std::vector<MachineOperand *> MachineFunction::getAllSavedRegs()
{
    std::vector<MachineOperand *> saved_regs;
    for (auto reg_no : this->saved_regs) {
        if (reg_no < 15) // 通用寄存器
            saved_regs.push_back(new MachineOperand(MachineOperand::REG, reg_no));
        else if (reg_no > 15) // 专用寄存器
            saved_regs.push_back(new MachineOperand(MachineOperand::REG, reg_no, true));
    }
    return saved_regs;
}

std::vector<MachineOperand *> MachineFunction::getSavedRegs()
{
    std::vector<MachineOperand *> saved_regs;
    for (auto reg_no : this->saved_regs) {
        if (reg_no < 15)
            saved_regs.push_back(new MachineOperand(MachineOperand::REG, reg_no));
    }
    return saved_regs;
}

std::vector<MachineOperand *> MachineFunction::getSavedFRegs()
{
    std::vector<MachineOperand *> saved_regs;
    for (auto reg_no : this->saved_regs)
        if (reg_no > 15)
            saved_regs.push_back(new MachineOperand(MachineOperand::REG, reg_no, true));

    return saved_regs;
}

void MachineFunction::output()
{
    std::string funcName = this->sym_ptr->toStr() + "\0";  // 获取函数名，并在末尾添加空字符
    const char *func_name = funcName.c_str() + 1;         // 获取函数名的指针，跳过第一个字符（空字符）
    fprintf(yyout, "\t.global %s\n", func_name);          // 打印全局声明，声明函数名
    fprintf(yyout, "\t.type %s , %%function\n", func_name);   // 打印函数类型声明

    fprintf(yyout, "%s:\n", func_name);   // 打印函数标签

    auto entry = block_list[0];   // 获取函数的第一个基本块
    auto fp = new MachineOperand(MachineOperand::REG, 11);   // 创建一个指向fp寄存器的操作数对象
    auto sp = new MachineOperand(MachineOperand::REG, 13);   // 创建一个指向sp寄存器的操作数对象
    auto lr = new MachineOperand(MachineOperand::REG, 14);   // 创建一个指向lr寄存器的操作数对象

    std::vector<MachineOperand *> save_regs = getSavedRegs();   // 获取需要保存的通用寄存器列表
    std::vector<MachineOperand *> save_fregs = getSavedFRegs();   // 获取需要保存的浮点寄存器列表

    (new StackMInstrcuton(entry, StackMInstrcuton::PUSH, save_regs))->output();   // 输出保存通用寄存器的指令

    if (!save_fregs.empty()) {
        (new StackMInstrcuton(entry, StackMInstrcuton::VPUSH, save_fregs))->output();   // 输出保存浮点寄存器的指令
    }

    (new MovMInstruction(entry, MovMInstruction::MOV, fp, sp))->output();   // 输出将fp寄存器设置为sp寄存器的指令

    int stackSize = stack_size;   // 获取栈空间大小
    auto stSize = new MachineOperand(MachineOperand::IMM, stackSize);   // 创建一个表示栈空间大小的立即数操作数对象

    if (AsmBuilder::judge(stackSize)) {
        (new BinaryMInstruction(entry, BinaryMInstruction::SUB, sp, sp, stSize))->output();   // 输出分配栈空间的指令（当栈空间大小是2的幂时）
    }
    else {
        if (stackSize & 0xff) {
            (new BinaryMInstruction(entry, BinaryMInstruction::SUB, sp, sp, new MachineOperand(MachineOperand::IMM, stackSize & 0xff)))->output();   
            // 输出分配栈空间的指令（低8位）
        }
        if (stackSize & 0xff00) {
            (new BinaryMInstruction(entry, BinaryMInstruction::SUB, sp, sp, new MachineOperand(MachineOperand::IMM, stackSize & 0xff00)))->output();   
            // 输出分配栈空间的指令（次低8位）
        }
        if (stackSize & 0xff0000) {
            (new BinaryMInstruction(entry, BinaryMInstruction::SUB, sp, sp, new MachineOperand(MachineOperand::IMM, stackSize & 0xff0000)))->output();   
            // 输出分配栈空间的指令（次高8位）
        }
        if (stackSize & 0xff000000) {
            (new BinaryMInstruction(entry, BinaryMInstruction::SUB, sp, sp, new MachineOperand(MachineOperand::IMM, stackSize & 0xff000000)))->output();  
             // 输出分配栈空间的指令（高8位）
        }
    }

    fprintf(yyout, "\tlsr sp, sp, #3\n");   // 输出将sp寄存器右移3位的指令，以实现8字节对齐
    fprintf(yyout, "\tlsl sp, sp, #3\n");   // 输出将sp寄存器左移3位的指令，以实现8字节对齐

    for (auto iter : block_list) {
        iter->backPatch(getAllSavedRegs());   // 对每个基本块进行回填，将需要保存的寄存器的值保存到栈上
        iter->output();   // 输出基本块的指令
    }
}


void MachineUnit::PrintGlobalDecl()
{
    // 先把const的和不是const的给分开
    std::vector<SymbolEntry *> commonVar;  // 非 常量列表
    std::vector<SymbolEntry *> constVar;   //  常量列表
    for (auto se : global_vars) {  // 遍历全局变量列表
        if (se->getType()->isInt() && dynamic_cast<IntType* >(se->getType())->isConst() 
            || se->getType()->isFloat() && dynamic_cast<FloatType* >(se->getType())->isConst() 
            || se->getType()->isArray() && dynamic_cast<ArrayType* >(se->getType())->isConst())
            constVar.push_back(se);         // 常量放入 constVar 中
        else
            commonVar.push_back(se);        // 非常量放入 commonVar 中
    }

    // 不是const的放data区
    if (commonVar.size() > 0) {
        fprintf(yyout, ".data\n");          // 输出数据段标记
        for (auto se : commonVar) {          // 遍历非常量列表
            if (se->getType()->isArray()) { // 如果是数组类型
                if (dynamic_cast<IdentifierSymbolEntry* >(se)->isInitial()) // 如果数组有初始化值
                {
                    fprintf(yyout, ".global %s\n", se->toStr().c_str() + 1);       // 打印全局变量声明
                    fprintf(yyout, ".size %s, %d\n", se->toStr().c_str() + 1, se->getType()->getSize() / 8);  // 打印变量大小
                    fprintf(yyout, "%s:\n", se->toStr().c_str() + 1);              // 打印变量名
                    double *arrayValue = ((IdentifierSymbolEntry *)se)->getArrayValue();  // 获取数组元素的值
                    int length = se->getType()->getSize() / 32;                 // 计算数组长度
                    if (dynamic_cast<ArrayType* >(se->getType())->getBaseType()->isFloat()) {  // 如果数组元素是浮点数类型
                        for (int i = 0; i < length; i++) {
                            float value = (float)arrayValue[i];                // 将 double 类型的元素转换为 float 类型
                            uint32_t num = *((uint32_t *)&value);             // 转换为整数
                            fprintf(yyout, "\t.word %u\n", num);               // 打印
                        }
                    }
                    else {
                        for (int i = 0; i < length; i++)
                            fprintf(yyout, "\t.word %d\n", (int)arrayValue[i]);   // 数组元素是整数类型，直接输出
                    }
                }
                else
                    //.comm symbol, length:在bss段申请一段命名空间,该段空间的名称叫symbol, 长度为length.
                    // Ld 连接器在连接会为它留出空间.
                    fprintf(yyout, ".comm %s, %d\n", se->toStr().c_str() + 1, se->getType()->getSize() / 8);  // 数组无初始化值，则在 BSS 段中申请空间
            }
            else {
                fprintf(yyout, ".global %s\n", se->toStr().c_str() + 1);           // 打印全局变量声明
                fprintf(yyout, ".size %s, %d\n", se->toStr().c_str() + 1, se->getType()->getSize() / 8);  // 打印变量大小
                fprintf(yyout, "%s:\n", se->toStr().c_str() + 1);                  // 打印变量名
                if (se->getType()->isInt())
                    fprintf(yyout, "\t.word %d\n", (int)((IdentifierSymbolEntry *)se)->getValue());  // 如果是整数类型，则直接输出值
                if (se->getType()->isFloat()) {
                    float value = (float)((IdentifierSymbolEntry *)se)->getValue();     // 将 double 类型的值转换为 float 类型
                    uint32_t num = *((uint32_t *)&value);                              // 转换为整数
                    fprintf(yyout, "\t.word %u\n", num);                                // 打印
                }
            }
        }
    }

    // const的放只读区
    if (constVar.size() > 0)
    {
        fprintf(yyout, ".section .rodata\n");          // 输出只读区标记
        for (auto se : constVar) {                     // 遍历常量变量列表
            if (se->getType()->isArray()) {             // 如果是数组类型
                if (dynamic_cast<IdentifierSymbolEntry* >(se)->isInitial()) {  // 如果数组有初始化值
                    fprintf(yyout, ".global %s\n", se->toStr().c_str() + 1);   // 打印全局变量声明
                    fprintf(yyout, ".size %s, %d\n", se->toStr().c_str() + 1, se->getType()->getSize() / 8);  // 打印变量大小
                    fprintf(yyout, "%s:\n", se->toStr().c_str() + 1);          // 打印变量名
                    double *arrayValue = dynamic_cast<IdentifierSymbolEntry* >(se)->getArrayValue();  // 获取数组元素的值
                    int length = se->getType()->getSize() / 32;               // 计算数组长度
                    if (dynamic_cast<ArrayType* >(se->getType())->getBaseType()->isFloat()) {  // 如果数组元素是浮点数类型
                        for (int i = 0; i < length; i++) {
                            float value = (float)arrayValue[i];                // 将 double 类型的元素转换为 float 类型
                            uint32_t num = *((uint32_t *)&value);             // 转换为整数
                            fprintf(yyout, "\t.word %u\n", num);               // 打印
                        }
                    }
                    else
                        for (int i = 0; i < length; i++)
                            fprintf(yyout, "\t.word %d\n", (int)arrayValue[i]);   // 数组元素是整数类型，直接输出
                }
                else
                    //.comm symbol, length:在bss段申请一段命名空间,该段空间的名称叫symbol, 长度为length.
                    // Ld 连接器在连接会为它留出空间.
                    fprintf(yyout, ".comm %s, %d\n", se->toStr().c_str() + 1, se->getType()->getSize() / 8);  // 数组无初始化值，则在 BSS 段中申请空间
            }
            else {
                fprintf(yyout, ".global %s\n", se->toStr().c_str() + 1);           // 打印全局变量声明
                fprintf(yyout, ".size %s, %d\n", se->toStr().c_str() + 1, se->getType()->getSize() / 8);  // 打印变量大小
                fprintf(yyout, "%s:\n", se->toStr().c_str() + 1);                  // 打印变量名
                if (se->getType()->isInt())
                    fprintf(yyout, "\t.word %d\n", (int)((IdentifierSymbolEntry *)se)->getValue());  // 如果是整数类型，则直接输出值
                if (se->getType()->isFloat()) {
                    float value = (float)((IdentifierSymbolEntry *)se)->getValue();     // 将 double 类型的值转换为 float 类型
                    uint32_t num = *((uint32_t *)&value);                              // 转换为整数
                    fprintf(yyout, "\t.word %u\n", num);                                // 打印
                }
            }
        }
    }
}


void MachineUnit::printGlobal()
{
    // 打印文字池
    if (global_vars.size() > 0) {                        // 如果全局变量列表不为空
        for (auto se : global_vars) {                    // 遍历全局变量列表
            fprintf(yyout, "addr_%s_%d:\n", se->toStr().c_str() + 1, ltorg_num);   // 打印地址标签，格式为 "addr_变量名_序号"
            fprintf(yyout, "\t.word %s\n", se->toStr().c_str() + 1);               // 打印变量名
        }
    }
    ltorg_num++;                                        // 增加文字池序号
}


void MachineUnit::output()
{
    // TODO
    /* Hint:
     * 1. You need to print global variable/const declarition code;
     * 2. Traverse all the function in func_list to print assembly code;
     * 3. Don't forget print bridge label at the end of assembly code!! */
    fprintf(yyout, "\t.arch armv8-a\n");
    fprintf(yyout, "\t.arch_extension crc\n");
    fprintf(yyout, "\t.arm\n");
    PrintGlobalDecl();
    fprintf(yyout, "\n\t.text\n");
    for (auto iter : func_list)
        iter->output();
    fprintf(yyout, "\n.LTORG\n");//.LTORG: 用于标记文字池的开始
    printGlobal();//最后要输出全局变量标签信息
}
