#ifndef __MACHINECODE_H__
#define __MACHINECODE_H__
#include <vector>
#include <set>
#include <string>
#include <algorithm>
#include <fstream>
#include "SymbolTable.h"

/* Hint:
 * MachineUnit: Compiler unit
 * MachineFunction: Function in assembly code
 * MachineInstruction: Single assembly instruction
 * MachineOperand: Operand in assembly instruction, such as immediate number, register, address label */

/* Todo:
 * We only give the example code of "class BinaryMInstruction" and "class AccessMInstruction" (because we believe in you !!!),
 * You need to complete other the member function, especially "output()" ,
 * After that, you can use "output()" to print assembly code . */

class MachineUnit;
class MachineFunction;
class MachineBlock;
class MachineInstruction;

class MachineOperand
{
private:
    MachineInstruction *parent;
    int type;
    int val;           // value of immediate number
    int reg_no;        // register no
    std::string label; // address label
    bool fpu = false;  // 这个变量用来表示是不是浮点寄存器，s那一系列
                       // 如果位true，则用s
public:
    enum
    {
        IMM,
        VREG,
        REG,
        LABEL
    };
    MachineOperand(int tp, int val, bool fpu = false);
    MachineOperand(std::string label);
    bool operator==(const MachineOperand &) const;
    bool operator<(const MachineOperand &) const;
    bool isImm() { return this->type == IMM; };
    bool isReg() { return this->type == REG; };
    bool isVReg() { return this->type == VREG; };
    bool isLabel() { return this->type == LABEL; };
    bool isFReg() { return this->fpu; }
    int getVal() { return this->val; };
    int getReg() { return this->reg_no; };
    void setReg(int regno)
    {
        this->type = REG;
        this->reg_no = regno;
    };
    std::string getLabel() { return this->label; };
    void setParent(MachineInstruction *p) { this->parent = p; };
    MachineInstruction *getParent() { return this->parent; };
    void PrintReg();
    void output();
};

class MachineInstruction
{
protected:
    MachineBlock *parent;
    int no;
    int type;                            // Instruction type
    int cond = MachineInstruction::NONE; // 条件码Instruction execution condition, optional !!
    int op;                              // 操作码Instruction opcode
    // Instruction operand list, sorted by appearance order in assembly instruction
    std::vector<MachineOperand *> def_list;
    std::vector<MachineOperand *> use_list;
    void addDef(MachineOperand *ope) { def_list.push_back(ope); };
    void addUse(MachineOperand *ope) { use_list.push_back(ope); };
    // Print execution code after printing opcode
    void PrintCond();
    enum instType//指令类型
    {
        BINARY,
        LOAD,
        STORE,
        MOV,
        BRANCH,
        CMP,
        STACK,
        VCVT,
        VMRS
    };
    /*
        VCVT：用于进行向量数据类型之间的转换。VCVT指令用于在不同的向量数据类型之间进行转换。
        VMRS：用于将向量寄存器中的数据转移到标量寄存器中。VMRS指令将向量寄存器中的数据转移到标量寄存器中，以便进行标量操作。
    */

public:
    enum condType//条件指令
    {
        EQ,
        NE,
        LT,
        LE,
        GT,
        GE,
        NONE
    };
    virtual void output() = 0;
    void setNo(int no) { this->no = no; };
    int getNo() { return no; };
    std::vector<MachineOperand *> &getDef() { return def_list; };
    std::vector<MachineOperand *> &getUse() { return use_list; };
    MachineBlock *getParent() { return parent; };
    bool isStack() { return type == STACK; };
    // 确认了op类型与操作码
    bool isPOP() { return type == STACK && this->op == 1; }
    bool isVPOP() { return type == STACK && this->op == 3; }
    bool isLoad() { return type == LOAD; };
    bool isBinary() { return type == BINARY; };
    bool isRet() { return type == BRANCH && op == 2; };
    bool isUncondBranch() { return type == BRANCH && (op == 2 || (op == 0 && cond == MachineInstruction::NONE)); };
};

class BinaryMInstruction : public MachineInstruction
{
public:
    enum opType
    {
        ADD,
        SUB,
        MUL,
        DIV,
        AND,
        OR,
        VADD, // 向量加减乘除
        VSUB,
        VMUL,
        VDIV
    };
    BinaryMInstruction(MachineBlock *p, int op,
                       MachineOperand *dst, MachineOperand *src1, MachineOperand *src2,
                       int cond = MachineInstruction::NONE);
    void setStackSize(int stack_size) {
        use_list[1] = new MachineOperand(MachineOperand::IMM, stack_size);
    }
    void output();
};

class LoadMInstruction : public MachineInstruction
{
public:
    enum opType { LDR, VLDR };
    LoadMInstruction(MachineBlock *p, int op,
                     MachineOperand *dst, MachineOperand *src1, MachineOperand *src2 = nullptr,
                     int cond = MachineInstruction::NONE);
    void setOff(int offset) {
        use_list[1] = new MachineOperand(MachineOperand::IMM, use_list[1]->getVal() + offset);
    }
    void output();
};

class StoreMInstruction : public MachineInstruction
{
public:
    enum opType { STR, VSTR };
    StoreMInstruction(MachineBlock *p, int op,
                      MachineOperand *src1, MachineOperand *src2, MachineOperand *src3 = nullptr,
                      int cond = MachineInstruction::NONE);
    void output();
};

class MovMInstruction : public MachineInstruction
{
public:
    enum opType
    {
        MOV,
        MVN,  //进行位求反操作
        VMOV, // 向量寄存器之间 这个指令用于r放到s里
        VMOV32//32位向量寄存器之间
    };
    MovMInstruction(MachineBlock *p, int op,
                    MachineOperand *dst, MachineOperand *src,
                    int cond = MachineInstruction::NONE);
    void output();
};

class BranchMInstruction : public MachineInstruction
{
public:
    enum opType { B, BL, BX, BEQ, BNE, BGE, BLE, BGT, BLT };
    BranchMInstruction(MachineBlock *p, int op,
                       MachineOperand *dst,
                       int cond = MachineInstruction::NONE);
    void output();
};

class CmpMInstruction : public MachineInstruction
{
public:
    enum opType { CMP, VCMP };
    CmpMInstruction(MachineBlock *p, int op,
                    MachineOperand *src1, MachineOperand *src2,
                    int cond = MachineInstruction::NONE);
    void output();
};

//栈操作相关指令
class StackMInstrcuton : public MachineInstruction
{
public:
    enum opType { PUSH, POP, VPUSH, VPOP };
    StackMInstrcuton(MachineBlock *p, int op,
                     std::vector<MachineOperand *> srcs,
                     int cond = MachineInstruction::NONE);
    void setRegs(std::vector<MachineOperand *> regs) {
        use_list.assign(regs.begin(), regs.end());
    }
    void output();
};

//数据类型转换相关
class VcvtMInstruction : public MachineInstruction
{
public:
    enum opType { FTS, STF };
    VcvtMInstruction(MachineBlock *p, int op,
                     MachineOperand *dst, MachineOperand *src,
                     int cond = MachineInstruction::NONE);
    void output();
};

//读取程序状态寄存器的机器指令
class VmrsMInstruction : public MachineInstruction
{
public:
    VmrsMInstruction(MachineBlock *p,
                     int cond = MachineInstruction::NONE);
    void output();
};

//零扩展字节的机器指令
class UxtbMInstruction : public MachineInstruction
{
public:
    UxtbMInstruction(MachineBlock* p, MachineOperand* src);
    void output();

};

class MachineBlock
{
private:
    MachineFunction *parent;
    int no;
    std::vector<MachineBlock *> pred, succ;
    std::vector<MachineInstruction *> inst_list;
    std::vector<MachineInstruction *> unsure_insts;
    std::set<MachineOperand *> live_in;
    std::set<MachineOperand *> live_out;
    int cmpCond;

public:
    std::vector<MachineInstruction *> &getInsts() { return inst_list; };
    std::vector<MachineInstruction *>::iterator begin() { return inst_list.begin(); };
    std::vector<MachineInstruction *>::iterator end() { return inst_list.end(); };
    std::vector<MachineInstruction *>::reverse_iterator rbegin() { return inst_list.rbegin(); };
    std::vector<MachineInstruction *>::reverse_iterator rend() { return inst_list.rend(); };
    MachineBlock(MachineFunction *p, int no)
    {
        this->parent = p;
        this->no = no;
    };
    void InsertInst(MachineInstruction *inst) { this->inst_list.push_back(inst); };
    void addPred(MachineBlock *p) { this->pred.push_back(p); };
    void addSucc(MachineBlock *s) { this->succ.push_back(s); };
    void insertBefore(MachineInstruction *, MachineInstruction *);
    void insertAfter(MachineInstruction *, MachineInstruction *);
    void insertFront(MachineInstruction *inst) { this->inst_list.insert(inst_list.begin(), inst); };
    void backPatch(std::vector<MachineOperand *>);
    void addUInst(MachineInstruction *inst) { unsure_insts.push_back(inst); };
    void eraseInst(MachineInstruction *inst)
    {
        this->inst_list.erase(find(inst_list.begin(), inst_list.end(), inst));
    };
    std::set<MachineOperand *> &getLiveIn() { return live_in; };
    std::set<MachineOperand *> &getLiveOut() { return live_out; };
    std::vector<MachineBlock *> &getPreds() { return pred; };
    std::vector<MachineBlock *> &getSuccs() { return succ; };
    MachineFunction *getParent() { return parent; };
    int getCmpCond() const { return cmpCond; };
    void setCmpCond(int cond) { cmpCond = cond; };
    void output();
};

class MachineFunction
{
private:
    MachineUnit *parent;
    std::vector<MachineBlock *> block_list;
    int stack_size;
    std::set<int> saved_regs;
    SymbolEntry *sym_ptr;
    bool isleaf = true;

public:
    std::vector<MachineBlock *> &getBlocks() { return block_list; };
    std::vector<MachineBlock *>::iterator begin() { return block_list.begin(); };
    std::vector<MachineBlock *>::iterator end() { return block_list.end(); };
    MachineFunction(MachineUnit *p, SymbolEntry *sym_ptr);
    /* HINT:
     * Alloc stack space for local variable;
     * return current frame offset ;
     * we store offset in symbol entry of this variable in function AllocInstruction::genMachineCode()
     * you can use this function in LinearScan::genSpillCode() */
    int AllocSpace(int size)
    {
        this->stack_size += size;
        return this->stack_size;
    };
    void InsertBlock(MachineBlock *block) { this->block_list.push_back(block); };
    void addSavedRegs(int regno) { saved_regs.insert(regno); };
    std::vector<MachineOperand *> getAllSavedRegs();
    std::vector<MachineOperand *> getSavedRegs();
    std::vector<MachineOperand *> getSavedFRegs();
    MachineUnit *getParent() { return parent; };
    bool Isleaf() { return isleaf; };
    void setUnleaf() { isleaf = false; };
    void output();
};

class MachineUnit
{
private:
    std::vector<MachineFunction *> func_list;
    std::vector<SymbolEntry *> global_vars;
    int ltorg_num = 0;
    void PrintGlobalDecl();

public:
    std::vector<MachineFunction *> &getFuncs() { return func_list; };
    std::vector<MachineFunction *>::iterator begin() { return func_list.begin(); };
    std::vector<MachineFunction *>::iterator end() { return func_list.end(); };
    void InsertFunc(MachineFunction *func) { func_list.push_back(func); };
    void setGlobalVars(std::vector<SymbolEntry *> gv) { global_vars = gv; };
    void printGlobal(); // 打印文字池
    int getLtorgNum() { return ltorg_num; };
    void output();
};

#endif