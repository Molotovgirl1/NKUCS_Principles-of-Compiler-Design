#ifndef __ASMBUILDER_H__
#define __ASMBUILDER_H__

#include "MachineCode.h"

/*AsmBuilder.h 中为汇编代码构造辅助类。其主要作用就是在中间代码向目标代码进行自顶向下
的转换时，记录当前正在进行转换操作的对象，以进行函数、基本块及指令的插入。*/
class AsmBuilder
{
private:
    MachineUnit *mUnit;         // mahicne unit
    MachineFunction *mFunction; // current machine code function;
    MachineBlock *mBlock;       // current machine code block;
    int cond;              // CmpInstruction opcode, for CondInstruction;
public:
    void setUnit(MachineUnit *unit) { this->mUnit = unit; };
    void setFunction(MachineFunction *func) { this->mFunction = func; };
    void setBlock(MachineBlock *block) { this->mBlock = block; };
    void setCond(int opcode) { this->cond = opcode; };
    MachineUnit *getUnit() { return this->mUnit; };
    MachineFunction *getFunction() { return this->mFunction; };
    MachineBlock *getBlock() { return this->mBlock; };
    int getCond() { return this->cond; };
    static unsigned int rightmove(unsigned int num,int bit){
        return num=(num<<(32-bit)|(num>>bit));
    }
    static bool judge(int num){
         /*
         +++++++++++++非法性判断
         1.如十六进制数在0x00到0xFF之间，则它一定是合法的

         2.若十六进制数>0XFF则将十六进制的数转化成十进制

         3.如果这个十进制数能够被4整除则这个十六进制的立即数和合法的，否则是非法的
         如果一个立即数小于 0xFF（255）那么直接用前 7～0 位表示即可，此时不用移位，11～8 位的 Rotate_imm 等于 0。
        如果前八位 immed_8 的数值大于 255，那么就看这个数是否能有 immed_8 中的某个数移位 2*Rotate_imm 位形成的。
            如果能，那么就是合法立即数；否则非法。

         */
        num = (unsigned int)num;
        unsigned int num_temp;
        int i;
        for(i=0;i<32;i++){
            num_temp=rightmove(num,i);
            if(num_temp<=0x00ff&&i%2==0){
                return true;
            }
        }
        return false;
    }
    
};

#endif