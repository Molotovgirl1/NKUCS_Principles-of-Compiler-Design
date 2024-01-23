/**
 * linear scan register allocation
 */

#ifndef _LINEARSCAN_H__
#define _LINEARSCAN_H__
#include <set>
#include <map>
#include <vector>
#include <list>

class MachineUnit;
class MachineOperand;
class MachineFunction;

// 线性扫描寄存器分配算法相关类，为虚拟寄存器分配物理寄存器
class LinearScan
{
private:
    //目前还未分配寄存器的活跃区间
    struct Interval
    {
        int start;//开始位置
        int end;//结束位置
        bool spill; // whether this vreg should be spilled to memory
        int disp;   // displacement in stack栈偏移
        int rreg;   // the real register mapped from virtual register if the vreg is not spilled to memory
                    // 物理寄存器号
        std::set<MachineOperand *> defs;
        std::set<MachineOperand *> uses;
        bool fpu; // 是不是浮点寄存器
    };
    MachineUnit *unit;//从unit自顶向下
    MachineFunction *func;//遍历函数
    std::vector<Interval *> spillIntervals; // 加这个变量也是希望能够快一点，不过最后好像差不多
    std::vector<int> regs; // 这个存通用寄存器
    std::vector<int> fpuRegs; // 这个存special寄存器
    std::map<MachineOperand *, std::set<MachineOperand *>> du_chains;
    std::vector<Interval *> intervals;// 未分配寄存器的活跃区间
    std::vector<Interval *> active;// 新增 占用物理寄存器的活跃区间
    static bool compareStart(Interval *a, Interval *b);
    static bool compareEnd(Interval *a, Interval *b);
    void expireOldIntervals(Interval *interval);
    void spillAtInterval(Interval *interval);
    void makeDuChains();
    void computeLiveIntervals();
    bool linearScanRegisterAllocation();
    void modifyCode();
    void genSpillCode();

public:
    LinearScan(MachineUnit *unit);
    void allocateRegisters();
};

#endif