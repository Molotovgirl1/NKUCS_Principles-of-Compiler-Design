#include "Unit.h"
#include "Type.h"
#include "AsmBuilder.h"
#include "MachineCode.h"
#include <sstream>
extern FILE *yyout;

void Unit::insertFunc(Function *f)
{
    func_list.push_back(f);
}

void Unit::removeFunc(Function *func)
{
    func_list.erase(std::find(func_list.begin(), func_list.end(), func));
}

void Unit::output() const
{
    // 先打印全局变量
    for (auto var : global_vars)
    {
        IdentifierSymbolEntry *se = (IdentifierSymbolEntry *)var;
        if (!se->getType()->isArray())
        {
            double value = se->getValue();
            if (se->getType()->isInt())
                fprintf(yyout, "%s = global %s %d, align 4\n", se->toStr().c_str(), se->getType()->toStr().c_str(), (int)value);
            else if (se->getType()->isFloat())
            {
                value = (float)value;
                uint64_t v = reinterpret_cast<uint64_t&>(value);
                fprintf(yyout, "%s = global %s 0x%lx, align 4\n", se->toStr().c_str(), se->getType()->toStr().c_str(), v);
                // fprintf(yyout, "%s = global %s %f, align 4\n", se->toStr().c_str(), se->getType()->toStr().c_str(), (float)value);
            }
        }
        else
        {
            fprintf(yyout, "%s = global %s zeroinitializer, align 4\n", se->toStr().c_str(), se->getType()->toStr().c_str());
        }
    }
    // 再打印函数
    for (auto &func : func_list)
        func->output();
    // 打印declare
    for (auto se : declare_list)
    {
        std::string type = ((FunctionType *)se->getType())->toStr();
        std::string name = se->toStr();
        std::string retType = type.substr(0, type.find("("));
        std::string fparams = type.substr(type.find("("));
        fprintf(yyout, "declare %s %s%s\n", retType.c_str(), name.c_str(), fparams.c_str());
    }
}

void Unit::genMachineCode(MachineUnit *munit)
{
    AsmBuilder *builder = new AsmBuilder();
    builder->setUnit(munit);
    munit->setGlobalVars(this->global_vars);
    for (auto &func : func_list)
        func->genMachineCode(builder);
}

Unit::~Unit()
{
    for (auto &func : func_list)
        delete func;
}
