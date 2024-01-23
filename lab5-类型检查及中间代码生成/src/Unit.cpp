#include "Unit.h"
// 程序单元
void Unit::insertFunc(Function *f)
{
    func_list.push_back(f);
}

void Unit::removeFunc(Function *func)
{
    func_list.erase(std::find(func_list.begin(), func_list.end(), func));
}

void Unit::insertDecl(IdentifierSymbolEntry* se)
{
    declare_func.insert(se);
}
//++++++++++ 输出unit中存储的所有声明和函数的信息
void Unit::output() const
{
    //++++++++++ 输出所有非库函数的声明
    for (auto decl : declare_func){
        if(!decl->isLibFunc())
            decl->outputFuncDecl();
    }
    //++++++++++ 输出所有库函数的声明
    for (auto decl : declare_func){
        if(decl->isLibFunc())
            decl->outputFuncDecl();
    }
    //++++++++++输出所有函数的信息
    for (auto &func : func_list){
        func->output();
    }
}

Unit::~Unit()
{
    auto delete_list = func_list;
    for(auto &func:delete_list)
        delete func;
}
