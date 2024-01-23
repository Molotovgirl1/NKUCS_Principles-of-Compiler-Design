#ifndef __UNIT_H__
#define __UNIT_H__
// 程序编译单元数据结构
#include <vector>
// ++++++++++集合头文件
#include <set>
#include "Function.h"

class Unit
{
    typedef std::vector<Function *>::iterator iterator;
    typedef std::vector<Function *>::reverse_iterator reverse_iterator;

private:
    //所有函数定义
    std::vector<Function *> func_list;
    // ++++++++++库函数声明和全局变量声明
    std::set<IdentifierSymbolEntry*> declare_func;
public:
    Unit() = default;
    ~Unit() ;
    // 添加、移除函数
    void insertFunc(Function *);
    void removeFunc(Function *);
    //++++++++++ 向declare_func中添加一个声明
    void insertDecl(IdentifierSymbolEntry*);
    void output() const;
    // 迭代器
    iterator begin() { return func_list.begin(); };
    iterator end() { return func_list.end(); };
    // 反向迭代器
    reverse_iterator rbegin() { return func_list.rbegin(); };
    reverse_iterator rend() { return func_list.rend(); };
};

#endif