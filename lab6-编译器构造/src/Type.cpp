#include "Type.h"
#include <assert.h>
#include <string>
#include <sstream>
#include <iostream>

IntType TypeSystem::commonInt = IntType(32);
IntType TypeSystem::commonBool = IntType(1);
FloatType TypeSystem::commonFloat = FloatType(32);
VoidType TypeSystem::commonVoid = VoidType();
IntType TypeSystem::commonConstInt = IntType(32, true);
FloatType TypeSystem::commonConstFloat = FloatType(32, true);

Type *TypeSystem::intType = &commonInt;
Type *TypeSystem::floatType = &commonFloat;
Type *TypeSystem::voidType = &commonVoid;
Type *TypeSystem::boolType = &commonBool;
Type *TypeSystem::constIntType = &commonConstInt;
Type *TypeSystem::constFloatType = &commonConstFloat;

std::string IntType::toStr()
{
    std::ostringstream buffer;
    buffer << "i" << size;
    return buffer.str();
}

std::string VoidType::toStr()
{
    return "void";
}

std::string FunctionType::toStr()
{
    std::ostringstream buffer;
    buffer << returnType->toStr() << "(";
    for (auto it = paramsType.begin(); it != paramsType.end(); it++)
    {
        buffer << (*it)->toStr();
        if (it + 1 != paramsType.end())
            buffer << ", ";
    }
    buffer << ")";
    return buffer.str();
}

std::string PointerType::toStr()
{
    std::ostringstream buffer;
    buffer << valueType->toStr() << "*";
    return buffer.str();
}

std::string ArrayType::toStr()
{
    std::ostringstream buffer;
    for (auto index : indexs)
    {
        buffer << "[" << index << " x ";
    }
    // 结束的时候，tmp应该是基本类型
    buffer << baseType->toStr();
    for (int i = 0; i < indexs.size(); i++)
    {
        buffer << "]";
    }
    return buffer.str();
}

std::string FloatType::toStr()
{
    std::ostringstream buffer;
    buffer << "float";
    return buffer.str();
}
