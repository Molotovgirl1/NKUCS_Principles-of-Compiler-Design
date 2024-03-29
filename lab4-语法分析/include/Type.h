#ifndef __TYPE_H__
#define __TYPE_H__
#include <vector>
#include <string>

class Type
{
private:
    int kind;

protected:
    enum
    {
        INT,
        VOID,
        FUNC,
        STRING
    };

public:
    Type(int kind) : kind(kind){};
    virtual ~Type(){};
    virtual std::string toStr() = 0;
    bool isInt() const { return kind == INT; };
    bool isVoid() const { return kind == VOID; };
    bool isFunc() const { return kind == FUNC; };
    bool isString() const { return kind == STRING; };
};

class IntType : public Type
{
private:
    int size;
    bool constant;

public:
    IntType(int size, bool constant = false)
        : Type(Type::INT), size(size), constant(constant){};
    std::string toStr();
    bool isConstant() { return constant; }
};

class VoidType : public Type
{
public:
    VoidType() : Type(Type::VOID){};
    std::string toStr();
};

class FunctionType : public Type
{
private:
    Type *returnType;
    std::vector<Type *> paramsType;

public:
    FunctionType(Type *returnType, std::vector<Type *> paramsType) : Type(Type::FUNC), returnType(returnType), paramsType(paramsType){};
    std::string toStr();
};


class StringType : public Type
{
private:
    int length;

public:
    StringType(int length) : Type(Type::STRING), length(length){};
    int getLength() const { return length; };
    std::string toStr();
};

class TypeSystem
{
private:
    static IntType commonInt;
    static VoidType commonVoid;
    static IntType commonConstInt;

public:
    static Type *intType;
    static Type *voidType;
    static Type *constIntType;
};

#endif
