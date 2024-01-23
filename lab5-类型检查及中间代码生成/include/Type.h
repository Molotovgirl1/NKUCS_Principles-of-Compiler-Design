#ifndef __TYPE_H__
#define __TYPE_H__
#include <vector>
#include <string>

class Type
{
private:
    int kind;
    //++++++++++增加常量判断
    bool is_const;
protected:
    //++++++++++扩充了一些类型
    enum {INT, FLOAT, VOID, BOOL, FUNC, INT_ARRAY, FLOAT_ARRAY, CONST_INT_ARRAY, CONST_FLOAT_ARRAY, PTR};
public:
    Type(int kind, bool is_const = false) : kind(kind), is_const(is_const) {};
    virtual ~Type() {};
    virtual std::string toStr() = 0;
    bool isInt() const {return kind == INT;};
    bool isVoid() const {return kind == VOID;}
    bool isFunc() const {return kind == FUNC;}
    //++++++++++新增成员函数
    bool isFloat() const {return kind == FLOAT;};
    bool isBool() const {return kind == BOOL;}
    bool isIntArray() const {return kind == INT_ARRAY;}
    bool isFloatArray() const {return kind == FLOAT_ARRAY;}
    bool isConstIntArray() const {return kind == CONST_INT_ARRAY;}
    bool isConstFloatArray() const {return kind == CONST_FLOAT_ARRAY;}
    bool isArray() const {return kind == INT_ARRAY || kind == FLOAT_ARRAY || 
                            kind == CONST_FLOAT_ARRAY || kind == CONST_INT_ARRAY;}
    bool isAnyInt() const {return kind == INT || kind == INT_ARRAY || kind == CONST_INT_ARRAY;}
    bool isAnyFloat() const {return kind == FLOAT || kind == FLOAT_ARRAY || kind == CONST_FLOAT_ARRAY;}
    //是否可以参与计算
    bool calculatable() const {return isAnyInt()||isAnyFloat() || isBool();}//不是void其实就行
    bool isConst() const {return is_const || kind == CONST_INT_ARRAY || kind == CONST_FLOAT_ARRAY;}
};

class IntType : public Type
{
private:
    int size;
public:
    IntType(int size, bool is_const = false) : Type(Type::INT, is_const), size(size){};
    std::string toStr();
};

//++++++++++新增float型
class FloatType : public Type
{
private:
    int size;
public:
    FloatType(int size, bool is_const = false) : Type(Type::FLOAT, is_const), size(size){};
    std::string toStr();
};
//++++++++++新增bool型
class BoolType : public Type
{
private:
    int size;
public:
    BoolType(int size, bool is_const = false) : Type(Type::BOOL, is_const), size(size){};
    std::string toStr();
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
    std::vector<Type*> paramsType;
public:
    FunctionType(Type* returnType, std::vector<Type*> paramsType) : 
    Type(Type::FUNC), returnType(returnType), paramsType(paramsType){};
    void setparamsType(std::vector<Type*>);
    Type* getRetType() {return returnType;};
    //++++++++++新增获取参数类型
    std::vector<Type*> getParamsType() {return this->paramsType;}
    std::string toStr();
};
//++++++++++整型数组
class IntArrayType : public Type
{
private:
    std::vector<int> dimensions;
public:
    IntArrayType() : Type(Type::INT_ARRAY){};
    void pushBackDimension(int);
    std::vector<int> getDimensions();
    std::string toStr();
};
//++++++++++浮点型数组
class FloatArrayType : public Type
{
private:
    std::vector<int> dimensions;
public:
    FloatArrayType() : Type(Type::FLOAT_ARRAY){};
    void pushBackDimension(int);
    std::vector<int> getDimensions();
    std::string toStr();
};
//++++++++++常量整型数组
class ConstIntArrayType : public Type
{
private:
    std::vector<int> dimensions;
public:
    ConstIntArrayType() : Type(Type::CONST_INT_ARRAY){};
    void pushBackDimension(int);
    std::vector<int> getDimensions();
    std::string toStr();
};
////++++++++++常量浮点型数组
class ConstFloatArrayType : public Type
{
private:
    std::vector<int> dimensions;
public:
    ConstFloatArrayType() : Type(Type::CONST_FLOAT_ARRAY){};
    void pushBackDimension(int);
    std::vector<int> getDimensions();
    std::string toStr();
};

class PointerType : public Type
{
private:
    Type *valueType;
public:
    PointerType(Type* valueType) : Type(Type::PTR) {this->valueType = valueType;};
    //++++++++++获取指针对应值的类型
    Type* getValueType() {return this->valueType;};
    std::string toStr();
};

class TypeSystem
{
private:
    static IntType commonInt;
    static IntType commonConstInt;
    static FloatType commonFloat;
    static FloatType commonConstFloat;
    static BoolType commonBool;
    static BoolType commonConstBool;
    static VoidType commonVoid;
public:
    static Type *intType;
    static Type *constIntType;
    static Type *floatType;
    static Type *constFloatType;
    static Type *boolType;
    static Type *constBoolType;
    static Type *voidType;
    static Type* getMaxType(Type* type1, Type* type2);
    //++++++++++类型转换
    static bool needCast(Type* type1, Type* type2);
};


#endif
