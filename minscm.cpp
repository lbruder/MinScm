// vim:et

// MinScm.cpp version 2013-02-21
// An experimental Scheme subset interpreter in C++, based on SchemeNet.cs
// Features: Tail calls, CL style macros, part of SRFI-1
// Copyright (c) 2013, Leif Bruder <leifbruder@gmail.com>
//
// TODO: No reference counting or GC yet. Leaking memory like hell!
// TODO: string/number conversion: Not possible for base 2 yet
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
// 
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

//----------------------------------------------------------------------------------------------------------------------

// VM instructions:

//private void PerformArgsToValue() { valueRegister = argumentsRegister; programCounter++; }
//private void PerformBranchLabel(int target) { programCounter = IsTrue(valueRegister) ? target : programCounter + 1; }
//private void PerformContinue() { programCounter = continueRegister; }
//private void PerformDefineVariable(Symbol variable) { environmentRegister.Define(variable, valueRegister); programCounter++; }
//private void PerformGetVariable(Symbol variable) { valueRegister = environmentRegister.Get(variable); programCounter++; }
//private void PerformGotoLabel(int target) { programCounter = target; }
//private void PerformInitArgs() { argumentsRegister = null; programCounter++; }
//private void PerformLdConst(object value) { valueRegister = value; programCounter++; }
//private void PerformLdCont(int target) { continueRegister = target; programCounter++; }
//private void PerformPushParam() { argumentsRegister = new Pair(valueRegister, argumentsRegister); programCounter++; }
//private void PerformRestoreRegisters() { environmentRegister = (Environment)stack.Pop(); continueRegister = (int)stack.Pop(); argumentsRegister = stack.Pop(); programCounter++; }
//private void PerformSaveRegisters() { stack.Push(argumentsRegister); stack.Push(continueRegister); stack.Push(environmentRegister); programCounter++; }
//private void PerformSetVariable(Symbol variable) { environmentRegister.Set(variable, valueRegister); programCounter++; }
//private void PerformMakeClosure(string name, int target, bool hasRestParameter, Symbol[] parameterNames) { valueRegister = new Closure(name, environmentRegister, target, parameterNames, hasRestParameter); programCounter++; }
//private void PerformValueToArgs() { argumentsRegister = valueRegister; programCounter++; }

//private void PerformCall()
//{
//    object[] args;
//    if (argumentsRegister == null) args = new object[0];
//    else if (argumentsRegister is Pair) args = ((Pair)argumentsRegister).ToArray();
//    else throw new VirtualMachineException("Invalid function application: Expected list of arguments, got " + argumentsRegister.GetType());

//    argumentsRegister = null;

//    if (valueRegister is Func<object[], object>)
//    {
//        valueRegister = ((Func<object[], object>)valueRegister)(args);
//        programCounter = continueRegister;
//        return;
//    }

//    if (valueRegister is Closure)
//    {
//        var closure = (Closure)valueRegister;
//        var env = new Environment((closure).Captured);

//        if (closure.HasRestParameter)
//        {
//            if (closure.ParameterNames.Length - 1 > args.Length)
//                throw new VirtualMachineException("Invalid parameter count in call to '" + closure.Name + "': Expected " + (closure.ParameterNames.Length - 1) + " or more, got " + args.Length);
//            for (int i = 0; i < closure.ParameterNames.Length - 1; ++i) env.Define(closure.ParameterNames[i], args[i]);
//            env.Define(closure.ParameterNames.Last(), Pair.FromEnumerable(args.Skip(closure.ParameterNames.Length - 1)));
//        }
//        else
//        {
//            if (closure.ParameterNames.Length != args.Length)
//                throw new VirtualMachineException("Invalid parameter count in call to '" + closure.Name + "': Expected " + closure.ParameterNames.Length + ", got " + args.Length);
//            for (int i = 0; i < closure.ParameterNames.Length; ++i) env.Define(closure.ParameterNames[i], args[i]);
//        }

//        environmentRegister = env;
//        programCounter = closure.PC;
//        return;
//    }

//    throw new VirtualMachineException("Invalid CALL target");
//}


//----------------------------------------------------------------------------------------------------------------------

#define error(msg) throw msg

enum ObjectType { otFixnum, otFlonum, otSymbol, otPair, otString, otBoolean, otChar, otNull, otProcedure, otVector, otEof, otEnvironment };

class Object
{
public:
    virtual ~Object() { }
    virtual ObjectType getType() const = 0;
    virtual string toString() const = 0;
};

void assertType(const char *procedure, const Object *o, ObjectType expectedType)
{
    if (o->getType() != expectedType)
        error((string)procedure + ": Invalid argument type");
}

//----------------------------------------------------------------------------------------------------------------------

class Pair: public Object
{
public:
    Object *_car;
    Object *_cdr;
    Pair(Object *car, Object *cdr): _car(car), _cdr(cdr) { }
    ObjectType getType() const { return otPair; }
    
    string toString() const
    {
        stringstream sb;
        sb << '(';
        Object *i=(Object*) this;
        while (i->getType() == otPair)
        {
            sb << ((Pair*)i)->_car->toString() << " ";
            i = ((Pair*)i)->_cdr;
        }
        if (i->getType() == otNull)
        {
            string ret = sb.str();
            ret[ret.length()-1] = ')';
            return ret;
        }
        else
        {
            sb << ". " << i->toString() << ")";
            return sb.str();
        }
    }
    
    bool isDottedList()
    {
        Object *i = this;
        while (i->getType() == otPair) i = ((Pair*)i)->_cdr;
        return i->getType() != otNull;
    }
};

//----------------------------------------------------------------------------------------------------------------------

class Null: public Object
{
public:
    ObjectType getType() const { return otNull; }
    string toString() const { return "()"; }
    static const Null *getInstance() { return _instance; }

private:
    Null() { }
    static const Null *_instance;
};

const Null *Null::_instance = new Null();

//----------------------------------------------------------------------------------------------------------------------

class Environment: public Object
{
public:
    Environment(): _outer(NULL) { }
    Environment(Environment *outer): _outer(outer) { }
    ObjectType getType() const { return otEnvironment; }
    string toString() const { return "<Environment>"; }

    void define(const string& identifier, Object *value)
    {
        if (identifier == "if" || identifier == "define" ||identifier == "defmacro" ||identifier == "set!" || identifier == "lambda" ||identifier == "quote" ||identifier == "begin")
            error("Symbol '" + identifier + "' is constant and must not be changed");
        else
            _data[identifier] = value;
    }

    void set(const string& identifier, Object *value)
    {
        if (_data.count(identifier)) _data[identifier] = value;
        else if (_outer != NULL) _outer->set(identifier, value);
        else error("Unknown variable '" + identifier + "'");
    }

    Object* get(const string& identifier)
    {
        if (_data.count(identifier)) return _data[identifier];
        else if (_outer != NULL) return _outer->get(identifier);
        else error("Unknown variable '" + identifier + "'");
        return NULL; // Just to keep the compiler happy
    }

    Environment* extendIntoNew(const vector<string> *argumentNames, const vector<Object*> *arguments, bool hasRestParameter)
    {
        Environment *ret = new Environment(this);

        if (hasRestParameter)
        {
            if (arguments->size() < argumentNames->size() - 1) error("Invalid parameter count");
            for (size_t i = 0; i < argumentNames->size() - 1; ++i) ret->define(argumentNames->at(i), arguments->at(i));
            Object *o = (Object*) Null::getInstance();
            for (long i = arguments->size()-1; i >= (long)argumentNames->size() - 1; --i) o = new Pair(arguments->at(i), o);
            ret->define(argumentNames->back(), o);
        }
        else
        {
            if (arguments->size() != argumentNames->size()) error("Invalid parameter count");
            for (size_t i = 0; i < argumentNames->size(); ++i) ret->define(argumentNames->at(i), arguments->at(i));
        }
        return ret;
    }

private:
    map<string, Object*> _data;
    Environment *_outer;
};

//----------------------------------------------------------------------------------------------------------------------

class Fixnum: public Object
{
public:
    Fixnum(long value): _value(value) { }
    long getValue() const { return _value; }
    ObjectType getType() const { return otFixnum; }
    
    string toString() const
    {
        stringstream ss;
        ss << _value;
        return ss.str();
    }

private:
    const long _value;
};

//----------------------------------------------------------------------------------------------------------------------

class Flonum: public Object
{
public:
    Flonum(double value): _value(value) { }
    double getValue() const { return _value; }
    ObjectType getType() const { return otFlonum; }
    
    string toString() const
    {
        stringstream ss;
        ss << _value;
        return ss.str();
    }

private:
    const double _value;
};

//----------------------------------------------------------------------------------------------------------------------

class Symbol: public Object
{
public:
    ObjectType getType() const { return otSymbol; }
    string toString() const { return _value; }
    string getName() const { return _value; }
    
    static Symbol *fromString(const string& value)
    {
        if (!cache.count(value)) cache[value] = new Symbol(value);
        return cache[value];
    }

private:
    const string _value;
    static map<string, Symbol*> cache;
    Symbol(const string& value): _value(value) { }
};

map<string, Symbol*> Symbol::cache;

//----------------------------------------------------------------------------------------------------------------------

class String: public Object
{
public:
    String(const vector<int>& value): _value(value) { }
    String(const long size) { _value.resize(size, ' '); }
    ObjectType getType() const { return otString; }
    string toString() const { return getValue(); }
    long getLength() const { return _value.size(); }
    int GetAt(int index) const { return _value[index]; }
    void SetAt(int index, int newChar) { _value[index] = newChar; }

    string getValue() const
    {
        stringstream sb;
        for (size_t i=0; i<_value.size(); ++i) sb << (char)_value[i];
        return sb.str();
    }

private:
    vector<int> _value;
};

//----------------------------------------------------------------------------------------------------------------------

class Boolean: public Object
{
public:
    bool getValue() const { return _value; }
    ObjectType getType() const { return otBoolean; }
    string toString() const { return _value ? "#t" : "#f"; }
    static const Boolean *getTrue() { return _true; }
    static const Boolean *getFalse() { return _false; }
    static const Boolean *valueOf(bool value) { return value ? _true : _false; }

private:
    Boolean(bool value): _value(value) { }
    const bool _value;
    static const Boolean *_true;
    static const Boolean *_false;
};

const Boolean *Boolean::_true = new Boolean(true);
const Boolean *Boolean::_false = new Boolean(false);

//----------------------------------------------------------------------------------------------------------------------

class Char: public Object
{
public:
    Char(int value): _value(value) { }
    int getValue() const { return _value; }
    ObjectType getType() const { return otChar; }
    
    string toString() const
    {
        stringstream sb;
        sb << (char)_value;
        return sb.str();
    }

private:
    const int _value;
};

//----------------------------------------------------------------------------------------------------------------------

class Eof: public Object
{
public:
    ObjectType getType() const { return otEof; }
    string toString() const { return "<EOF>"; }
    static const Eof *getInstance() { return _instance; }

private:
    Eof() { }
    static const Eof *_instance;
};

const Eof *Eof::_instance = new Eof();

//----------------------------------------------------------------------------------------------------------------------

class Procedure: public Object
{
public:
    Procedure(const string& name): _name(name) { }
    virtual ~Procedure() { }
    string getName() const { return _name; }
    ObjectType getType() const { return otProcedure; }
    string toString() const { return "<procedure " + _name + ">"; }
    virtual Object* call(const vector<Object*> *parameters) = 0;
    virtual bool isBuiltin() const { return true; }
    virtual bool hasRestParameter() const { return false; }
    virtual Object* getBody() const { return Symbol::fromString("builtin"); }
    virtual const vector<string>* getArgumentNames() const { return new vector<string>(); }

protected:
    void assertParameterCount(int expected, int got) const
    {
        if (expected != got)
        {
            stringstream sb;
            sb << _name << ": Invalid parameter count. Expected: " << expected << ", got " << got;
            error(sb.str());
        }
    }

private:
    const string _name;
};

class UnaryProcedure: public Procedure
{
public:
    UnaryProcedure(const string& name, Object *(*f)(Object*)): Procedure(name), _f(f) { }
    Object* call(const vector<Object*> *parameters)
    {
        assertParameterCount(1, parameters->size());
        return _f(parameters->at(0));
    }

private:
    Object *(*_f)(Object*);
};

class BinaryProcedure: public Procedure
{
public:
    BinaryProcedure(const string& name, Object *(*f)(Object*, Object*)): Procedure(name), _f(f) { }
    Object* call(const vector<Object*> *parameters)
    {
        assertParameterCount(2, parameters->size());
        return _f(parameters->at(0), parameters->at(1));
    }

private:
    Object *(*_f)(Object*, Object*);
};

class TrinaryProcedure: public Procedure
{
public:
    TrinaryProcedure(const string& name, Object *(*f)(Object*, Object*, Object*)): Procedure(name), _f(f) { }
    Object* call(const vector<Object*> *parameters)
    {
        assertParameterCount(3, parameters->size());
        return _f(parameters->at(0), parameters->at(1), parameters->at(2));
    }

private:
    Object *(*_f)(Object*, Object*, Object*);
};

class Lambda: public Procedure
{
public:
    Lambda(const string& name, Object *body, Environment *env, vector<string> argumentNames, bool hasRestParameter):
        Procedure(name),
        _body(new Pair(Symbol::fromString("begin"), body)),
        _env(env),
        _argumentNames(argumentNames),
        _hasRest(hasRestParameter)
    {
    }

    Object* call(const vector<Object*> *parameters)
    {
        error("Internal error: Lambda must be called by executing the body in tail position");
        return NULL; // Just to keep the compiler happy
    }

    virtual bool isBuiltin() const { return false; }
    virtual bool hasRestParameter() const { return _hasRest; }
    virtual Object* getBody() const { return _body; }
    virtual const vector<string>* getArgumentNames() const { return &_argumentNames; }
    Environment *getCapturedEnvironment() const { return _env; }

private:
    Object *_body;
    Environment *_env;
    vector<string> _argumentNames;
    bool _hasRest;
};


//----------------------------------------------------------------------------------------------------------------------

class Vector: public Object
{
public:
    Vector(vector<Object*> value): _value(value) { }
    Vector(const long size) { _value.resize(size, Symbol::fromString("undefined")); }
    ObjectType getType() const { return otVector; }
    long getLength() const { return _value.size(); }
    Object* GetAt(int index) const { return _value[index]; }
    void SetAt(int index, Object* newValue) { _value[index] = newValue; }
    
    string toString() const
    {
        stringstream sb;
        sb << "#(";
        for (size_t i=0; i<_value.size(); ++i) sb << _value[i]->toString() << " ";
        string ret = sb.str();
        ret[ret.length()-1] = ')';
        return ret;
    }

private:
    vector<Object*> _value;
};

//----------------------------------------------------------------------------------------------------------------------

class Reader
{
public:
    Reader(istream *input):
        _input(input),
        dot((Object*) Symbol::fromString(".")),
        listEnd((Object*) Symbol::fromString(")"))
    {
    }

    Object *read(bool throwOnEof = true)
    {
        skipWhitespace();
        
        if (isEof())
        {
            if (throwOnEof) error("Unexpected end of input stream");
            return (Object*) Eof::getInstance();
        }

        switch (peekChar())
        {
            case ';':
                skipComment();
                return read(throwOnEof);
            case '\'':
                readChar();
                return (Object*) new Pair((Object*) Symbol::fromString("quote"), (Object*) new Pair(read(), (Object*) Null::getInstance()));
            case '(':
                return readList();
            case '"':
                return readString();
            case '#':
                return readSpecial();
            default:
                return readSymbolOrNumber();
        }
    }

private:
    istream *_input;
    Object *dot;
    Object *listEnd;

    void skipWhitespace()
    {
        while (!isEof() && isspace(peekChar()))
            readChar();
    }

    void skipComment()
    {
        while (!isEof() && peekChar() != '\n')
            readChar();
    }

    bool isEof() const
    {
        return _input->peek() == -1;
    }

    int peekChar() const
    {
        assertNotEof();
        return _input->peek();
    }

    int readChar()
    {
        assertNotEof();
        return _input->get();
    }

    void assertNotEof() const
    {
        if (isEof()) error("Unexpected end of input stream");
    }

    Object *readList()
    {
        readChar(); // Opening parenthesis
        Object *ret = (Object*) Null::getInstance();
        Object *current = (Object*) Null::getInstance();

        for (;;)
        {
            Object *o = read();
            if (o == listEnd) return ret; // Closing parenthesis
            if (o == dot)
            {
                if (current->getType() == otNull) error("Read error: Invalid dotted list");
                o = read();
                ((Pair*)current)->_cdr = o;
                if (read() != listEnd)error("Read error: Invalid dotted list");
                return ret;
            }

            Pair *newPair = new Pair(o, (Object*) Null::getInstance());
            if (current->getType() == otNull)
            {
                ret = current = (Object*) newPair;
            }
            else
            {
                ((Pair*)current)->_cdr = (Object*) newPair;
                current = (Object*) newPair;
            }
        }
    }

    Object *readString()
    {
        readChar(); // Opening quote
        vector<int> sb;
        for (char c = readChar(); c != '"'; c = readChar())
        {
            if (c == '\\')
            {
                c = readChar();
                if (c == 'n') c = '\n';
                if (c == 'r') c = '\r';
                if (c == 't') c = '\t';
            }
            sb.push_back((char)c);
        }
        return (Object*) new String(sb);
    }

    Object *readSpecial()
    {
        readChar(); // #
        if (peekChar() == '(') return readVector();
        if (peekChar() != '\\') return readSymbolOrNumber("#");
        readChar();
        return readCharacter();
    }

    Object *readVector()
    {
        readChar(); // Opening parenthesis
        vector<Object*> ret;

        for (;;)
        {
            Object *o = read();
            if (o == listEnd) return (Object*) new Vector(ret); // Closing parenthesis
            if (o == dot) error("Read error: Dot is invalid inside a vector literal");
            ret.push_back(o);
        }
    }

    Object *readCharacter()
    {
        char c = readChar();
        if (!isalpha(c)) return (Object*) new Char(c);

        stringstream sb;
        sb << (char)c;

        while (!isEof() && peekChar() != ')' && !isspace(peekChar())) sb << (char)readChar();
        string name = sb.str();
        if (name == "newline") return (Object*) new Char(10);
        if (name == "cr") return (Object*) new Char(13);
        if (name == "tab") return (Object*) new Char(9);
        if (name == "space") return (Object*) new Char(32);
        if (name.length() == 1) return (Object*) new Char(name[0]);
        error("Read error: Invalid character name: \\" + name);
        return NULL; // Just to keep the compiler happy
    }

    Object *readSymbolOrNumber(const string& init="")
    {
        if (init == "" && peekChar() == ')')
        {
            readChar();
            return listEnd;
        }

        stringstream sb;
        sb << init;
        while (!isEof() && peekChar() != ')' && !isspace(peekChar())) sb << (char)readChar();
        string symbol = sb.str();

        if (symbol == "#t") return (Object*) Boolean::getTrue();
        if (symbol == "#f") return (Object*) Boolean::getFalse();

        bool hasPeriod = symbol.find('.') != string::npos;
        long lValue;
        if (!hasPeriod && sb >> lValue) return (Object*) new Fixnum(lValue);
        sb.clear();
        double dValue;
        if (sb >> dValue) return (Object*) new Flonum(dValue);
        sb.clear();
        if (symbol.substr(0, 2) == "#x")
        {
            sb << hex;
            if (sb >> lValue) return (Object*) new Fixnum(lValue);
        }
        return (Object*) Symbol::fromString(symbol);
    }
};

//----------------------------------------------------------------------------------------------------------------------

Object *car(Object *o)
{
    assertType("car", o, otPair);
    return ((Pair*)o)->_car;
}

Object *cdr(Object *o)
{
    assertType("cdr", o, otPair);
    return ((Pair*)o)->_cdr;
}

Object *isFixnum(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otFixnum);
}

Object *isFlonum(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otFlonum);
}

Object *isSymbol(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otSymbol);
}

Object *isPair(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otPair);
}

Object *isString(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otString);
}

Object *isBoolean(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otBoolean);
}

Object *isChar(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otChar);
}

Object *isNull(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otNull);
}

Object *isProcedure(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otProcedure);
}

Object *isVector(Object *o)
{
    return (Object*) Boolean::valueOf(o->getType() == otVector);
}

Object *integerToChar(Object *o)
{
    assertType("integer->char", o, otFixnum);
    return (Object*) new Char(((Fixnum*)o)->getValue());
}

Object *charToInteger(Object *o)
{
    assertType("char->integer", o, otChar);
    return (Object*) new Fixnum(((Char*)o)->getValue());
}

Object *stringLength(Object *o)
{
    assertType("string-length", o, otString);
    return (Object*) new Fixnum(((String*)o)->getLength());
}

Object *stringToSymbol(Object *o)
{
    assertType("string->symbol", o, otString);
    return (Object*) Symbol::fromString(((String*)o)->getValue());
}

Object *symbolToString(Object *o)
{
    assertType("symbol->string", o, otSymbol);
    string symName = ((Symbol*)o)->getName();
    vector<int> characters;
    for (size_t i=0; i<symName.size(); ++i) characters.push_back(symName[i]);
    return (Object*) new String(characters);
}

Object *vectorLength(Object *o)
{
    assertType("vector-length", o, otVector);
    return (Object*) new Fixnum(((Vector*)o)->getLength());
}

Object *makeString(Object *o)
{
    assertType("make-string", o, otFixnum);
    return (Object*) new String(((Fixnum*)o)->getValue());
}

Object *makeVector(Object *o)
{
    assertType("make-vector", o, otFixnum);
    return (Object*) new Vector(((Fixnum*)o)->getValue());
}

Object *sysDisplayString(Object *o)
{
    assertType("sys:display-string", o, otString);
    cout << ((String*)o)->getValue();
    return Symbol::fromString("undefined");
}

Object *sysExit(Object *o)
{
    assertType("sys:exit", o, otFixnum);
    stringstream sb;
    sb << "Execution stopped with error code " << ((Fixnum*)o)->getValue();
    error(sb.str());
    return NULL; // Just to keep the compiler happy
}

//----------------------------------------------------------------------------------------------------------------------

Object *cons(Object *car, Object *cdr)
{
    return (Object*) new Pair(car, cdr);
}

Object *setCar(Object *o, Object *newCar)
{
    assertType("set-car!", o, otPair);
    ((Pair*)o)->_car = newCar;
    return (Object*) Symbol::fromString("undefined");
}

Object *setCdr(Object *o, Object *newCdr)
{
    assertType("set-cdr!", o, otPair);
    ((Pair*)o)->_car = newCdr;
    return (Object*) Symbol::fromString("undefined");
}

double getDoubleValue(string functionName, Object *o)
{
    switch(o->getType())
    {
    case otFixnum: return ((Fixnum*)o)->getValue(); 
    case otFlonum : return ((Flonum*)o)->getValue(); 
    default: error(functionName + ": Invalid argument type"); return 0;
    }
}

Object *mathPlus(Object *o1, Object *o2)
{
    if (o1->getType() == otFixnum && o2->getType() == otFixnum)
        return (Object*) new Fixnum(((Fixnum*)o1)->getValue() + ((Fixnum*)o2)->getValue());
    else
        return (Object*) new Flonum(getDoubleValue("+", o1) + getDoubleValue("+", o2));
}

Object *mathMinus(Object *o1, Object *o2)
{
    if (o1->getType() == otFixnum && o2->getType() == otFixnum)
        return (Object*) new Fixnum(((Fixnum*)o1)->getValue() - ((Fixnum*)o2)->getValue());
    else
        return (Object*) new Flonum(getDoubleValue("-", o1) - getDoubleValue("-", o2));
}

Object *mathMult(Object *o1, Object *o2)
{
    if (o1->getType() == otFixnum && o2->getType() == otFixnum)
        return (Object*) new Fixnum(((Fixnum*)o1)->getValue() * ((Fixnum*)o2)->getValue());
    else
        return (Object*) new Flonum(getDoubleValue("*", o1) * getDoubleValue("*", o2));
}

Object *mathDiv(Object *o1, Object *o2)
{
    if (o1->getType() == otFixnum && o2->getType() == otFixnum)
        return (Object*) new Flonum((double)((Fixnum*)o1)->getValue() / ((Fixnum*)o2)->getValue());
    else
        return (Object*) new Flonum(getDoubleValue("/", o1) / getDoubleValue("/", o2));
}

Object *mathLt(Object *o1, Object *o2)
{
    if (o1->getType() == otFixnum && o2->getType() == otFixnum)
        return (Object*) Boolean::valueOf(((Fixnum*)o1)->getValue() < ((Fixnum*)o2)->getValue());
    else
        return (Object*) Boolean::valueOf(getDoubleValue("<", o1) < getDoubleValue("<", o2));
}

Object *mathEq(Object *o1, Object *o2)
{
    if (o1->getType() == otFixnum && o2->getType() == otFixnum)
        return (Object*) Boolean::valueOf(((Fixnum*)o1)->getValue() == ((Fixnum*)o2)->getValue());
    else
        return (Object*) Boolean::valueOf(getDoubleValue("=", o1) == getDoubleValue("=", o2));
}

Object *mathQuotient(Object *o1, Object *o2)
{
    if (o1->getType() == otFixnum && o2->getType() == otFixnum)
        return (Object*) new Fixnum(((Fixnum*)o1)->getValue() / ((Fixnum*)o2)->getValue());
    else
        return (Object*) new Fixnum((int) getDoubleValue("quotient", o1) / (int) getDoubleValue("quotient", o2));
}

Object *mathRemainder(Object *o1, Object *o2)
{
    if (o1->getType() == otFixnum && o2->getType() == otFixnum)
        return (Object*) new Fixnum(((Fixnum*)o1)->getValue() % ((Fixnum*)o2)->getValue());
    else
        return (Object*) new Fixnum((int) getDoubleValue("remainder", o1) % (int) getDoubleValue("remainder", o2));
}

Object *eq(Object *o1, Object *o2)
{
    return (Object*) Boolean::valueOf(o1 == o2);
}

Object* applyHack(Object *form, Environment *env);

Object *apply(Object *o, Object *args)
{
    assertType("apply", o, otProcedure);
    Procedure *proc = (Procedure*) o;
    vector<Object*> params;
    if (args->getType() != otNull && args->getType() != otPair) error("apply: Invalid argument type");
    for (Object *i = args; i->getType() == otPair; i = ((Pair*)i)->_cdr) params.push_back(((Pair*)i)->_car);

    if (proc->isBuiltin()) return proc->call(&params);

    Lambda *l = (Lambda*) proc;
    Environment *expandEnv = l->getCapturedEnvironment()->extendIntoNew(l->getArgumentNames(), &params, l->hasRestParameter());
    return applyHack(l->getBody(), expandEnv);
} 

Object *stringRef(Object *o1, Object *o2)
{
    assertType("string-ref", o1, otString);
    assertType("string-ref", o2, otFixnum);
    return (Object*) new Char(((String*)o1)->GetAt(((Fixnum*)o2)->getValue()));
}

Object *vectorRef(Object *o1, Object *o2)
{
    assertType("vector-ref", o1, otVector);
    assertType("vector-ref", o2, otFixnum);
    return (Object*) ((Vector*)o1)->GetAt(((Fixnum*)o2)->getValue());
}

Object *sysStrToNum(Object *o1, Object *o2)
{
    assertType("sys:strtonum", o1, otString);
    assertType("sys:strtonum", o2, otFixnum);
    string strValue = ((String*)o1)->getValue();
    long base = ((Fixnum*)o2)->getValue();
    stringstream sb;
    switch(base)
    {
    case 2: error("sys:strtonum: Base 2 not implemented yet"); return NULL; // TODO
    case 8: sb << oct; break;
    case 10: sb << dec; break;
    case 16: sb << hex; break;
    default: error("sys:strtonum: Invalid base"); return NULL;
    }

    sb << strValue;
    // TODO: Make sure the string consists of digits and a single period only!
    bool hasPeriod = strValue.find('.') != string::npos;
    long lValue;
    if (!hasPeriod && sb >> lValue) return (Object*) new Fixnum(lValue);
    sb.clear();
    double dValue;
    if (sb >> dValue) return (Object*) new Flonum(dValue);
    sb.clear();
    return Symbol::fromString("nan");
}

Object *sysNumToStr(Object *o1, Object *o2)
{
    stringstream sb;
    assertType("sys:numtostr", o2, otFixnum);
    long base = ((Fixnum*)o2)->getValue();

    switch(base)
    {
    case 2: error("sys:numtostr: Base 2 not implemented yet"); return NULL; // TODO
    case 8: sb << oct; break;
    case 10: sb << dec; break;
    case 16: sb << hex; break;
    default: error("sys:numtostr: Invalid base"); return NULL;
    }

    if (o1->getType() == otFixnum) sb << ((Fixnum*)o1)->getValue();
    else if (o1->getType() == otFlonum) sb << ((Flonum*)o1)->getValue();
    else error("sys:numtostr: Invalid argument type");

    string sValue;
    sb >> sValue;
    vector<int> characters;
    for (size_t i=0; i<sValue.size(); ++i) characters.push_back(sValue[i]);
    return (Object*) new String(characters);
}

//----------------------------------------------------------------------------------------------------------------------

Object *stringSet(Object *o1, Object *o2, Object *o3)
{
    assertType("string-set!", o1, otString);
    assertType("string-set!", o2, otFixnum);
    assertType("string-set!", o3, otChar);
    ((String*)o1)->SetAt(((Fixnum*)o2)->getValue(), ((Char*)o3)->getValue());
    return Symbol::fromString("undefined");
}

Object *vectorSet(Object *o1, Object *o2, Object *o3)
{
    assertType("vector-set!", o1, otVector);
    assertType("vector-set!", o2, otFixnum);
    ((Vector*)o1)->SetAt(((Fixnum*)o2)->getValue(), o3);
    return Symbol::fromString("undefined");
}

//----------------------------------------------------------------------------------------------------------------------

#define DEFUN1(name, lispName) _global.define(lispName, new UnaryProcedure(lispName, &name))
#define DEFUN2(name, lispName) _global.define(lispName, new BinaryProcedure(lispName, &name))
#define DEFUN3(name, lispName) _global.define(lispName, new TrinaryProcedure(lispName, &name))

class Interpreter
{
public:
    Interpreter()
    {
        DEFUN1(car, "car");
        DEFUN1(cdr, "cdr");
        DEFUN1(isFixnum, "integer?");
        DEFUN1(isFlonum, "real?");
        DEFUN1(isSymbol, "symbol?");
        DEFUN1(isPair, "pair?");
        DEFUN1(isString, "string?");
        DEFUN1(isBoolean, "boolean?");
        DEFUN1(isChar, "char?");
        DEFUN1(isNull, "null?");
        DEFUN1(isProcedure, "procedure?");
        DEFUN1(isVector, "vector?");
        DEFUN1(integerToChar, "integer->char");
        DEFUN1(charToInteger, "char->integer");
        DEFUN1(stringLength, "string-length");
        DEFUN1(stringToSymbol, "string->symbol");
        DEFUN1(symbolToString, "symbol->string");
        DEFUN1(vectorLength, "vector-length");
        DEFUN1(makeString, "make-string");
        DEFUN1(makeVector, "make-vector");
        DEFUN1(sysDisplayString, "sys:display-string");
        DEFUN1(sysExit, "sys:exit");

        DEFUN2(cons, "cons");
        DEFUN2(setCar, "set-car!");
        DEFUN2(setCdr, "set-cdr!");
        DEFUN2(mathPlus, "+");
        DEFUN2(mathMinus, "-");
        DEFUN2(mathMult, "*");
        DEFUN2(mathDiv, "/");
        DEFUN2(mathLt, "<");
        DEFUN2(mathEq, "=");
        DEFUN2(mathQuotient, "quotient");
        DEFUN2(mathRemainder, "remainder");
        DEFUN2(eq, "eq?");
        DEFUN2(apply, "apply");
        DEFUN2(stringRef, "string-ref");
        DEFUN2(vectorRef, "vector-ref");
        DEFUN2(sysStrToNum, "sys:strtonum");
        DEFUN2(sysNumToStr, "sys:numtostr");

        DEFUN3(stringSet, "string-set!");
        DEFUN3(vectorSet, "vector-set!");

        ifstream in("init.scm");
        evalAll(in);
    }

    Object* eval(string expression)
    {
        stringstream sb;
        sb << expression;
        return evalAll(sb);
    }

    Object* evalExpandedForm(Object *form, Environment *env)
    {
tailCall:
        //cout << "evalExpandedForm: " << form->toString() << endl;

        switch (form->getType())
        {
        case otNull:
            error("eval: Empty list can not be evaluated");
            return NULL; // Just to keep the compiler happy

        case otVector:
            error("eval: Vector must be quoted");
            return NULL; // Just to keep the compiler happy

        case otSymbol:
            return env->get(((Symbol*)form)->getName());

        case otPair:
            {
                Pair *asPair = (Pair*) form;
                if (asPair->_car->getType() == otSymbol)
                {
                    string sym = ((Pair*)form)->_car->toString();
                    if (sym == "define") return evalDefine(asPair, env);
                    if (sym == "set!") return evalSet(asPair, env);
                    if (sym == "lambda") return evalLambda(asPair, env);
                    if (sym == "quote") return evalQuote(asPair, env);

                    if (sym == "if")
                    {
                        if (asPair->_cdr->getType() != otPair) error("eval: Invalid if form");
                        Object *condition = ((Pair*)asPair->_cdr)->_car;
                        Object *rest = ((Pair*)asPair->_cdr)->_cdr;
                        if (rest->getType() != otPair) error("eval: Invalid if form");
                        Pair *restAsPair = (Pair*) rest;
                        Object *thenPart = restAsPair->_car;
                        rest = restAsPair->_cdr;
                        if (rest->getType() != otPair) error("eval: Invalid if form");
                        restAsPair = (Pair*) rest;
                        Object *elsePart = restAsPair->_car;
                        if (restAsPair->_cdr->getType() != otNull) error("eval: Invalid if form");

                        Object *conditionValue = evalExpandedForm(condition, env);
                        form = (conditionValue->getType() != otBoolean || ((Boolean*)conditionValue)->getValue()) ? thenPart : elsePart;
                        goto tailCall;
                    }

                    if (sym == "begin") 
                    {
                        for (Object *i = asPair->_cdr; ;)
                        {
                            if (i->getType() == otNull) error("eval: Invalid begin form");
                            if (i->getType() != otPair) error("eval: Dotted list not allowed in begin form");

                            Pair *p = (Pair*) i;

                            if (p->_cdr->getType() == otNull)
                            {
                                // Execute last form in tail position
                                form = ((Pair*)i)->_car;
                                goto tailCall;
                            }

                            evalExpandedForm(p->_car, env);
                            i = p->_cdr;
                        }
                    }
                }

                Object *function = evalExpandedForm(asPair->_car, env);
                vector<Object*> *parameters = new vector<Object*>();

                for (Object *i = asPair->_cdr; ;)
                {
                    if (i->getType() == otNull) break;
                    if (i->getType() != otPair) error("eval: Dotted list not allowed in function call");
                    Pair *p = (Pair*) i;
                    parameters->push_back(evalExpandedForm(p->_car, env));
                    i = p->_cdr;
                }

                if (function->getType() != otProcedure)
                    error("eval: '" + function->toString() + "' is not callable");

                if (((Procedure*)function)->isBuiltin())
                {
                    return ((Procedure*)function)->call(parameters);
                }
                else
                {
                    Lambda *l = (Lambda*)function;
                    form = l->getBody();
                    env =  l->getCapturedEnvironment()->extendIntoNew(l->getArgumentNames(), parameters, l->hasRestParameter());
                    goto tailCall;
                }
            }
        default:
            return form;
        }
    }

private:
    Environment _global;
    map<string, Lambda*> _macros;

    Object *evalAll(istream& in)
    {
        Reader rd(&in);
        Object *ret = (Object*) Null::getInstance();
        for (Object *o=rd.read(false); o->getType() != otEof; o=rd.read(false))
        {
            handleMacros(&o);
            //cout << endl << "eval: " << o->toString() << endl;
            ret = evalExpandedForm(o, &_global);
        }
        return ret;
    }

    void handleMacros(Object **obj)
    {
        if ((*obj)->getType() != otPair) return;
        Pair *asPair = (Pair*) *obj;
        if (asPair->_car->getType() != otSymbol) return;
        if (asPair->_car->toString() == "defmacro")
        {
            if (asPair->_cdr->getType() != otPair) error("Invalid defmacro form: Expected (defmacro name (parameters) form ...)");
            if (((Pair*)asPair->_cdr)->_car->getType() != otSymbol) error("Invalid defmacro form: Name must be a symbol");
            string name = ((Pair*)asPair->_cdr)->_car->toString();
            if (((Pair*)asPair->_cdr)->_cdr->getType() != otPair) error("Invalid defmacro form");
            _macros[name] = (Lambda*) evalLambda((Pair*)((Pair*)asPair->_cdr), &_global);
            *obj = (Object*) Boolean::getTrue();
            return;
        }

        for (;;) if (!expandMacros(obj)) break;
    }

    bool expandMacros(Object **obj)
    {
        if ((*obj)->getType() != otPair) return false;
        Pair *asPair = (Pair*) *obj;
        if (asPair->_car == Symbol::fromString("quote")) return false;

        for (Object *i = *obj; i->getType() == otPair; i = ((Pair*)i)->_cdr)
            if (expandMacros(&((Pair*)i)->_car))
                return true;

        if (asPair->_car->getType() != otSymbol) return false;
        string sym = asPair->_car->toString();
        if (!_macros.count(sym)) return false;

        Lambda *l = _macros[sym];
        vector<Object*> params;
        for (Object *i = asPair->_cdr; i->getType() == otPair; i = ((Pair*)i)->_cdr) params.push_back(((Pair*)i)->_car);
        Environment *expandEnv = l->getCapturedEnvironment()->extendIntoNew(l->getArgumentNames(), &params, l->hasRestParameter());
        //cout << endl << "expandMacro: " << l->getBody()->toString() << endl;
        *obj = evalExpandedForm(l->getBody(), expandEnv);
        return true;
    }

    Object* evalDefine(Pair *asPair, Environment *env)
    {
        if (asPair->_cdr->getType() != otPair) error("eval: Invalid define form");
        Object *whatToDefine = ((Pair*)asPair->_cdr)->_car;
        Object *definedAs = ((Pair*)asPair->_cdr)->_cdr;

        switch (whatToDefine->getType())
        {
        case otPair:
            {
                Object *nameObj = ((Pair*)whatToDefine)->_car;
                if (nameObj->getType() != otSymbol) error("eval: Invalid define form");
                string name = nameObj->toString();
                vector<string> parameterNames;
                bool hasRestParameter = false;
                for (Object *i = ((Pair*)whatToDefine)->_cdr; ; )
                {
                    if (i->getType() == otNull) break;
                    if (i->getType() != otPair)
                    {
                        if (i->getType() != otSymbol) error("eval: Invalid define form");
                        parameterNames.push_back(i->toString());
                        hasRestParameter = true;
                        break;
                    }
                    Pair *p = (Pair*) i;
                    parameterNames.push_back(p->_car->toString());
                    i = p->_cdr;
                }

                env->define(name, new Lambda(name, definedAs, env, parameterNames, hasRestParameter));
                return Symbol::fromString("undefined"); 
            }
        case otSymbol:
            if (definedAs->getType() != otPair || ((Pair*)definedAs)->_cdr->getType() != otNull) error("eval: Invalid define form");
            env->define(((Symbol*)whatToDefine)->getName(), evalExpandedForm(((Pair*)definedAs)->_car, env));
            return Symbol::fromString("undefined");
        default:
            error("eval: Invalid define form");
            return NULL; // Just to keep the compiler happy
        }
    }

    Object* evalSet(Pair *asPair, Environment *env)
    {
        if (asPair->_cdr->getType() != otPair) error("eval: Invalid set! form");
        Object *whatToSet = ((Pair*)asPair->_cdr)->_car;
        Object *definedAs = ((Pair*)asPair->_cdr)->_cdr;
        if (whatToSet->getType() != otSymbol) error("eval: Invalid set! form");
        if (definedAs->getType() != otPair || ((Pair*)definedAs)->_cdr->getType() != otNull) error("eval: Invalid set! form");
        env->set(((Symbol*)whatToSet)->getName(), evalExpandedForm(((Pair*)definedAs)->_car, env));
        return Symbol::fromString("undefined");
    }

    Object* evalLambda(Pair *asPair, Environment *env)
    {
        string name = asPair->_car->toString();
        if (asPair->_cdr->getType() != otPair) error("eval: Invalid lambda form");
        Pair *parametersAndBody = (Pair*)asPair->_cdr;
        Object *parameters = parametersAndBody->_car;
        Object *body = parametersAndBody->_cdr;
        vector<string> argumentNames;
        bool hasRestParameter = false;

        switch (parameters->getType())
        {
        case otSymbol: // (lambda a (form) (form) (form))
            argumentNames.push_back(parameters->toString());
            return new Lambda(name, body, env, argumentNames, true);
        case otNull: // (lambda () (form) (form) (form))
            return new Lambda(name, body, env, argumentNames, false);
        case otPair: // (lambda (a b c) (form) (form) (form))
            for (Object *i = parameters; ; )
            {
                if (i->getType() == otNull) break;
                if (i->getType() != otPair)
                {
                    if (i->getType() != otSymbol) error("eval: Invalid lambda form");
                    argumentNames.push_back(i->toString());
                    hasRestParameter = true;
                    break;
                }
                Pair *p = (Pair*) i;
                argumentNames.push_back(p->_car->toString());
                i = p->_cdr;
            }
            return new Lambda(name, body, env, argumentNames, hasRestParameter);
        default:
            error("Invalid lambda form");
            return NULL; // Just to make the compiler happy
        }
    }

    Object* evalQuote(Pair *asPair, Environment *env)
    {
        if (asPair->_cdr->getType() != otPair || ((Pair*)asPair->_cdr)->_cdr->getType() != otNull)
            error("eval: Invalid quote form");
        return ((Pair*)asPair->_cdr)->_car;
    }
};

//----------------------------------------------------------------------------------------------------------------------

Interpreter interp;

Object* applyHack(Object *form, Environment *env)
{
    return interp.evalExpandedForm(form, env);
}

int main()
{
    for (;;)
    {
        try
        {
            string expression;
            cout << "> " << flush;
            cin.clear();
            getline(cin, expression);
            if (!cin) break;
            if (expression.length() >= 2 && expression[0] == ',')
            {
                if (expression[1] == 'q') break;
                // HACK: Add some more
            }
            cout << interp.eval(expression)->toString() << endl;
        }
        catch(string& message)
        {
            cout << "Error: " << message << endl;
        }
        catch(char* message)
        {
            cout << "Error: " << message << endl;
        }
    }
    return 0;
}


    //public sealed class RegisterMachine : VirtualMachine
    //{
    //    private int programCounter;
    //    private Environment environmentRegister;
    //    private int continueRegister;
    //    private object valueRegister;
    //    private object argumentsRegister;

    //    private readonly List<int> gotosWithoutLabelValue = new List<int>();
    //    private readonly Dictionary<string, int> labelPositions = new Dictionary<string, int>();
    //    private readonly List<Instruction> Instructions = new List<Instruction>();
    //    public int ProgramSize { get { return Instructions.Count; } }

    //    private readonly Func<object, bool> IsTrue;
    //    private readonly Stack<object> stack = new Stack<object>();
    //    private readonly Environment globalEnvironment = new Environment();

    //    public RegisterMachine(Func<object, bool> isTrue)
    //    {
    //        IsTrue = isTrue;
    //    }

    //    private void PerformArgsToValue() { valueRegister = argumentsRegister; programCounter++; }
    //    private void PerformBranchLabel(int target) { programCounter = IsTrue(valueRegister) ? target : programCounter + 1; }
    //    private void PerformContinue() { programCounter = continueRegister; }
    //    private void PerformDefineVariable(Symbol variable) { environmentRegister.Define(variable, valueRegister); programCounter++; }
    //    private void PerformGetVariable(Symbol variable) { valueRegister = environmentRegister.Get(variable); programCounter++; }
    //    private void PerformGotoLabel(int target) { programCounter = target; }
    //    private void PerformInitArgs() { argumentsRegister = null; programCounter++; }
    //    private void PerformLdConst(object value) { valueRegister = value; programCounter++; }
    //    private void PerformLdCont(int target) { continueRegister = target; programCounter++; }
    //    private void PerformPushParam() { argumentsRegister = new Pair(valueRegister, argumentsRegister); programCounter++; }
    //    private void PerformRestoreRegisters() { environmentRegister = (Environment)stack.Pop(); continueRegister = (int)stack.Pop(); argumentsRegister = stack.Pop(); programCounter++; }
    //    private void PerformSaveRegisters() { stack.Push(argumentsRegister); stack.Push(continueRegister); stack.Push(environmentRegister); programCounter++; }
    //    private void PerformSetVariable(Symbol variable) { environmentRegister.Set(variable, valueRegister); programCounter++; }
    //    private void PerformMakeClosure(string name, int target, bool hasRestParameter, Symbol[] parameterNames) { valueRegister = new Closure(name, environmentRegister, target, parameterNames, hasRestParameter); programCounter++; }
    //    private void PerformValueToArgs() { argumentsRegister = valueRegister; programCounter++; }

    //    private void PerformCall()
    //    {
    //        object[] args;
    //        if (argumentsRegister == null) args = new object[0];
    //        else if (argumentsRegister is Pair) args = ((Pair)argumentsRegister).ToArray();
    //        else throw new VirtualMachineException("Invalid function application: Expected list of arguments, got " + argumentsRegister.GetType());

    //        argumentsRegister = null;

    //        if (valueRegister is Func<object[], object>)
    //        {
    //            valueRegister = ((Func<object[], object>)valueRegister)(args);
    //            programCounter = continueRegister;
    //            return;
    //        }

    //        if (valueRegister is Closure)
    //        {
    //            var closure = (Closure)valueRegister;
    //            var env = new Environment((closure).Captured);

    //            if (closure.HasRestParameter)
    //            {
    //                if (closure.ParameterNames.Length - 1 > args.Length)
    //                    throw new VirtualMachineException("Invalid parameter count in call to '" + closure.Name + "': Expected " + (closure.ParameterNames.Length - 1) + " or more, got " + args.Length);
    //                for (int i = 0; i < closure.ParameterNames.Length - 1; ++i) env.Define(closure.ParameterNames[i], args[i]);
    //                env.Define(closure.ParameterNames.Last(), Pair.FromEnumerable(args.Skip(closure.ParameterNames.Length - 1)));
    //            }
    //            else
    //            {
    //                if (closure.ParameterNames.Length != args.Length)
    //                    throw new VirtualMachineException("Invalid parameter count in call to '" + closure.Name + "': Expected " + closure.ParameterNames.Length + ", got " + args.Length);
    //                for (int i = 0; i < closure.ParameterNames.Length; ++i) env.Define(closure.ParameterNames[i], args[i]);
    //            }

    //            environmentRegister = env;
    //            programCounter = closure.PC;
    //            return;
    //        }

    //        throw new VirtualMachineException("Invalid CALL target");
    //    }

    //    public void EmitLabel(string label)
    //    {
    //        if (labelPositions.ContainsKey(label)) throw new VirtualMachineException("Label defined twice: '" + label + "'");
    //        int targetPC = Instructions.Count;
    //        labelPositions[label] = targetPC;

    //        for (int i = 0; i < gotosWithoutLabelValue.Count; ++i)
    //        {
    //            if (Instructions[gotosWithoutLabelValue[i]].Label == label)
    //            {
    //                Instructions[gotosWithoutLabelValue[i]].LabelTarget = targetPC;
    //                gotosWithoutLabelValue.RemoveAt(i);
    //                i--;
    //            }
    //        }
    //    }

    //    public void EmitArgsToValue() { Emit(new LambdaInstruction(PerformArgsToValue)); }
    //    public void EmitBranchLabel(string label) { Emit(new BranchLabelInstruction(label)); }
    //    public void EmitCall() { Emit(new LambdaInstruction(PerformCall)); }
    //    public void EmitContinue() { Emit(new LambdaInstruction(PerformContinue)); }
    //    public void EmitDefineVariable(Symbol variable) { Emit(new LambdaInstruction(() => PerformDefineVariable(variable))); }
    //    public void EmitGetVariable(Symbol variable) { Emit(new LambdaInstruction(() => PerformGetVariable(variable))); }
    //    public void EmitGotoLabel(string label) { Emit(new GotoLabelInstruction(label)); }
    //    public void EmitInitArgs() { Emit(new LambdaInstruction(PerformInitArgs)); }
    //    public void EmitLdConst(object value) { Emit(new LambdaInstruction(() => PerformLdConst(value))); }
    //    public void EmitLdCont(string label) { Emit(new LdContInstruction(label)); }
    //    public void EmitMakeClosure(string name, string label, bool hasRestParameter, Symbol[] parameterNames) { Emit(new MakeClosureInstruction(name, label, hasRestParameter, parameterNames)); }
    //    public void EmitPushParam() { Emit(new LambdaInstruction(PerformPushParam)); }
    //    public void EmitRestoreRegisters() { Emit(new LambdaInstruction(PerformRestoreRegisters)); }
    //    public void EmitSaveRegisters() { Emit(new LambdaInstruction(PerformSaveRegisters)); }
    //    public void EmitSetVariable(Symbol variable) { Emit(new LambdaInstruction(() => PerformSetVariable(variable))); }
    //    public void EmitValueToArgs() { Emit(new LambdaInstruction(PerformValueToArgs)); }

    //    private void Emit(Instruction value)
    //    {
    //        if (value.Label != null)
    //        {
    //            if (labelPositions.ContainsKey(value.Label)) value.LabelTarget = labelPositions[value.Label];
    //            else gotosWithoutLabelValue.Add(Instructions.Count);
    //        }
    //        Instructions.Add(value);
    //    }

    //    private void AssertRunnable()
    //    {
    //        if (gotosWithoutLabelValue.Any()) throw new VirtualMachineException("Invalid program: Jump targets without valid label");
    //    }

    //    private int nextLabelNo;
    //    public string MakeLabel()
    //    {
    //        return "##label##" + nextLabelNo++ + "##";
    //    }

    //    public void SetVariable(string name, object value) { globalEnvironment.Define(Symbol.FromString(name), value); }
    //    public void SetVariable(Symbol name, object value) { globalEnvironment.Define(name, value); }
    //    public bool HasVariable(Symbol name) { return globalEnvironment.HasVariable(name); }
    //    public object GetVariable(Symbol name) { return globalEnvironment.Get(name); }

    //    public object Run(int startingPC = 0)
    //    {
    //        AssertRunnable();

    //        programCounter = startingPC;
    //        environmentRegister = globalEnvironment;
    //        continueRegister = -1;
    //        valueRegister = null;
    //        argumentsRegister = null;
    //        stack.Clear();

    //        while (programCounter < Instructions.Count)
    //        {
    //            Instructions[programCounter].Execute(this);
    //            if (programCounter == -1) break;
    //        }

    //        if (stack.Any()) throw new VirtualMachineException("Bad program: Stack not empty after last instruction");
    //        if (argumentsRegister != null) throw new VirtualMachineException("Bad program: Arguments register not empty after last instruction");
    //        return valueRegister;
    //    }

    //    public static bool IsCallable(object value)
    //    {
    //        return value is Func<object[], object> || value is Closure;
    //    }

    //    private sealed class Closure
    //    {
    //        public readonly Environment Captured;
    //        public readonly int PC;
    //        public readonly Symbol[] ParameterNames;
    //        public readonly bool HasRestParameter;
    //        public readonly string Name;

    //        public Closure(string name, Environment captured, int pc, Symbol[] parameterNames, bool hasRestParameter)
    //        {
    //            Name = name;
    //            Captured = captured;
    //            PC = pc;
    //            ParameterNames = parameterNames;
    //            HasRestParameter = hasRestParameter;
    //        }

    //        public override string ToString() { return "<Compiled function " + Name + ">"; }
    //    }

    //    private class Instruction
    //    {
    //        public readonly string Label;
    //        public int LabelTarget;
    //        protected Instruction(string label) { Label = label; }
    //        public virtual void Execute(RegisterMachine machine) { }
    //    }

    //    private sealed class LambdaInstruction : Instruction
    //    {
    //        private readonly Action f;
    //        public LambdaInstruction(Action f) : base(null) { this.f = f; }
    //        public override void Execute(RegisterMachine machine) { f(); }
    //    }

    //    private sealed class BranchLabelInstruction : Instruction
    //    {
    //        public BranchLabelInstruction(string label) : base(label) { }
    //        public override void Execute(RegisterMachine machine) { machine.PerformBranchLabel(LabelTarget); }
    //    }

    //    private sealed class GotoLabelInstruction : Instruction
    //    {
    //        public GotoLabelInstruction(string label) : base(label) { }
    //        public override void Execute(RegisterMachine machine) { machine.PerformGotoLabel(LabelTarget); }
    //    }

    //    private sealed class LdContInstruction : Instruction
    //    {
    //        public LdContInstruction(string label) : base(label) { }
    //        public override void Execute(RegisterMachine machine) { machine.PerformLdCont(LabelTarget); }
    //    }

    //    private sealed class MakeClosureInstruction : Instruction
    //    {
    //        private readonly string Name;
    //        private readonly bool HasRestParameter;
    //        private readonly Symbol[] ParameterNames;
    //        public MakeClosureInstruction(string name, string label, bool hasRestParameter, Symbol[] parameterNames) : base(label) { Name = name; HasRestParameter = hasRestParameter; ParameterNames = parameterNames; }
    //        public override void Execute(RegisterMachine machine) { machine.PerformMakeClosure(Name, LabelTarget, HasRestParameter, ParameterNames); }
    //    }
    //}












    //    private void HandleMacros(ref object obj)
    //    {
    //        if (obj == null) return;
    //        if (!(obj is Pair)) return;
    //        if (!(((Pair)obj).First is Symbol)) return;
    //        var form = ((Pair)obj).ToList();

    //        if (form[0].ToString() == "defmacro")
    //        {
    //            if (!(form[1] is Symbol)) throw new SchemeException("Invalid defmacro form: Name must be a symbol");
    //            string name = "sys:macro##" + form[1] + "##";
    //            obj = new Pair(Symbol.FromString("define"), new Pair(new Pair(Symbol.FromString(name), form[2]), ((Pair)((Pair)((Pair)obj).Second).Second).Second));
    //            return;
    //        }

    //        while (true) if (!ExpandMacros(ref obj)) break;
    //    }

    //    private bool ExpandMacros(ref object obj)
    //    {
    //        if (obj == null) return false;
    //        if (!(obj is Pair)) return false;
    //        if (((Pair)obj).First.ToString() == "quote") return false;
    //        for (object i = obj; i is Pair; i = ((Pair)i).Second) if (ExpandMacros(ref ((Pair)i).First)) return true;

    //        Symbol o1 = ((Pair)obj).First as Symbol;
    //        if (o1 == null) return false;

    //        Symbol macroSymbol = Symbol.FromString("sys:macro##" + o1 + "##");
    //        if (!machine.HasVariable(macroSymbol)) return false;

    //        int nextPC = machine.ProgramSize;
    //        compiler.Compile(new Pair(macroSymbol, Pair.FromEnumerable(((Pair)((Pair)obj).Second).Select(i => new Pair(Symbol.FromString("quote"), new Pair(i, null))))));
    //        obj = machine.Run(nextPC);

    //        return true;
    //    }
