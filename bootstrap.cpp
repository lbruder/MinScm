// vim:et

// MinScm.cpp version 2013-03-25
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

#define error(msg) do { cout << msg << endl; throw 0; } while(0)

enum ObjectType { otFixnum, otFlonum, otSymbol, otPair, otString, otBoolean, otChar, otNull, otProcedure, otVector, otEof, otEnvironment, otTag };

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

class Tag: public Object
{
public:
    Tag(Object *value): _value(value) { }
    Object *getValue() { return _value; }
    ObjectType getType() const { return otTag; }
    string toString() const { return "<tag " + _value->toString() + ">"; }

private:
    Object *_value;
};

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
            case '`':
                readChar();
                return (Object*) new Pair((Object*) Symbol::fromString("quasiquote"), (Object*) new Pair(read(), (Object*) Null::getInstance()));
            case ',':
                readChar();
                return (Object*) new Pair((Object*) Symbol::fromString("unquote"), (Object*) new Pair(read(), (Object*) Null::getInstance()));
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

        int periods = 0;
        bool digitsAndPeriodsOnly = true;
        for (size_t i=0; i<symbol.size(); ++i)
        {
            if (symbol[i] == '.') ++periods;
            else if (symbol[i] < '0' || symbol[i] > '9') digitsAndPeriodsOnly = false;
        }

        long lValue;
        if (periods == 0 && digitsAndPeriodsOnly && sb >> lValue) return (Object*) new Fixnum(lValue);
        sb.clear();
        double dValue;
        if (periods < 2 && digitsAndPeriodsOnly && sb >> dValue) return (Object*) new Flonum(dValue);
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

Object *sysType(Object *o)
{
    switch (o->getType())
    {
    case otFixnum: return Symbol::fromString("fixnum");
    case otFlonum: return Symbol::fromString("flonum");
    case otSymbol: return Symbol::fromString("symbol");
    case otPair: return Symbol::fromString("pair");
    case otString: return Symbol::fromString("string");
    case otBoolean: return Symbol::fromString("boolean");
    case otChar: return Symbol::fromString("char");
    case otNull: return Symbol::fromString("null");
    case otProcedure: return Symbol::fromString("procedure");
    case otVector: return Symbol::fromString("vector");
    case otEof: return Symbol::fromString("eof");
    case otEnvironment: return Symbol::fromString("environment");
    case otTag: return Symbol::fromString("tag");
    default: throw "type: Invalid argument type";
    }
}

Object *sysTag(Object *o)
{
    return new Tag(o);
}

Object *sysUntag(Object *o)
{
    assertType("untag", o, otTag);
    return ((Tag*)o)->getValue();
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
    assertType("display-string", o, otString);
    cout << ((String*)o)->getValue();
    return Symbol::fromString("undefined");
}

Object *sysExit(Object *o)
{
    assertType("exit", o, otFixnum);
    stringstream sb;
    sb << "Execution stopped with error code " << ((Fixnum*)o)->getValue();
    error(sb.str());
    return NULL; // Just to keep the compiler happy
}

Object *sysFixToFlo(Object *o1)
{
    assertType("fix->flo", o1, otFixnum);
    return (Object*) new Flonum(((Fixnum*)o1)->getValue());
}

Object *sysStrToFlo(Object *o1)
{
    assertType("str->flo", o1, otString);
    string strValue = ((String*)o1)->getValue();
    stringstream sb;
    sb << strValue;
    // TODO: Make sure the string consists of digits only!
    double dValue;
    if (sb >> dValue) return (Object*) new Flonum(dValue);
    return Symbol::fromString("nan");
}

Object *sysFloToStr(Object *o1)
{
    stringstream sb;
    assertType("flo->str", o1, otFlonum);

    sb << ((Flonum*)o1)->getValue();
    string sValue;
    sb >> sValue;
    vector<int> characters;
    for (size_t i=0; i<sValue.size(); ++i) characters.push_back(sValue[i]);
    return (Object*) new String(characters);
}

//----------------------------------------------------------------------------------------------------------------------

Object *setCar(Object *o, Object *newCar)
{
    assertType("set-car!", o, otPair);
    ((Pair*)o)->_car = newCar;
    return (Object*) Symbol::fromString("undefined");
}

Object *setCdr(Object *o, Object *newCdr)
{
    assertType("set-cdr!", o, otPair);
    ((Pair*)o)->_cdr = newCdr;
    return (Object*) Symbol::fromString("undefined");
}

int getFix(const char* procedure, Object *o)
{
    assertType(procedure, o, otFixnum);
    return ((Fixnum*)o)->getValue();
}

double getFlo(const char* procedure, Object *o)
{
    assertType(procedure, o, otFlonum);
    return ((Flonum*)o)->getValue();
}

Object *cons(Object *car, Object *cdr) { return (Object*) new Pair(car, cdr); }
Object *fixPlus(Object *o1, Object *o2) { return (Object*) new Fixnum(getFix("fix+", o1) + getFix("fix+", o2)); }
Object *fixMinus(Object *o1, Object *o2) { return (Object*) new Fixnum(getFix("fix-", o1) - getFix("fix-", o2)); }
Object *fixMult(Object *o1, Object *o2) { return (Object*) new Fixnum(getFix("fix*", o1) * getFix("fix*", o2)); }
Object *fixDiv(Object *o1, Object *o2) { return (Object*) new Fixnum(getFix("fix/", o1) / getFix("fix/", o2)); }
Object *fixMod(Object *o1, Object *o2) { return (Object*) new Fixnum(getFix("fix%", o1) % getFix("fix%", o2)); }
Object *fixLt(Object *o1, Object *o2) { return (Object*) Boolean::valueOf(getFix("fix<", o1) < getFix("fix<", o2)); }
Object *fixEq(Object *o1, Object *o2) { return (Object*) Boolean::valueOf(getFix("fix=", o1) == getFix("fix=", o2)); }
Object *floPlus(Object *o1, Object *o2) { return (Object*) new Flonum(getFlo("flo+", o1) + getFlo("flo+", o2)); }
Object *floMinus(Object *o1, Object *o2) { return (Object*) new Flonum(getFlo("flo-", o1) - getFlo("flo-", o2)); }
Object *floMult(Object *o1, Object *o2) { return (Object*) new Flonum(getFlo("flo*", o1) * getFlo("flo*", o2)); }
Object *floDiv(Object *o1, Object *o2) { return (Object*) new Flonum(getFlo("flo/", o1) / getFlo("flo/", o2)); }
//TODO Object *floMod(Object *o1, Object *o2) { return (Object*) new Flonum(getFlo("flo%", o1) % getFlo("flo%", o2)); }
Object *floLt(Object *o1, Object *o2) { return (Object*) Boolean::valueOf(getFlo("flo<", o1) < getFlo("flo<", o2)); }
Object *floEq(Object *o1, Object *o2) { return (Object*) Boolean::valueOf(getFlo("flo=", o1) == getFlo("flo=", o2)); }
Object *eq(Object *o1, Object *o2) { return (Object*) Boolean::valueOf(o1 == o2); }

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

Object *sysStrToFix(Object *o1, Object *o2)
{
    assertType("str->fix", o1, otString);
    assertType("str->fix", o2, otFixnum);
    string strValue = ((String*)o1)->getValue();
    long base = ((Fixnum*)o2)->getValue();
    stringstream sb;
    switch(base)
    {
    case 2: error("str->fix: Base 2 not implemented yet"); return NULL; // TODO
    case 8: sb << oct; break;
    case 10: sb << dec; break;
    case 16: sb << hex; break;
    default: error("str->fix: Invalid base"); return NULL;
    }

    sb << strValue;
    long lValue;
    char c;
    if (sb >> lValue && !(sb.get(c))) return (Object*) new Fixnum(lValue);
    return Symbol::fromString("nan");
}

Object *sysFixToStr(Object *o1, Object *o2)
{
    stringstream sb;
    assertType("fix->str", o1, otFixnum);
    assertType("fix->str", o2, otFixnum);
    long base = ((Fixnum*)o2)->getValue();

    switch(base)
    {
    case 2: error("fix->str: Base 2 not implemented yet"); return NULL; // TODO
    case 8: sb << oct; break;
    case 10: sb << dec; break;
    case 16: sb << hex; break;
    default: error("fix->str: Invalid base"); return NULL;
    }

    sb << ((Fixnum*)o1)->getValue();
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
        _global.define("print-eval-forms", (Object*) Null::getInstance());

        DEFUN1(car, "car");
        DEFUN1(cdr, "cdr");
        DEFUN1(sysType, "type");
        DEFUN1(sysTag, "tag");
        DEFUN1(sysUntag, "untag");
        DEFUN1(integerToChar, "integer->char");
        DEFUN1(charToInteger, "char->integer");
        DEFUN1(stringLength, "string-length");
        DEFUN1(stringToSymbol, "string->symbol");
        DEFUN1(symbolToString, "symbol->string");
        DEFUN1(vectorLength, "vector-length");
        DEFUN1(makeString, "make-string");
        DEFUN1(makeVector, "make-vector");
        DEFUN1(sysDisplayString, "display-string");
        DEFUN1(sysExit, "exit");
        DEFUN1(sysStrToFlo, "str->flo");
        DEFUN1(sysFloToStr, "flo->str");
        DEFUN1(sysFixToFlo, "fix->flo");

        DEFUN2(cons, "cons");
        DEFUN2(setCar, "set-car!");
        DEFUN2(setCdr, "set-cdr!");
        DEFUN2(fixPlus, "fix+");
        DEFUN2(fixMinus, "fix-");
        DEFUN2(fixMult, "fix*");
        DEFUN2(fixDiv, "fix/");
        DEFUN2(fixLt, "fix<");
        DEFUN2(fixEq, "fix=");
        DEFUN2(fixMod, "fix%");
        DEFUN2(floPlus, "flo+");
        DEFUN2(floMinus, "flo-");
        DEFUN2(floMult, "flo*");
        DEFUN2(floDiv, "flo/");
        DEFUN2(floLt, "flo<");
        DEFUN2(floEq, "flo=");
//TODO        DEFUN2(floMod, "flo%");
        DEFUN2(eq, "eq?");
        DEFUN2(apply, "sys:apply");
        DEFUN2(stringRef, "string-ref");
        DEFUN2(vectorRef, "vector-ref");
        DEFUN2(sysStrToFix, "str->fix");
        DEFUN2(sysFixToStr, "fix->str");

        DEFUN3(stringSet, "string-set!");
        DEFUN3(vectorSet, "vector-set!");

        ifstream in("init.scm");
        evalAll(in);
        
        ifstream in2("init.scm");
        vector<int> str;
        while (in2) str.push_back((int) in2.get());
        _global.define("gaga", new String(str));
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
        if (_global.get("print-eval-forms")->getType() != otNull)
            cout << "evalExpandedForm: " << form->toString() << endl;

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
        for (;;) if (!expandMacros(obj)) break;
        Pair *asPair = (Pair*) *obj;
        if (asPair->_car->getType() != otSymbol) return;
        if (asPair->_car->toString() != "defmacro") return;

        if (asPair->_cdr->getType() != otPair) error("Invalid defmacro form: Expected (defmacro name (parameters) form ...)");
        if (((Pair*)asPair->_cdr)->_car->getType() != otSymbol) error("Invalid defmacro form: Name must be a symbol");
        string name = ((Pair*)asPair->_cdr)->_car->toString();
        if (((Pair*)asPair->_cdr)->_cdr->getType() != otPair) error("Invalid defmacro form");
        _macros[name] = (Lambda*) evalLambda((Pair*)((Pair*)asPair->_cdr), &_global);
        *obj = (Object*) Boolean::getTrue();
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
        catch(int message)
        {
        }
    }
    return 0;
}
