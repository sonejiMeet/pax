#pragma once

#include <string>

#include "pool.h"

struct Token;

struct Ast;
struct Ast_Statement;
struct Ast_Expression;
struct Ast_Comma_Separated_Args;
struct Ast_Function;

struct Ast_Literal;
struct Ast_Ident;
struct Ast_Procedure_Call_Expression;
struct Ast_Binary;
struct Ast_Unary;
struct Ast_Block;

struct Ast_If;
struct Ast_While;

struct Ast_Declaration;
struct Ast_Struct_Description;
struct Ast_Type_Definition;

enum Ast_Type {
    AST_UNKNOWN,
    AST_BLOCK,
    AST_IDENT,
    AST_STATEMENT,
    AST_EXPRESSION,
    AST_BINARY,
    AST_IF,
    AST_WHILE,
    AST_LITERAL,
    AST_UNARY,
    AST_DECLARATION,
    AST_PROCEDURE_CALL_EXPRESSION,
    AST_COMMA_SEPARATED_ARGS,
    AST_STRUCT_DESCRIPTION,
    AST_TYPE_DEFINITION,
};


inline std::string astTypeToString(Ast_Type type) {
    switch (type) {
        case AST_BLOCK:          return "Block";
        case AST_IDENT:          return "Identifier";
        case AST_STATEMENT:      return "Statement";
        case AST_EXPRESSION:     return "Expression";
        case AST_BINARY:         return "BinaryExpr";
        case AST_UNARY:         return "UnaryExpr";
        case AST_IF:             return "IfStmt";
        case AST_WHILE:             return "WhileStmt";
        case AST_LITERAL:        return "Literal";
        case AST_DECLARATION:    return "Declaration";
        case AST_PROCEDURE_CALL_EXPRESSION:    return "ProcCallExpr";
        case AST_COMMA_SEPARATED_ARGS: return "CommaSeparatedArg";
        case AST_STRUCT_DESCRIPTION: return "StructDescription";
        case AST_TYPE_DEFINITION: return "TypeDefinition";
        default:                 return "Unknown";
    }
}

struct Ast {
    Ast_Type type = AST_UNKNOWN;
    int line_number = 0;
    int character_number = 0;

    virtual ~Ast() = default;

    std::string getTypeString() const {
        return astTypeToString(type);
    }
};

struct Ast_Statement : public Ast {
    Ast_Statement(Pool* = nullptr) { type = AST_STATEMENT; }
    Ast_Type_Definition *type_definition = nullptr;
    Ast_Expression *expression = nullptr;
    Ast_Block *block = nullptr;
};

struct Ast_Expression : public Ast {
    Ast_Expression(Pool* = nullptr) { type = AST_EXPRESSION; }
    Ast_Type_Definition *inferred_type = nullptr;
};

struct Ast_Declaration : public Ast_Statement {
    Ast_Declaration(Pool* = nullptr) { type = AST_DECLARATION; }

    Ast_Type_Definition *declared_type = nullptr;
    Ast_Ident *identifier = nullptr;
    Ast_Expression *initializer = nullptr;
};

struct Ast_Comma_Separated_Args : public Ast_Expression {
    Ast_Comma_Separated_Args(Pool * p) :arguments(p) { type = AST_COMMA_SEPARATED_ARGS; }
    Array<Ast_Expression *> arguments;
};

struct Ast_Function : public Ast_Statement {
    Ast_Function(Pool* p) : params(p) { type = AST_STATEMENT; }

    const char* name = nullptr;

    Array<Ast_Declaration*> params;
    Ast_Block* body = nullptr;
    bool is_entry_point = false; // only true if 'main'

};

enum Value_Type {
    LITERAL_UNINITIALIZED,
    LITERAL_NUMBER,
    LITERAL_STRING,
    LITERAL_FLOAT,
    LITERAL_TRUE,
    LITERAL_FALSE,
};

struct Ast_Literal : public Ast_Expression {
    Ast_Literal(Pool* = nullptr) { type = AST_LITERAL; }

    Value_Type value_type = LITERAL_UNINITIALIZED;

    const char* string_value = nullptr;
    double float_value = 0;
    int64_t integer_value = 0;
};

struct Ast_Ident : public Ast_Expression {
    Ast_Ident(Pool* = nullptr) { type = AST_IDENT; }

    const char* name = nullptr;

};

struct Ast_Procedure_Call_Expression : public Ast_Expression {
    Ast_Procedure_Call_Expression (Pool* = nullptr) { type = AST_PROCEDURE_CALL_EXPRESSION; }

    Ast_Ident *function = nullptr;
    Ast_Comma_Separated_Args *arguments = nullptr;
};

enum Binary_Op {
    BINOP_UNKNOWN,
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_EQ,
    BINOP_NEQ,
    BINOP_ASSIGN,
};

struct Ast_Binary : public Ast_Expression {
    Ast_Binary(Pool* = nullptr) { type = AST_BINARY; }
    Binary_Op op = BINOP_UNKNOWN;
    Ast_Expression *lhs = nullptr;
    Ast_Expression *rhs = nullptr;
};

enum Ast_Unary_Op {
    UNARY_NEGATE,      // -x
    UNARY_NOT,         // !x
    UNARY_ADDRESS_OF,  // ^x  (address-of)
    UNARY_DEREFERENCE,  // *x  (pointer dereference)
    UNARY_REFERENCE,
};

struct Ast_Unary : Ast_Expression {
    Ast_Unary (Pool* = nullptr) { type = AST_UNARY; }
    Ast_Unary_Op op;
    Ast_Expression* operand = nullptr; // expression being operated on

};

struct Ast_Block : public Ast {
    Ast_Block(Pool* p) : statements(p) { type = AST_BLOCK; }

    Ast_Block *parent = nullptr;
    Array<Ast_Statement *> statements;

    bool is_scoped_block = false;
    bool is_entry_point = false;

};

struct Ast_If : public Ast_Statement {
    Ast_If(Pool* = nullptr) { type = AST_IF; }
    Ast_Expression *condition = nullptr;
    Ast_Block *then_block = nullptr;
    // Ast_If *else_if = nullptr; // not done yet
    Ast_Block *else_block = nullptr;
};

struct Ast_While : public Ast_Expression {  // not done yet
    Ast_While(Pool* = nullptr) { type = AST_WHILE; };
    Ast_Expression *condition = nullptr;
    Ast_Block *block = nullptr;
};


struct Ast_Struct_Description : public Ast_Expression {
    Ast_Struct_Description(Pool* p) : declarations_who_own_memory(p) { type = AST_STRUCT_DESCRIPTION; }

    const char* name = nullptr;

    Ast_Block *block = nullptr;

    Array<Ast_Declaration *> declarations_who_own_memory;

};
enum Ast_Builtin_Type {
    TYPE_UNKNOWN,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_BOOL,
    TYPE_STRING,
    TYPE_VOID,
    TYPE_STRUCT,
};

enum Array_Kind {
    ARRAY_NONE,      // Not an array
    ARRAY_DYNAMIC,   // []T
    ARRAY_STATIC,    // [N]T
};
struct Ast_Type_Definition : public Ast {
    Ast_Type_Definition(Pool* = nullptr) { type = AST_TYPE_DEFINITION; }

    Ast_Builtin_Type builtin_type = TYPE_UNKNOWN;

    Ast_Type_Definition *pointed_to_type = nullptr;

    Array_Kind array_kind = ARRAY_NONE;
    Ast_Type_Definition *element_type = nullptr; // for arrays
    int static_array_size = 0;

    bool is_reference = false;
    Ast_Struct_Description *struct_def = nullptr;

    std::string to_string() const {
        std::string base;
        switch (builtin_type) {
            case TYPE_INT:    base = "int"; break;
            case TYPE_FLOAT:  base = "float"; break;
            case TYPE_BOOL:   base = "bool"; break;
            case TYPE_STRING: base = "string"; break;
            case TYPE_VOID:   base = "void"; break;
            case TYPE_STRUCT:
                base = struct_def ? struct_def->name : "unknown struct";
                break;
            default: base =  "unknown";
        }

        return base;
    }
};
