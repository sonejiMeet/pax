#pragma once
#include <string>
#include <vector>

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
struct Ast_Block;

struct Ast_If;
struct Ast_While;

struct Ast_Type_Definition;
struct Ast_Declaration;


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
    AST_DECLARATION,
    AST_PROCEDURE_CALL_EXPRESSION,
    AST_COMMA_SEPARATED_ARGS,

};

inline std::string astTypeToString(Ast_Type type) {
    switch (type) {
        case AST_BLOCK:          return "Block";
        case AST_IDENT:          return "Identifier";
        case AST_STATEMENT:      return "Statement";
        case AST_EXPRESSION:     return "Expression";
        case AST_BINARY:         return "BinaryExpr";
        case AST_IF:             return "IfStmt";
        case AST_WHILE:             return "WhileStmt";
        case AST_LITERAL:        return "Literal";
        case AST_DECLARATION:    return "Declaration";
        case AST_PROCEDURE_CALL_EXPRESSION:    return "ProcCallExpr";
        case AST_COMMA_SEPARATED_ARGS: return "CommaSeparatedArg";
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

// Statements & Expressions
struct Ast_Statement : public Ast {
    Ast_Statement() { type = AST_STATEMENT; }
    struct Ast_Type_Definition *type_definition = nullptr;
    struct Ast_Expression *expression = nullptr;
    struct Ast_Block *block = nullptr;
};

struct Ast_Expression : public Ast {
    Ast_Expression() { type = AST_EXPRESSION; }
    Ast_Type_Definition *inferred_type = nullptr;
};

struct Ast_Comma_Separated_Args : public Ast_Expression {
    Ast_Comma_Separated_Args() { type = AST_COMMA_SEPARATED_ARGS; }
    std::vector<Ast_Expression *> arguments;
};

struct Ast_Function : public Ast_Statement {
    std::string name;          // e.g., "main"
    std::vector<Ast_Declaration*> params;
    Ast_Block* body = nullptr;
    bool is_entry_point = false; // true for 'main'
};

// Literals & Identifiers
enum Value_Type {
    LITERAL_UNINITIALIZED,
    LITERAL_NUMBER,
    LITERAL_STRING,
    LITERAL_FLOAT,
    LITERAL_TRUE,
    LITERAL_FALSE,
};

struct Ast_Literal : public Ast_Expression {
    Ast_Literal() { type = AST_LITERAL; }

    Value_Type value_type = LITERAL_UNINITIALIZED;
    std::string string_value;
    double float_value = 0;
    int64_t integer_value = 0;
};

struct Ast_Ident : public Ast_Expression {
    Ast_Ident() { type = AST_IDENT; }
    std::string name;
};

struct Ast_Procedure_Call_Expression : public Ast_Expression {
    Ast_Procedure_Call_Expression () { type = AST_PROCEDURE_CALL_EXPRESSION; }

    Ast_Ident *function = nullptr;
    Ast_Comma_Separated_Args *arguments = nullptr;
};

// Binary Expressions
enum Binary_Op {
    BINOP_UNKNOWN,
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_EQ,
    BINOP_NEQ,
    BINOP_ASSIGN,
    // more....
};

struct Ast_Binary : public Ast_Expression {
    Ast_Binary() { type = AST_BINARY; }
    Binary_Op op = BINOP_UNKNOWN;
    Ast_Expression *lhs = nullptr;
    Ast_Expression *rhs = nullptr;
};

// Blocks & Control Flow
struct Ast_Block : public Ast {
    Ast_Block() { type = AST_BLOCK; }
    std::vector<Ast_Statement *> statements;

    std::vector<Ast_Declaration *> members; // declarations in this scope
    std::vector<Ast_Block *> child_scopes;

    bool is_scoped_block = false;
    bool is_entry_point = false;
};

struct Ast_If : public Ast_Statement {
    Ast_If() { type = AST_IF; }
    Ast_Expression *condition = nullptr;
    Ast_Block *then_block = nullptr;
    // Ast_If *else_if = nullptr; // not done yet
    Ast_Block *else_block = nullptr;
};

struct Ast_While : public Ast_Expression {  // not done yet
    Ast_While() { type = AST_WHILE; };
    Ast_Expression *condition = nullptr;
    Ast_Block *block = nullptr;
};


// Declarations
enum Ast_Builtin_Type {
    TYPE_UNKNOWN,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_BOOL,
    TYPE_STRING,
    TYPE_VOID,
};

struct Ast_Type_Definition : public Ast {
    Ast_Builtin_Type builtin_type = TYPE_UNKNOWN;
    std::string name; // for user-defined types

    Ast_Type_Definition *element_type = nullptr; // for arrays
    bool is_array = false;

    // static Ast_Type_Definition make_builtin(Ast_Builtin_Type t) {
    //     Ast_Type_Definition def;
    //     def.builtin_type = t;
    //     return def;
    // }

    // static Ast_Type_Definition make_user_type(const std::string &n) {
    //     Ast_Type_Definition def;
    //     def.name = n;
    //     return def;
    // }

    std::string to_string() const {
        if (!name.empty()) return name;
        switch (builtin_type) {
            case TYPE_INT:    return "int";
            case TYPE_FLOAT:  return "float";
            case TYPE_BOOL:   return "bool";
            case TYPE_STRING: return "string";
            case TYPE_VOID:   return "void";
            default:          return "unknown";
        }
    }
};

struct Ast_Declaration : public Ast_Statement {
    Ast_Declaration() { type = AST_DECLARATION; }
    Ast_Type_Definition *declared_type = nullptr;
    Ast_Ident *identifier = nullptr;
    Ast_Expression *initializer = nullptr; // optional
};
