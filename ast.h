#pragma once
#include <string>
#include <vector>

// Forward declare Token (from lexer)
struct Token;

enum ASTNodeType {
    AST_PROGRAM,           //  root of the AST
    AST_STATEMENT_LIST,    // A container for a sequence of statements

    AST_VAR_DECL,   // int x = 10;
    AST_FUNCTION_DECL,     //  function declarations
    AST_STRUCT_DECL,       //  struct type declarations

    AST_TYPE_SPECIFIER,

    // Expressions
    AST_BINARY_EXPR,       // `a + b`, `x == y`
    AST_UNARY_EXPR,        // `-x`, `!flag`
    AST_NUMBER_LITERAL,    // `123`
    AST_STRING_LITERAL,    //  `"hello"`
    AST_BOOLEAN_LITERAL,   //  `true`, `false`
    AST_IDENTIFIER,        //  `myVar`
    AST_ASSIGNMENT,        //  `x = value`
    AST_CALL_EXPR,         //  function or method calls `print("hi")`
    AST_MEMBER_ACCESS,     //  struct access  `obj.member`
    AST_ARRAY_ACCESS,      // `arr[index]`

    AST_PARENTHESIZED_EXPR, // ( a + b ) would be added as a parent node ( not sure if its a good idea to add a propiertary node since parenthesis does not actually hold that much of a value)

    // Statements
    AST_IF_STMT,
    AST_ELSE_CLAUSE,
    AST_BLOCK_STMT,   //  block of code in curly braces {  }
    AST_RETURN_STMT,  //  return statement in a function
    AST_EXPR_STMT,    // Node for an expression treated as a standalone statement (e.g., `x + 5;`)
    AST_PRINT_STMT,   // print keyword


    AST_UNKNOWN
};

inline std::string astNodeTypeToString(ASTNodeType type) {
    switch (type) {
        case AST_PROGRAM:     return "Program";
        case AST_STATEMENT_LIST: return "StatementList";
        case AST_VAR_DECL: return "VarDecl";
        case AST_FUNCTION_DECL: return "FuncDecl";
        case AST_STRUCT_DECL:  return "StructDef";

        case AST_TYPE_SPECIFIER: return "TypeSpecifier";

        case AST_BINARY_EXPR: return "BinaryExpr";
        case AST_UNARY_EXPR: return "UnaryExpr";
        case AST_NUMBER_LITERAL: return "Number";
        case AST_STRING_LITERAL: return "String";
        case AST_BOOLEAN_LITERAL: return "BoolLiter";
        case AST_IDENTIFIER:  return "Identifier";
        case AST_ASSIGNMENT: return "Assign";
        case AST_CALL_EXPR: return "CallExpr";
        case AST_MEMBER_ACCESS: return "MemberAccess";
        case AST_ARRAY_ACCESS: return "ArrAccess";

        case AST_PARENTHESIZED_EXPR: return "ParenthesizedExpr";
        
        case AST_IF_STMT:     return "IfStmt";
        case AST_ELSE_CLAUSE: return "ElseClause";
        case AST_BLOCK_STMT: return "BlockStmt";
        case AST_RETURN_STMT: return "ReturnStmt";
        case AST_EXPR_STMT: return "ExprStmt";
        case AST_PRINT_STMT:  return "PrintStmt";

        case AST_UNKNOWN:     return "Unknown";
        default:              return "??";
    }
}

struct ASTNode
{
    ASTNodeType type;
    Token token;
    std::vector<ASTNode*> children;

    ASTNode(ASTNodeType t, const Token& tok) : type(t), token(tok) {}

    ASTNode(ASTNodeType t) : type(t), token({}) // we have to initialize token to defualt
                                                // otherwise segfault !!!
     {}


    ~ASTNode() {
        for (auto* child : children) delete child;
    }

    std::string getTypeString() const {
        return astNodeTypeToString(type);
    }
};
