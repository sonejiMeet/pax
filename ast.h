#pragma once
#include <string>
#include <vector>

// Forward declare Token (from lexer)
struct Token;

enum ASTNodeType {
    AST_PROGRAM,           // The root of the AST
    AST_STATEMENT_LIST,    // A container for a sequence of statements

    AST_VAR_DECL,   // int x = 10;
    AST_FUNCTION_DECL,     // Node for function declarations
    AST_STRUCT_DECL,       // Node for struct type declarations

    AST_TYPE_SPECIFIER,
    
    // Expressions
    AST_BINARY_EXPR,       // Node for binary operations (e.g., `a + b`, `x == y`)
    AST_UNARY_EXPR,        // Node for unary operations (e.g., `-x`, `!flag`)
    AST_NUMBER_LITERAL,    // Node for numeric constants (e.g., `123`)
    AST_STRING_LITERAL,    // Node for string constants (e.g., `"hello"`)
    AST_BOOLEAN_LITERAL,   // Node for boolean constants (e.g., `true`, `false`)
    AST_IDENTIFIER,   // Node representing a reference to an identifier (e.g., `myVar`)
    AST_ASSIGNMENT,   // Node for assignment operations (e.g., `x = value`)
    AST_CALL_EXPR,         // Node for function or method calls (e.g., `print("hi")`)
    AST_MEMBER_ACCESS,     // Node for accessing members of a struct/object (e.g., `obj.member`)
    AST_ARRAY_ACCESS,      // Node for accessing elements in an array (e.g., `arr[index]`)

    // Statements
    AST_IF_STMT,      // Node for an if-else control flow statement
    AST_ELSE_CLAUSE,       // Node for the 'else' part of an if statement (can be optional)
    AST_BLOCK_STMT,   // Node representing a block of code enclosed in curly braces { ... }
    AST_RETURN_STMT,  // Node for a return statement in a function
    AST_EXPR_STMT,    // Node for an expression treated as a standalone statement (e.g., `x + 5;`)
    AST_PRINT_STMT,   // Specific AST node for your 'print' keyword

    // Special
    AST_UNKNOWN,           // For unexpected or unhandled AST node types
};

inline std::string astNodeTypeToString(ASTNodeType type) {
    switch (type) {
        case AST_PROGRAM:     return "Program";
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
