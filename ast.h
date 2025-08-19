#pragma once
#include <string>
#include <vector>

// Forward declare Token (from lexer)
struct Token;

enum ASTNodeType {
    // Program structure
    AST_PROGRAM,           // The root of the AST, representing the entire program
    AST_STATEMENT_LIST,    // A container for a sequence of statements

    // Declarations
    AST_VAR_DECL,   // Node for variable declarations (e.g., `var x = 10;`)
    AST_FUNCTION_DECL,     // Node for function declarations
    AST_STRUCT_DECL,       // Node for struct type declarations

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


struct ASTNode
{
    ASTNodeType type;
    Token token;
    std::vector<ASTNode*> children;

    ASTNode(ASTNodeType t, const Token& tok)
        : type(t), token(tok) {}

    ~ASTNode() {
        for (auto* child : children) delete child;
    }
};
