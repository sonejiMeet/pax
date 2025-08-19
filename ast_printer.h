#pragma once
#include "ast.h"
#include <iostream>
#include <iomanip>
#include <string>

inline std::string astNodeTypeToString(ASTNodeType type) {
    switch (type) {
        case AST_PROGRAM:     return "Program";
        case AST_VAR_DECL: return "VarDecl";
        case AST_FUNCTION_DECL: return "FuncDecl";
        case AST_STRUCT_DECL:  return "StructDef";

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
        case AST_EXPR_STMT: return "ExprStmt"; // not sure
        case AST_PRINT_STMT:  return "PrintStmt";

        case AST_UNKNOWN:     return "Unknown";
        default:              return "??";
    }
}

inline void printAST(const ASTNode* node, std::string indent = "", bool isLast = true) {
    if (!node) return;

    std::cout << indent;

    if (!indent.empty()) {
        if (isLast)
            std::cout << "`-- ";  // last child
        else
            std::cout << "|-- ";  // not last
    }

    // Print node type
    std::cout << astNodeTypeToString(node->type);

    // Print extra info only if relevant
    switch (node->type) {
        case AST_IDENTIFIER:
            std::cout << " (" << node->token.value << ")";
            break;
        case AST_NUMBER_LITERAL:
            std::cout << " (" << node->token.int_value << ")";
            break;
        case AST_STRING_LITERAL:
            std::cout << " (\""
                      << std::string((char*)node->token.string_value.data,
                                     node->token.string_value.count)
                      << "\")";
            break;
        default:
            if (node->token.value) {
                std::cout << " (\"" << node->token.value << "\")";
            }
            break;
    }

    std::cout << std::endl;

    // Recurse for children
    for (size_t i = 0; i < node->children.size(); i++) {
        printAST(node->children[i],
                 indent + (isLast ? "    " : "|   "),
                 i == node->children.size() - 1);
    }
}



// // a:= 4 + (5*2);
// printf   ("something");

// if(asdasd){

// }

// functionName (asda) : int {

// }

// struct structName{

// }

// ,

// /*
// some random stuff
//  */