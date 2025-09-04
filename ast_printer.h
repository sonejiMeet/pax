#pragma once
#include "ast.h"
#include <iostream>

// Forward declarations
struct Ast;
struct Ast_Block;
struct Ast_Statement;
struct Ast_Expression;
struct Ast_Literal;
struct Ast_Ident;
struct Ast_Binary;
struct Ast_If;
struct Ast_Declaration;

// Helper: indent printing
inline void printIndent(int indent) {
    for (int i = 0; i < indent; ++i) std::cout << "  ";
}

// Forward declarations
void printAst(const Ast* node, int indent = 0);
void printStatement(const Ast_Statement* stmt, int indent);
void printExpression(const Ast_Expression* expr, int indent);
void printLiteral(const Ast_Literal* lit, int indent);
void printIdent(const Ast_Ident* ident, int indent);
void printBinary(const Ast_Binary* bin, int indent);
void printBlock(const Ast_Block* block, int indent);
void printIf(const Ast_If* ifnode, int indent);
void printDeclaration(const Ast_Declaration* decl, int indent);

// ==================================================
// Generic dispatcher
// ==================================================
void printAst(const Ast* node, int indent) {
    if (!node) return;

    switch (node->type) {
        case AST_STATEMENT:      printStatement(static_cast<const Ast_Statement*>(node), indent); break;
        case AST_EXPRESSION:     printExpression(static_cast<const Ast_Expression*>(node), indent); break;
        case AST_LITERAL:        printLiteral(static_cast<const Ast_Literal*>(node), indent); break;
        case AST_IDENT:          printIdent(static_cast<const Ast_Ident*>(node), indent); break;
        case AST_BINARY:         printBinary(static_cast<const Ast_Binary*>(node), indent); break;
        case AST_BLOCK:          printBlock(static_cast<const Ast_Block*>(node), indent); break;
        case AST_IF:             printIf(static_cast<const Ast_If*>(node), indent); break;
        case AST_DECLARATION:    printDeclaration(static_cast<const Ast_Declaration*>(node), indent); break;

        default:
            printIndent(indent);
            std::cout << "Unknown AST Node" << std::endl;
            break;
    }
}

// ==================================================
// Statements
// ==================================================
void printStatement(const Ast_Statement* stmt, int indent) {
    if (!stmt) return;

    // Dispatch based on concrete type
    switch (stmt->type) {
        case AST_DECLARATION:
            printDeclaration(static_cast<const Ast_Declaration*>(stmt), indent);
            break;
        case AST_IF:
            printIf(static_cast<const Ast_If*>(stmt), indent);
            break;
        case AST_STATEMENT: // generic statement (likely an expression statement)
            printIndent(indent);
            std::cout << "Expression Statement" << std::endl;
            if (stmt->expression) printExpression(stmt->expression, indent + 1);
            if (stmt->block) printBlock(stmt->block, indent + 1);
            break;
        default:
            printIndent(indent);
            std::cout << "Unknown Statement" << std::endl;
            break;
    }
}


// ==================================================
// Expressions
// ==================================================
void printExpression(const Ast_Expression* expr, int indent) {
    if (!expr) return;
    switch (expr->type) {
        case AST_LITERAL:    printLiteral(static_cast<const Ast_Literal*>(expr), indent); break;
        case AST_IDENT:      printIdent(static_cast<const Ast_Ident*>(expr), indent); break;
        case AST_BINARY:     printBinary(static_cast<const Ast_Binary*>(expr), indent); break;
        default:
            printIndent(indent);
            std::cout << "Expression (unknown type)" << std::endl;
            break;
    }
}

// ==================================================
// Literals
// ==================================================
void printLiteral(const Ast_Literal* lit, int indent) {
    printIndent(indent);
    std::cout << "Literal: ";
    switch (lit->value_type) {
        case LITERAL_NUMBER:  std::cout << lit->integer_value; break;
        case LITERAL_FLOAT:   std::cout << lit->float_value; break;
        case LITERAL_STRING:  std::cout << "\"" << lit->string_value << "\""; break;
        default:              std::cout << "(uninitialized)"; break;
    }
    std::cout << std::endl;
}

// ==================================================
// Identifiers
// ==================================================
void printIdent(const Ast_Ident* ident, int indent) {
    printIndent(indent);
    std::cout << "Identifier: " << ident->name << std::endl;
}

// ==================================================
// Binary Expressions
// ==================================================
void printBinary(const Ast_Binary* bin, int indent) {
    printIndent(indent);
    std::cout << "Binary Expression: ";

    switch (bin->op) {
        case BINOP_ADD:  std::cout << "+"; break;
        case BINOP_SUB:  std::cout << "-"; break;
        case BINOP_MUL:  std::cout << "*"; break;
        case BINOP_DIV:  std::cout << "/"; break;
        case BINOP_EQ:   std::cout << "=="; break;
        case BINOP_NEQ:  std::cout << "!="; break;
        case BINOP_ASSIGN: std::cout << "="; break;
        default:         std::cout << "(unknown)"; break;
    }
    std::cout << std::endl;

    if (bin->lhs) printExpression(bin->lhs, indent + 1);
    if (bin->rhs) printExpression(bin->rhs, indent + 1);
}

// ==================================================
// Blocks
// ==================================================
void printBlock(const Ast_Block* block, int indent) {
    printIndent(indent);
    std::cout << "Block {" << std::endl;
    for (auto* stmt : block->statements) {
        printStatement(stmt, indent + 1);
    }
    printIndent(indent);
    std::cout << "}" << std::endl;
}

// ==================================================
// If Statements
// ==================================================
void printIf(const Ast_If* ifnode, int indent) {
    printIndent(indent);
    std::cout << "If Statement" << std::endl;

    printIndent(indent + 1); std::cout << "Condition:" << std::endl;
    printExpression(ifnode->condition, indent + 2);

    printIndent(indent + 1); std::cout << "Then:" << std::endl;
    printBlock(ifnode->then_block, indent + 2);

    if (ifnode->else_block) {
        printIndent(indent + 1); std::cout << "Else:" << std::endl;
        printBlock(ifnode->else_block, indent + 2);
    }
}

// ==================================================
// Declarations
// ==================================================
void printDeclaration(const Ast_Declaration* decl, int indent) {
    printIndent(indent);
    std::cout << "Declaration: ";
    if (decl->identifier) std::cout << decl->identifier->name;
    if (decl->declared_type) std::cout << " : " << decl->declared_type->to_string();
    std::cout << std::endl;

    if (decl->initializer) {
        printIndent(indent + 1); std::cout << "Initializer:" << std::endl;
        printExpression(decl->initializer, indent + 2);
    }
}
