// // code_manager.cpp
// #include "ast.h"
// #include <iostream>
// #include <vector>
// #include <string>

// // Very small Var info
// struct VarInfo {
//     std::string name;
// };

// // Simple procedural CodeManager inside a struct (no C++ classes needed)
// struct CodeManager {
//     std::vector<VarInfo> variables;

//     // declare a variable (prints error on redeclare)
//     void declareVar(const std::string& name, int line, int col) {
//         for (auto &v : variables) {
//             if (v.name == name) {
//                 std::cerr << "Error (" << line << ":" << col << "): variable '" << name
//                           << "' already declared in this scope.\n";
//                 return;
//             }
//         }
//         variables.push_back({name});
//     }

//     // check if declared
//     bool isDeclared(const std::string& name) {
//         for (auto &v : variables) {
//             if (v.name == name) return true;
//         }
//         return false;
//     }

//     // helper to check an identifier AST node
//     void checkIdent(Ast_Ident* id) {
//         if (!id) return;
//         if (!isDeclared(id->name)) {
//             std::cerr << "Error (" << id->line_number << ":" << id->character_number
//                       << "): variable '" << id->name << "' not declared.\n";
//         }
//     }

//     // Recursively analyze expressions
//     void analyzeExpression(Ast_Expression* expr) {
//         if (!expr) return;

//         switch (expr->type) {
//             case AST_IDENT: {
//                 auto *ident = static_cast<Ast_Ident*>(expr);
//                 checkIdent(ident);
//                 break;
//             }

//             case AST_LITERAL:
//                 // nothing to check for literals
//                 break;

//             case AST_BINARY: {
//                 auto *bin = static_cast<Ast_Binary*>(expr);
//                 if (bin->lhs) analyzeExpression(bin->lhs);   // use lhs
//                 if (bin->rhs) analyzeExpression(bin->rhs);   // use rhs
//                 break;
//             }

//             case AST_PROCEDURE_CALL_EXPRESSION: {
//                 auto *call = static_cast<Ast_Procedure_Call_Expression*>(expr);
//                 // Do not require the called function to be declared here (e.g. builtin printf).
//                 // If you want to require it, you can check call->function->name via isDeclared.
//                 if (call->arguments) {
//                     for (auto *arg : call->arguments->arguments) {
//                         analyzeExpression(arg);
//                     }
//                 }
//                 break;
//             }

//             // Add more expression kinds as you implement them
//             default:
//                 break;
//         }
//     }

//     // Analyze a single statement
//     void analyzeStatement(Ast_Statement* stmt) {
//         if (!stmt) return;

//         // First handle explicit declaration node (VarDecl)
//         if (stmt->type == AST_DECLARATION) {
//             auto *decl = static_cast<Ast_Declaration*>(stmt);

//             if (!decl->identifier) {
//                 std::cerr << "Error (" << decl->line_number << ":" << decl->character_number
//                           << "): declaration missing identifier.\n";
//             } else {
//                 // declare variable (then analyze initializer if present)
//                 declareVar(decl->identifier->name, decl->line_number, decl->character_number);

//                 if (decl->initializer) {
//                     analyzeExpression(decl->initializer);
//                 }
//             }
//             return;
//         }

//         // Generic statement may contain an expression or a nested block
//         if (stmt->expression) {
//             analyzeExpression(stmt->expression);
//         }
//         if (stmt->block) {
//             analyzeBlock(stmt->block);
//         }
//     }

//     // Analyze a block (flat -- no nested scoping here yet)
//     void analyzeBlock(Ast_Block* block) {
//         if (!block) return;
//         for (auto *stmt : block->statements) {
//             analyzeStatement(stmt);
//         }
//     }
// };

#pragma once
#include <string>
#include <vector>

#include "ast.h" // your AST header

// Forward declare AST node types (if needed)
struct Ast_Block;
struct Ast_Statement;
struct Ast_Expression;
struct Ast_Declaration;
struct Ast_Ident;
struct Ast_Type_Definition;
struct Ast_Procedure_Call_Expression;
struct Ast_Comma_Seperated_Args;
struct Ast_Literal;
struct Ast_Binary;
struct Ast_If;

// Symbol entry
struct CM_Symbol {
    std::string name;
    Ast_Declaration* decl = nullptr;
    Ast_Type_Definition* type = nullptr; // explicit or inferred type
    bool initialized = false;
    bool inferred = false;
};

// One scope is a vector of symbols
using CM_Scope = std::vector<CM_Symbol>;

// Code manager (procedural style, but grouped in a struct)
struct CodeManager {
    std::vector<CM_Scope> scopes;
    // std::vector<std::string> errors;
    int count_errors = 0;
    // lifecycle
    void init();              // push global scope
    // bool has_errors() const;
    // void print_errors() const;
    int get_count_errors();
    // scope management
    void push_scope();
    void pop_scope();

    // symbol operations
    bool declare_variable(Ast_Declaration* decl); // returns false on redeclare
    CM_Symbol* lookup_symbol(const std::string& name); // searches scopes (innermost first)
    void mark_initialized(const std::string& name);

    // resolution & inference passes
    void resolve_idents(Ast_Block* block); // resolution pass (undeclared idents)
    void resolve_idents_in_declaration(Ast_Declaration* decl);
    void resolve_idents_in_expr(Ast_Expression* expr);

    // type inference/checking
    Ast_Type_Definition* infer_types_expr(Ast_Expression** expr_ptr);
    void infer_types_decl(Ast_Declaration* decl);
    void infer_types_block(Ast_Block* block);

    // helpers
    void report_error(int line, int col, const char* fmt, ...);
    bool check_that_types_match(Ast_Type_Definition* wanted, Ast_Type_Definition* have);
    bool is_integer_type(Ast_Type_Definition* type);
};
