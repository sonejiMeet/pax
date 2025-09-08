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

    CM_Symbol* lookup_symbol_current_scope(const std::string& name);
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
