#pragma once
#include <vector> // Temporary

#include "ast.h"

#include "pool.h"

struct CM_Symbol
{
    const char *name;
    Ast_Declaration* decl = nullptr;
    Ast_Type_Definition* type = nullptr; // explicit or inferred
    bool initialized = false;
    bool inferred = false;
};

using CM_Scope = std::vector<CM_Symbol>;

struct CodeManager
{
    std::vector<CM_Scope> scopes; // Temporary must replace with Array<>

    Pool* ast_pool;
    CodeManager(Pool* pool);

    int count_errors = 0;

    char *pool_strdup(Pool *pool, const char* str);

    void init();
    int get_count_errors();

    void report_error(int line, int col, const char* fmt, ...);

    void push_scope();
    void pop_scope();


    bool declare_variable(Ast_Declaration* decl);
    CM_Symbol* lookup_symbol(const char *name);

    CM_Symbol* lookup_symbol_current_scope(const char *name);
    void mark_initialized(const char *name);

    void resolve_idents(Ast_Block* block);
    void resolve_idents_in_declaration(Ast_Declaration* decl);
    void resolve_idents_in_expr(Ast_Expression* expr);

    Ast_Type_Definition* make_builtin_type(Ast_Builtin_Type t);


    Ast_Type_Definition* infer_types_expr(Ast_Expression** expr_ptr);
    void infer_types_decl(Ast_Declaration* decl);
    void infer_types_block(Ast_Block* block);

    bool check_that_types_match(Ast_Type_Definition* wanted, Ast_Type_Definition* have, bool is_pointer = false);

};
