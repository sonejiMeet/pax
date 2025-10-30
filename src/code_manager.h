#pragma once
#include <vector> // Temporary

#include "ast.h"
#include "pool.h"

struct CM_Unresolved_Call {
    Ast_Procedure_Call_Expression* call;
    int line_number;
    int character_number;
};

struct CM_Unresolved_Variable {
    Ast_Ident* ident;
    int line_number;
    int character_number;
};
struct CM_Unresolved_Type {
    Ast_Declaration* decl;
    Ast_Type_Definition* base_type;
    int line_number;
    int character_number;
};

struct CM_Unresolved_Member_Access {
    Ast_Binary* dot_expr;
    int line_number;
    int character_number;
    Ast_Block *my_scope;
};

struct ReturnCheckResult {
    bool has_return;
    bool all_paths_return;
};

struct CodeManager {

    Pool *ast_pool;
    Array<Ast_Block *> scope_stack;

    std::vector<CM_Unresolved_Call> unresolved_calls;
    std::vector<CM_Unresolved_Variable> unresolved_vars;
    std::vector<CM_Unresolved_Type> unresolved_types;
    std::vector<CM_Unresolved_Member_Access> unresolved_member_accesses;

    Def_Type *_type;

    CodeManager(Pool *pool, Def_Type *type);
    Ast_Literal *make_integer_literal(long long value);

    int count_errors = 0;

    char *pool_strdup(Pool *pool, const char* str);

    template <typename T>
    void report_error(T type, const char *fmt, ...);
    void report_error(int row, int col, const char* fmt, ...);

    void push_scope();
    void pop_scope();

    bool is_function_parameter(const char* name);

    bool declare_variable(Ast_Declaration *decl, bool force_decl = false);
    bool declare_function(Ast_Declaration *decl);
    bool declare_struct(Ast_Statement* struct_stmt);

    template <typename T>  // Temporary we want to simplify where this is used to get rid of this
    T *ast_static_cast(Ast *node, Ast_Type type) {
        return node->type == type ? static_cast<T *>(node) : nullptr;
    }

    Ast_Declaration *lookup_symbol(const char *name, Ast_Block *scope = nullptr); // here scope is for the case where we can't rely on scope_stack.pop() when going through queued unresolved statements, we pass in the scope. 
    Ast_Declaration *lookup_symbol_current_scope(const char *name);

    ReturnCheckResult checkReturnPaths(Ast_Block *block);
    void checkFunctionReturns(Ast_Declaration *decl);
    bool has_return_statement(Ast_Block *block);
    bool all_paths_return(Ast_Block *block);

    void resolve_idents(Ast_Block *block);
    void resolve_idents_in_declaration(Ast_Declaration *decl);

    Ast_Declaration *resolve_member_access(Ast_Binary* dot_expr, Ast_Block* my_scope = nullptr);

    void resolve_idents_in_expr(Ast_Expression *expr);
    void resolve_unresolved_vars();
    void resolve_unresolved_calls();

    Ast_Type_Definition* find_struct_type_in_scopes(const char* name) const;

    void resolve_unresolved_types_queue();
    void resolve_unresolved_member_accesses_queue();


    char *type_to_string(Ast_Type_Definition *type);

    void infer_types_return(Ast_Statement *ret, Ast_Declaration *func_decl);
    void infer_types_expr(Ast_Expression **expr_ptr);

    bool check_that_types_fit(long long value, Ast_Type_Definition *target);
    bool check_that_types_fit(double value, Ast_Type_Definition *target);

    long long wrap_integer_to_type(long long value, Ast_Type_Definition *target);

    void infer_types_decl(Ast_Declaration *decl);
    void infer_types_block(Ast_Block *block, Ast_Declaration *my_func = nullptr);

    bool check_that_types_match(Ast_Type_Definition *wanted, Ast_Type_Definition *have, bool is_pointer = false);

};
