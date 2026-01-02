#pragma once
#include <vector> // Temporary

#include "ast.h"
#include "pool.h"

struct Pax_Interp;

struct Unresolved_Call {
    Ast_Procedure_Call_Expression *call;
    Ast_Block* my_scope;
};

struct Unresolved_Variable {
    Ast_Ident *ident;
    Ast_Block* my_scope;
};
struct Unresolved_Type {
    Ast_Declaration *decl;
    Ast_Type_Definition *base_type;
};

struct Unresolved_Member_Access {
    Ast_Binary *dot_expr = nullptr;
    Ast_Binary *assignment_expr = nullptr;
    Ast_Declaration *decl = nullptr;
    Ast_Block *my_scope;
};

struct Unresolved_Array_Type {
    Ast_Type_Definition *array_type;
    Ast_Declaration *decl;  // The declaration that uses this array
};

struct ReturnCheckResult {
    bool has_return;
    bool all_paths_return;
};

struct CodeManager {
    Pax_Interp* interp;

    Array<Ast_Block *> scope_stack;

    std::vector<Unresolved_Call> unresolved_calls;
    std::vector<Unresolved_Variable> unresolved_vars;
    std::vector<Unresolved_Type> unresolved_types;
    std::vector<Unresolved_Member_Access> unresolved_member_accesses;
    std::vector<Unresolved_Array_Type> unresolved_array_types;

    Def_Type *_type;

    CodeManager(Pax_Interp *_interp);

    Ast_Literal *make_integer_literal(long long value);

    int count_errors = 0;

    // to ensure we have resolved everything successfully after inference stage
    void is_everything_resolved(){   // for now we are not calling this yet
        bool should_exit = false;
        if(!unresolved_calls.empty()) {
            printf("unresolved_calls is not empty broo\n");
            should_exit = true;
        }
        if(!unresolved_vars.empty()) {
            printf("unresolved_vars is not empty broo\n");
            should_exit = true;
        }
        if(!unresolved_types.empty()) {
            printf("unresolved_types is not empty broo\n");
            should_exit = true;
        }
        if(!unresolved_member_accesses.empty()) {
            printf("unresolved_member_accesses is not empty broo\n");
            should_exit = true;
        }
        if(!unresolved_array_types.empty()) {
            printf("unresolved_array_types is not empty broo\n");
            should_exit = true;
        }
        if(should_exit == true) exit(1);

    }
    template <typename T>
    void report_error(T type, const char *fmt, ...);

    template<typename T, typename P>
    void report_error_with_previous(T node, P previous, const char* fmt, ...);

    void push_scope();
    void pop_scope();


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

    static inline Ast_Array_Type* as_array_type(Ast_Type_Definition* t) {
        return (t && t->type == AST_ARRAY_TYPE) ? static_cast<Ast_Array_Type*>(t) : nullptr;
    }

    static Ast_Type_Definition *get_base_type(Ast_Type_Definition *type);

    void resolve_idents(Ast_Block *block);

    void resolve_idents_in_declaration(Ast_Declaration *decl);
    void transform_array_to_struct(Ast_Type_Definition* type);

    Ast_Declaration *resolve_member_access(Ast_Binary* dot_expr, Ast_Block* my_scope = nullptr, bool skip_init_check =false, bool skip_queuing = false, bool should_infer = false);

    Ast_Type_Definition* clone_type_definition(Ast_Type_Definition* original);
    void create_type_instantiation(Ast_Type_Definition* type);

    Ast_Type_Definition* find_struct_type_in_scopes(const char* name) const;

    inline void push_unresolved_type(Ast_Declaration *decl, Ast_Type_Definition *base_type);
    inline void push_unresolved_member_access(Ast_Binary *dot_expr, Ast_Binary *assignment_expr = nullptr);
    inline void push_unresolved_call(Ast_Procedure_Call_Expression *call);

    void resolve_idents_in_expr(Ast_Expression *expr);

    void resolve_unresolved_vars();
    void resolve_unresolved_calls();
    void resolve_unresolved_types();
    void resolve_unresolved_member_accesses();
    void resolve_unresolved_array_types();

    char *type_to_string(Ast_Type_Definition *type);

    void infer_types_return(Ast_Statement *ret, Ast_Declaration *func_decl);
    void infer_types_expr(Ast_Expression **expr_ptr);

    bool check_that_types_fit(long long value, Ast_Type_Definition *target);
    bool check_that_types_fit(double value, Ast_Type_Definition *target);

    long long wrap_integer_to_type(long long value, Ast_Type_Definition *target);

    void infer_types_decl(Ast_Declaration *decl);
    void infer_types_block(Ast_Block *block, Ast_Declaration *my_func = nullptr);

    bool check_that_types_match(Ast_Type_Definition *wanted, Ast_Type_Definition *have, bool is_pointer = false);

    inline bool can_implicitly_convert_const(Ast_Expression *expr, Ast_Type_Definition *target);
    Ast_Type_Definition *extract_sizeof_type(Ast_Expression *expr);
    Ast_Type_Definition *resolve_type_by_name(const char *name);

};
