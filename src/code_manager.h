#pragma once

struct Pax_Interp;

struct Unresolved_Call {
    Ast_Procedure_Call_Expression *call;
    Ast_Block *my_scope;
};

struct Unresolved_Variable {
    Ast_Ident *ident;
    Ast_Block *my_scope;
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

struct ReturnCheckResult {
    bool has_return;
    bool all_paths_return;
};

struct CodeManager {
    Pax_Interp *interp;

    Array<Ast_Block *> scope_stack;

    Array<Unresolved_Call*> unresolved_calls;
    Array<Unresolved_Variable*> unresolved_vars;
    Array<Unresolved_Type*> unresolved_types;
    Array<Unresolved_Member_Access*> unresolved_member_accesses;

    Def_Type *_type;

    CodeManager(Pax_Interp *_interp);

    Ast_Literal *make_integer_literal(long long value);

    int count_errors = 0;

    // to ensure we have resolved everything successfully after inference stage
    // @Temporary, this check is only during development process.
    void is_everything_resolved(){
        bool should_exit = false;
        if(unresolved_calls.count != 0) {
            printf("\nINTERNAL: unresolved_calls is not empty broo\n");
            should_exit = true;
        }
        if(unresolved_vars.count != 0) {
            printf("\nINTERNAL: unresolved_vars is not empty broo\n");
            should_exit = true;
        }
        if(unresolved_types.count != 0) {
            printf("\nINTERNAL: unresolved_types is not empty broo\n");
            should_exit = true;
        }
        if(unresolved_member_accesses.count != 0) {
            printf("\nINTERNAL: unresolved_member_accesses is not empty broo\n");
            should_exit = true;
        }
        if(should_exit == true) exit(1);

    }

    void report_error_impl(Ast *ast, const char *fmt, ...);
    void report_error_with_previous_impl(Ast *ast, Ast *previous, const char *fmt, ...);

    void push_scope();
    void pop_scope();


    bool declare_variable(Ast_Declaration *decl, bool force_decl = false);
    bool declare_function(Ast_Declaration *decl);
    bool declare_struct(Ast_Statement *struct_stmt);

    template <typename T>  // Temporary we want to simplify where this is used to get rid of this
    T *ast_static_cast(Ast *node, Ast_Type type) {
        return node->type == type ? static_cast<T *>(node) : nullptr;
    }

    Ast_Declaration *lookup_symbol(const char *name, Ast_Block *scope = nullptr); // here scope is for the case where we can't rely on scope_stack.pop() when going through queued unresolved statements, we pass in the scope.
    Ast_Declaration *lookup_symbol_current_scope(const char *name);
    Ast_Declaration *lookup_symbol_in_block(const char* name, Ast_Block* block);

    ReturnCheckResult checkReturnPathsIf(Ast_If *ifn);
    ReturnCheckResult checkReturnPaths(Ast_Block *block);
    void checkFunctionReturns(Ast_Declaration *decl);
    bool has_return_statement(Ast_Block *block);
    bool all_paths_return(Ast_Block *block);

    inline Ast_Array_Type *as_array_type(Ast_Type_Definition *t) {
        return (t && t->type == AST_ARRAY_TYPE) ? static_cast<Ast_Array_Type*>(t) : nullptr;
    }

    Ast_Type_Definition *get_base_type(Ast_Type_Definition *type);

    void resolve_idents(Ast_Block *block);

    void resolve_idents_if(Ast_If *ifn);

    void try_resolve_type_on_decl(Ast_Declaration *owner, Ast_Type_Definition *&ty);
    void resolve_idents_in_declaration(Ast_Declaration *decl);
    void transform_array_to_struct(Ast_Type_Definition *type);

    Ast_Ident *get_member_ident(Ast_Binary *dot_expr);

    Ast_Declaration *resolve_member_access(Ast_Binary *dot_expr, Ast_Block *my_scope = nullptr, bool skip_init_check =false, bool skip_queuing = false, bool should_infer = false);

    Ast_Type_Definition *clone_type_definition(Ast_Type_Definition *original);
    void create_type_instantiation(Ast_Type_Definition *type);

    Ast_Type_Definition *find_struct_type_in_scopes(const char *name) const;

    inline void push_unresolved_var(Ast_Ident *ident, Ast_Block *my_scope);
    inline void push_unresolved_type(Ast_Declaration *decl, Ast_Type_Definition *base_type);
    inline void push_unresolved_member_access(Ast_Binary *dot_expr, Ast_Binary *assignment_expr = nullptr);
    inline void push_unresolved_call(Ast_Procedure_Call_Expression *call);

    void resolve_idents_in_expr(Ast_Expression *expr, Ast_Block *my_scope = nullptr);

    void resolve_unresolved_vars();
    void resolve_unresolved_calls();
    void resolve_unresolved_types();
    void resolve_unresolved_member_accesses();

    void resolve_array_types(Ast_Array_Type *array_type, Ast_Declaration *decl);

    char *type_to_string(Ast_Type_Definition *type);

    void infer_types_return(Ast_Statement *ret, Ast_Declaration *func_decl);
    void infer_types_expr(Ast_Expression **expr_ptr);

    bool check_that_types_fit(long long value, Ast_Type_Definition *target);
    bool check_that_types_fit(double value, Ast_Type_Definition *target);

    long long wrap_integer_to_type(long long value, Ast_Type_Definition *target);

    void infer_types_decl(Ast_Declaration *decl);
    void infer_types_if(Ast_If *ifn, Ast_Declaration *my_func);
    void infer_types_block(Ast_Block *block, Ast_Declaration *my_func = nullptr);

    bool check_that_types_match(Ast_Type_Definition *wanted, Ast_Type_Definition *have, bool is_pointer = false);

    inline bool can_implicitly_convert_const(Ast_Expression *expr, Ast_Type_Definition *target);
    Ast_Type_Definition *extract_sizeof_type(Ast_Expression *expr);
    Ast_Type_Definition *resolve_type_by_name(const char *name);

    Ast_Expression *get_call_argument_value(Ast_Expression *argument);
    Ast_Declaration *find_call_parameter(Ast_Declaration *function_decl, const char *parameter_name);
    bool validate_call_arguments(Ast_Procedure_Call_Expression *call, Ast_Declaration *function_decl);

};
