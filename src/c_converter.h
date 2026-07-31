#pragma once

#include <functional> // these are temporary

struct Pax_Interp;

struct C_Converter {
    Pax_Interp *interp;
    Def_Type *_type;

    C_Converter(Pax_Interp *_interp);

    int expression_emit_depth = 0;
    int statement_emit_depth = 0;

    void emit_debug_info(FILE *out, Ast_Statement *stmt);
    void emit_debug_info(FILE *out, Ast_Expression *expr);

    void emitStatement(FILE *out, Ast_Statement *stmt, int indent = 0, bool is_else_if = false, Ast_Declaration *current_func = nullptr);
    void emitExpression(FILE *out, Ast_Expression *expr, int indent = 0, bool _struct = false);
    void emitBlock(FILE *out, Ast_Block *block, int indent = 0, Ast_Declaration *current_func = nullptr);

    Ast_Expression *get_call_argument_value(Ast_Expression *argument);
    Ast_Expression *find_call_argument(Ast_Procedure_Call_Expression *call, Ast_Declaration *parameter, int *next_positional_argument);
    Ast_Declaration *find_function_declaration(Ast_Procedure_Call_Expression *call);

    inline Ast_Array_Type *as_array_type(Ast_Type_Definition *t) {
        return (t && t->type == AST_ARRAY_TYPE) ? static_cast<Ast_Array_Type*>(t) : nullptr;
    }

    void type_to_c_string(FILE *out, Ast_Type_Definition *type, Ast_Declaration *decl, bool need_semicolon, int indent, bool should_initializer = false);

    void emitFunctionPrototype(FILE *out, Ast_Declaration *decl, int indent);

    const char *get_multi_ret_struct_name(Ast_Declaration *decl);

    void emit_function_return_spec(FILE *out, Ast_Declaration *decl, int indent);

    void emitStructPrototype(FILE *out, Ast_Statement *stmt, int indent);
    void emitStruct(FILE *out, Ast_Statement *stmt, int indent);

    void indentLine(FILE *out, int indent)
    {
        for (int i = 0; i < indent; ++i)
            fputc(' ', out);
    }

    Array<Ast_Statement*> topologically_sort_structs(Array<Ast_Statement*>& structs, Pool* pool);
    void emit_static_init_function(FILE *out,Array<Ast_Statement*> const &vars);
    void emit_struct_init_helper(FILE *out,  Ast_Statement *stmt);

    void emit_string_or_pointer(FILE *out, Ast_Expression *expr, Ast_Type_Definition *expected_ptr_type, int indent);
    
    void generate_cpp_code(const char *filename, Ast_Block *program);

};

