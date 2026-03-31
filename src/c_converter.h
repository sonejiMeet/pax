#pragma once

#include <map>
#include <set>
#include <functional> // these are temporary

struct Pax_Interp;

struct C_Converter {
    Pax_Interp *interp;
    Def_Type *_type;

    C_Converter(Pax_Interp *_interp);
    void emitStatement(FILE *out, Ast_Statement *stmt, int indent = 0, bool is_else_if = false);
    void emitExpression(FILE *out, Ast_Expression *expr, int indent = 0, bool _struct = false);
    void emitBlock(FILE *out, Ast_Block *block, int indent = 0);

    inline Ast_Array_Type *as_array_type(Ast_Type_Definition *t) {
        return (t && t->type == AST_ARRAY_TYPE) ? static_cast<Ast_Array_Type*>(t) : nullptr;
    }

    void type_to_c_string(FILE *out, Ast_Type_Definition *type, Ast_Declaration *decl, bool need_semicolon, int indent, bool should_initializer = false);

    void emitFunctionPrototype(FILE *out, Ast_Declaration *decl, int indent);

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

    void generate_cpp_code(const char *filename, Ast_Block *program);

};

