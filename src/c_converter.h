#pragma once

#include <vector>  
#include <map>
#include <set>
#include <functional> // these are temporary
#include <string>
#include "ast.h"

struct C_Converter {

    Def_Type *_type;

    C_Converter(Def_Type *type) : _type(type) {}
    void emitStatement(FILE* out, Ast_Statement* stmt, int indent = 0);
    void emitExpression(FILE* out, Ast_Expression* expr, int indent = 0, bool _struct = false);
    void emitBlock(FILE* out, Ast_Block* block, int indent = 0);

    void type_to_c_string(FILE *out, Ast_Type_Definition* type, Ast_Declaration *decl, bool need_semicolon, int indent);

    void emitFunctionPrototype(FILE* out, Ast_Declaration* decl, int indent);

    void emitStructPrototype(FILE* out, Ast_Statement* stmt, int indent);
    void emitStruct(FILE* out, Ast_Statement* stmt, int indent);

    void indentLine(FILE* out, int indent)
    {
        for (int i = 0; i < indent; ++i)
            fputc(' ', out);
    }

    std::vector<Ast_Statement*> topologically_sort_structs(const std::vector<Ast_Statement*>& structs);
    void generate_cpp_code(const char* filename, Ast_Block* program);

};

