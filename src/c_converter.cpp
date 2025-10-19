#include "ast.h"
#include "c_converter.h"

#include <cstdio>
#include <string>
#include <vector>

const char *BOILTERPLATE_TOP =
    "/* GENERATED FILE */\n\n"
    "#include <stdlib.h>\n"
    "#include <stdio.h>\n"
    "typedef unsigned long long u64;\n"
    "typedef unsigned int       u32;\n"
    "typedef unsigned short     u16;\n"
    "typedef unsigned char      u8;\n"
    "typedef long long  s64;\n"
    "typedef int        s32;\n"
    "typedef short      s16;\n"
    "typedef char       s8;\n"
    "typedef float      float32;\n"
    "typedef double     float64;\n"
    "\n"
;

void C_Converter::emitExpression(FILE* out, Ast_Expression* expr, int indent)
{
    if (!expr) return;

    switch (expr->type) {
        case AST_LITERAL: {
            auto* lit = static_cast<Ast_Literal*>(expr);
            switch (lit->value_type) {
                case LITERAL_NUMBER: fprintf(out, "%lld", lit->integer_value); break;
                case LITERAL_FLOAT:  fprintf(out, "%.17f", lit->float_value); break;
                case LITERAL_STRING: fprintf(out, "\"%s\"", lit->string_value); break;
                case LITERAL_TRUE: {
                    char *s = (char *)"true";
                    fprintf(out, "%s",s);
                    break;
                }
                case LITERAL_FALSE: {
                    char *s = (char *)"false";
                    fprintf(out, "%s",s);
                    break;
                }
                default: fprintf(out, "/* unknown literal */"); break;
            }
            break;
        }
        case AST_UNARY: {
            Ast_Unary* u = static_cast<Ast_Unary*>(expr);

            switch (u->op) {
                case UNARY_ADDRESS_OF:
                    fprintf(out, "/*ADDRESS_OF*/ &");
                    emitExpression(out, u->operand, indent);
                    break;

                case UNARY_DEREFERENCE:
                    fprintf(out, "/*DEREF*/ * ");
                    emitExpression(out, u->operand, indent);
                    break;

                case UNARY_NEGATE:
                    fprintf(out, "-");
                    emitExpression(out, u->operand, indent);
                    break;

                case UNARY_NOT:
                    fprintf(out, "!");
                    emitExpression(out, u->operand, indent);
                    break;
            }
            break;
        }

        case AST_IDENT: {
            auto* ident = static_cast<Ast_Ident*>(expr);
            fprintf(out, "%s", ident->name);
            break;
        }

        case AST_BINARY: {
            auto* bin = static_cast<Ast_Binary*>(expr);
            fprintf(out, "("); // we will enclose binary expression in parenthesis to prove our operator precedence does work correctly and we are not just copy pasting expressions to c code
            emitExpression(out, bin->lhs, indent);
            switch (bin->op) {
                case BINOP_ADD: fprintf(out, " + "); break;
                case BINOP_SUB: fprintf(out, " - "); break;
                case BINOP_MUL: fprintf(out, " * "); break;
                case BINOP_DIV: fprintf(out, " / "); break;
                case BINOP_EQ:  fprintf(out, " == "); break;
                case BINOP_NEQ: fprintf(out, " != "); break;
                case BINOP_ASSIGN: fprintf(out, " = "); break;
                case BINOP_LESS: fprintf(out, " < "); break;
                case BINOP_GREATER: fprintf(out, " > "); break;
                case BINOP_LESS_EQUAL: fprintf(out, " <= "); break;
                case BINOP_GREATER_EQUAL: fprintf(out, " >= "); break;

                default: fprintf(out, "/*BINOP OP ERROR*/"); break;
            }
            emitExpression(out, bin->rhs, indent);
            fprintf(out, ")");
            break;
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            auto* call = static_cast<Ast_Procedure_Call_Expression*>(expr);
            fprintf(out, "%s(", call->function->name);
            if (call->arguments)
            {
                bool first = true;
                for (int i = 0; i < call->arguments->arguments.count; i++)
                {
                    Ast_Expression* arg = call->arguments->arguments.data[i];

                    if (!first) fprintf(out, ",");
                    emitExpression(out, arg, indent);
                    first = false;
                }
            }
            fprintf(out, ")");
            break;
        }

        default:
            fprintf(out, "/* unhandled expression */");
            break;
    }
}


void C_Converter::type_to_c_string(FILE *out, Ast_Type_Definition* type, Ast_Declaration *decl, bool need_semicolon, int indent) {
    if (!type) return;

    std::string type_str;
    std::string array_suffix;
    Ast_Type_Definition* base_type = type;
    int pointer_depth = 0;


    while (base_type->pointed_to_type) {
        base_type = base_type->pointed_to_type;
        pointer_depth++;
    }


    if (type->array_kind == ARRAY_STATIC && type->element_type) {
        type_str = type->element_type->to_string(*_type);
        array_suffix = "[" + std::to_string(type->static_array_size) + "]";

    } else {
        type_str = base_type->to_string(*_type);
    }

    for (int i = 0; i < pointer_depth; ++i) {
        type_str += " *";
    }

    fprintf( out, "%s %s%s", type_str.c_str(), decl->identifier->name, array_suffix.c_str());

     if (decl->initializer) {
        fprintf(out, " = ");
        emitExpression(out, decl->initializer, indent);
     }

    if(need_semicolon == true)
        fprintf(out, ";\n");

}

void C_Converter::emitFunctionPrototype(FILE* out, Ast_Declaration* decl, int indent) {
    if (!decl || !decl->is_function || !decl->identifier) return;

    indentLine(out, indent);
    type_to_c_string(out, decl->return_type, decl, false, indent);
    fprintf(out, "(");
    for (int i = 0; i < decl->parameters.count; ++i) {
        auto* param = decl->parameters.data[i];
        if (i > 0) fprintf(out, ", ");
        type_to_c_string(out, param->declared_type, param, false, indent);
    }
    if (decl->parameters.count == 0) {
        fprintf(out, "void");
    }
    fprintf(out, ");\n");
}


void C_Converter::emitStatement(FILE* out, Ast_Statement* stmt, int indent)
{
    if (!stmt) return;

    switch (stmt->type) {

        case AST_DECLARATION: {

            auto* decl = static_cast<Ast_Declaration*>(stmt);

            if (decl->is_function) {
                fprintf(out, "\n");
                indentLine(out, indent);

                // emit return type
                type_to_c_string(out, decl->return_type, decl, false, indent);

                fprintf(out, " (");

                // emit params
                for (int i = 0; i < decl->parameters.count; ++i) {
                    auto* param = decl->parameters.data[i];

                    type_to_c_string(out, param->declared_type, param, false, indent);

                    if (i + 1 < decl->parameters.count)
                        fprintf(out, ", ");
                }

                fprintf(out, ")");

                if (decl->is_function_body && decl->my_scope) {
                    fprintf(out, " ");
                    emitBlock(out, decl->my_scope, indent);
                    fprintf(out, "\n");
                } else {
                    fprintf(out, ";\n");
                }

                break;
            }

            indentLine(out, indent);

            type_to_c_string(out, decl->declared_type, decl, true, indent);

            break;
        }

        case AST_STATEMENT: {
            if (stmt->expression) {
                indentLine(out, indent);
                if(stmt->is_return == true){
                    fprintf(out, "return ");
                }
                emitExpression(out, stmt->expression, indent);

                fprintf(out, ";\n");
            }
            else if (stmt->block) {
                indentLine(out, indent);
                emitBlock(out, stmt->block, indent);
            }
            // else {
            //     indentLine(out, indent);
            //     fprintf(out, "/* Reached neither */\n");
            // }
            break;
        }


        case AST_IF: {
            auto* ifstmt = static_cast<Ast_If*>(stmt);
            indentLine(out, indent);
            fprintf(out, "if(");
            emitExpression(out, ifstmt->condition, indent);
            fprintf(out, ")");
            emitBlock(out, ifstmt->then_block, indent);

            if (ifstmt->else_block) {
                indentLine(out, indent);
                fprintf(out, "else ");
                emitBlock(out, ifstmt->else_block, indent);
            }
            break;
        }

        default:
            indentLine(out, indent);
            fprintf(out, "// [unhandled statement]\n");
            break;
    }
}

void C_Converter::emitBlock(FILE* out, Ast_Block* block, int indent)
{
    if (!block) return;

    fprintf(out, "{\n");

    for (int i = 0; i < block->statements.count; i++) {
        Ast_Statement* stmt = block->statements.data[i];

        emitStatement(out, stmt, indent+4);
    }


    indentLine(out, indent);
    fprintf(out, "}\n");
}

void C_Converter::generate_cpp_code(const char* filename, Ast_Block* program)
{
    FILE* out = nullptr;

#ifdef _WIN32
    fopen_s(&out, filename, "w");
#elif __linux
    out = fopen64(filename, "w");
#endif
    if (!out) {
        printf("Failed to open file: %s\n", filename);
        return;
    }

    fprintf(out, "%s", BOILTERPLATE_TOP);

    std::vector<Ast_Declaration*> functions; // TEMPORARY
    for (int i = 0; i < program->statements.count; i++) {
        Ast_Statement* stmt = program->statements.data[i];
        if (!stmt) continue;
        if (stmt->type == AST_DECLARATION) {
            Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
            if (decl->is_function && decl->is_function_body) {
                functions.push_back(decl);
            }
        }
    }

    fprintf(out, "/*GLOBAL FUNCTION FORWARD DECLARATIONS*/\n");
    for (Ast_Declaration* decl : functions) {
        emitFunctionPrototype(out, decl, 0);
    }
    fprintf(out, "\n");


    // Normal declarations
    for (int i = 0; i < program->statements.count; i++) {
        Ast_Statement* stmt = program->statements.data[i];

        if (!stmt) continue;

        if (stmt->type == AST_DECLARATION) {
            emitStatement(out, stmt, 0);
        }
    }

    Ast_Block* mainBlock = nullptr;

    for (int i = 0; i < program->statements.count; i++) {
        Ast_Statement* stmt = program->statements.data[i];

        if (stmt && stmt->block && stmt->block->is_entry_point) {
            mainBlock = stmt->block;
            break;
        }
    }

    if (!mainBlock) {
        printf("No main block found in AST\n");
        fclose(out);
        return;
    }

    fprintf(out, "\nvoid GENERATED_MAIN()");
    emitBlock(out, mainBlock, 0);

    fprintf(out, "\nint main(int argc, char **argv){\n");
    fprintf(out, "    GENERATED_MAIN();\n");
    fprintf(out, "    return 0;\n");
    fprintf(out, "}\n");

    fclose(out);
}
