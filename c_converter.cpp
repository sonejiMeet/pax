#include "ast.h"
#include <cstdio>
#include <string>

void indentLine(FILE* out, int indent) {
    for (int i = 0; i < indent; ++i) fputc(' ', out);
}

void emitStatement(FILE* out, Ast_Statement* stmt, int indent = 0);
void emitExpression(FILE* out, Ast_Expression* expr, int indent = 0);
void emitBlock(FILE* out, Ast_Block* block, int indent = 0);

void emitExpression(FILE* out, Ast_Expression* expr, int indent) {
    if (!expr) return;

    switch (expr->type) {
        case AST_LITERAL: {
            auto* lit = static_cast<Ast_Literal*>(expr);
            switch (lit->value_type) {
                case LITERAL_NUMBER: fprintf(out, "%lld", lit->integer_value); break;
                case LITERAL_FLOAT:  fprintf(out, "%f", lit->float_value); break;
                case LITERAL_STRING: fprintf(out, "\"%s\"", lit->string_value.c_str()); break;
                default: fprintf(out, "/* unknown literal */"); break;
            }
            break;
        }

        case AST_IDENT: {
            auto* ident = static_cast<Ast_Ident*>(expr);
            fprintf(out, "%s", ident->name.c_str());
            break;
        }

        case AST_BINARY: {
            auto* bin = static_cast<Ast_Binary*>(expr);
            fprintf(out, "("); // we will enclose binary expression in parenthesis to prove our precedence work correctly and we are not just copy pasting expression to c code
            emitExpression(out, bin->lhs, indent);
            switch (bin->op) {
                case BINOP_ADD: fprintf(out, " + "); break;
                case BINOP_SUB: fprintf(out, " - "); break;
                case BINOP_MUL: fprintf(out, " * "); break;
                case BINOP_DIV: fprintf(out, " / "); break;
                case BINOP_EQ:  fprintf(out, " == "); break;
                case BINOP_NEQ: fprintf(out, " != "); break;
                default: fprintf(out, " ? "); break;
            }
            emitExpression(out, bin->rhs, indent);
            fprintf(out, ")");
            break;
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            auto* call = static_cast<Ast_Procedure_Call_Expression*>(expr);
            fprintf(out, "%s(", call->function->name.c_str());
            if (call->arguments) {
                bool first = true;
                for (auto* arg : call->arguments->arguments) {
                    if (!first) fprintf(out, ", ");
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

void emitStatement(FILE* out, Ast_Statement* stmt, int indent) {
    if (!stmt) return;

    switch (stmt->type) {
        case AST_DECLARATION: {
            auto* decl = static_cast<Ast_Declaration*>(stmt);
            indentLine(out, indent);
            fprintf(out, "%s %s", decl->declared_type->to_string().c_str(), decl->identifier->name.c_str());
            if (decl->initializer) {
                fprintf(out, " = ");
                emitExpression(out, decl->initializer, indent);
            }
            fprintf(out, ";\n");
            break;
        }

        case AST_STATEMENT: {
            indentLine(out, indent);
            emitExpression(out, stmt->expression, indent);
            fprintf(out, ";\n");
            break;
        }

        case AST_IF: {
            auto* ifstmt = static_cast<Ast_If*>(stmt);
            indentLine(out, indent);
            fprintf(out, "if (");
            emitExpression(out, ifstmt->condition, indent);
            fprintf(out, ") ");
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

void emitBlock(FILE* out, Ast_Block* block, int indent) {
    if (!block) return;

    fprintf(out, "{\n");
    for (auto* stmt : block->statements) {
        emitStatement(out, stmt, indent + 4);
    }
    indentLine(out, indent);
    fprintf(out, "}\n");
}


void generate_cpp_code(const char* filename, Ast_Block* program) {
    FILE* out = fopen(filename, "w");
    if (!out) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        return;
    }

    fprintf(out, "/* GENERATED FILE */\n\n");

    fprintf(out, "#include <stdlib.h>\n");
    fprintf(out, "#include <stdio.h>\n\n");

    fprintf(out, "void _generated_main() ");
    emitBlock(out, program, 0);
    fprintf(out, "\n\n");
    fprintf(out, "int main() {\n");
    fprintf(out, "    _generated_main();\n");
    fprintf(out, "    return 0;\n");
    fprintf(out, "}\n");

    fclose(out);
}
