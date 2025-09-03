#include "ast.h"
#include <cstdio>
#include <string>

void indentLine(FILE* out, int indent) {
    for (int i = 0; i < indent; ++i) fputc(' ', out);
}

void emitStatement(FILE* out, Ast_Statement* stmt, int indent = 0);
void emitExpression(FILE* out, Ast_Expression* expr, int indent = 0);

void emitDeclaration(FILE* out, Ast_Declaration* decl, int indent = 0);

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
                case BINOP_ASSIGN: fprintf(out, " = "); break;
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

// void emitDeclaration(FILE* out, Ast_Declaration* decl, int indent) {
//     if (!decl || !decl->declared_type)
//     {
//         printf("undelcared type!!! %s\n", decl->identifier->name);
//         return;
//     }
//     indentLine(out, indent);
//     fprintf(out, "%s %s", decl->declared_type->to_string().c_str(), decl->identifier->name.c_str());
//     if (decl->initializer) {
//         fprintf(out, " = ");
//         emitExpression(out, decl->initializer, 0);
//     }
//     fprintf(out, ";\n");
// }

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
            if (stmt->expression) {
                indentLine(out, indent);
                emitExpression(out, stmt->expression, indent);

                fprintf(out, ";\n");
            }
            else if (stmt->block) {
                indentLine(out, indent);
                // Scoped block inside statement
                emitBlock(out, stmt->block, indent);
            }
            else {
                indentLine(out, indent);
                fprintf(out, "; // empty statement\n");
            }
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

    // fprintf(out, "\n");

    // indentLine(out, indent);
    fprintf(out, "{\n");

    // right now we emit all the members local to current block to the very top.
    // idk how this would affect in future when we implement #line directive feature to step into our code
    // for (auto* decl : block->members) {
    //     emitDeclaration(out, decl, indent + 4);
    // }


    // if (!block->members.empty() && !block->statements.empty()) {
    //     fprintf(out, "\n");
    // }

    for (auto* stmt : block->statements) {
        emitStatement(out, stmt, indent+4);
    }

    // for (auto *c_scope : block->child_scopes){
    //     emitBlock(out, c_scope, indent+4);
    // }
    indentLine(out, indent);
    fprintf(out, "}\n");
}

void generate_cpp_code(const char* filename, Ast_Block* program) {
    FILE* out = nullptr;
    fopen_s(&out, filename, "w");
    if (!out) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        return;
    }

    fprintf(out, "/* GENERATED FILE */\n\n");
    fprintf(out, "#include <stdlib.h>\n");
    fprintf(out, "#include <stdio.h>\n\n");

    // 1️⃣ Emit top-level globals
    for (auto* stmt : program->statements) {
        if (!stmt) continue;

    // global variable declarations
        if (stmt->type == AST_DECLARATION) {
            emitStatement(out, stmt, 0);
        }
    }
    // for (auto* decl : program->members) {
    //     emitDeclaration(out, decl, 0);
    // }

    // 2️⃣ Find the entry point block (main)
    Ast_Block* mainBlock = nullptr;
    for (auto* stmt : program->statements) {
        if (stmt && stmt->block && stmt->block->is_entry_point) {
            mainBlock = stmt->block;
            break;
        }
    }

    if (!mainBlock) {
        fprintf(stderr, "No main block found in AST\n");
        fclose(out);
        return;
    }

    // 3️⃣ Emit the entry point function
    fprintf(out, "\nvoid _generated_main() ");
    emitBlock(out, mainBlock, 0);

    // 4️⃣ Emit standard C main
    fprintf(out, "\nint main() {\n");
    fprintf(out, "    _generated_main();\n");
    fprintf(out, "    return 0;\n");
    fprintf(out, "}\n");

    fclose(out);
}
