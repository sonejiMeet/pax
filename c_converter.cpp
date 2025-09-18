#include "ast.h"
#include <cstdio>
#include <string>

void emitStatement(FILE* out, Ast_Statement* stmt, int indent = 0);
void emitExpression(FILE* out, Ast_Expression* expr, int indent = 0);
void emitBlock(FILE* out, Ast_Block* block, int indent = 0);

void indentLine(FILE* out, int indent)
{
    for (int i = 0; i < indent; ++i)
        fputc(' ', out);
}

void emitExpression(FILE* out, Ast_Expression* expr, int indent)
{
    if (!expr) return;

    switch (expr->type) {
        case AST_LITERAL: {
            auto* lit = static_cast<Ast_Literal*>(expr);
            switch (lit->value_type) {
                case LITERAL_NUMBER: fprintf(out, "%lld", lit->integer_value); break;
                case LITERAL_FLOAT:  fprintf(out, "%f", lit->float_value); break;
                case LITERAL_STRING: fprintf(out, "\"%s\"", lit->string_value ? lit->string_value : ""); break;
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
                    // ^x -> &x
                    fprintf(out, "&");
                    emitExpression(out, u->operand, indent);
                    break;

                case UNARY_DEREFERENCE:
                    // *x -> *x
                    fprintf(out, "/*Dereference*/ * ");
                    emitExpression(out, u->operand, indent);
                    break;

                case UNARY_REFERENCE:
                    // &x -> just output operand (handled as pointer in declaration)
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
            //fprintf(out, "%s", ident->name.c_str());
            fprintf(out, "%s", ident->name ? ident->name : "");
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
            fprintf(out, "%s(", call->function->name ? call->function->name : "");
            if (call->arguments) {
                bool first = true;
                    for (int i = 0; i < call->arguments->arguments.count; i++) {
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

void emitStatement(FILE* out, Ast_Statement* stmt, int indent)
{
    if (!stmt) return;

    switch (stmt->type) {

        case AST_DECLARATION: {
            auto* decl = static_cast<Ast_Declaration*>(stmt);
            indentLine(out, indent);

            // ----- 1. Build C type string -----
            std::string type_str;
            std::string array_suffix;
            Ast_Type_Definition* type = decl->declared_type;

            if (!type) {
                type_str = "UNKNOWN";
            } else {
                Ast_Type_Definition* base_type = type;
                int pointer_depth = 0;

                // Walk down the type chain to find the base type
                while (base_type->pointed_to_type) {
                    base_type = base_type->pointed_to_type;
                    pointer_depth++;
                }

                // Handle arrays
                if (type->array_kind == ARRAY_STATIC && type->element_type) {
                    type_str = type->element_type->to_string();
                    array_suffix = "[" + std::to_string(type->static_array_size) + "]";
                } else {
                    type_str = base_type->to_string();
                }

                // Add pointer layers for ^int, ^^int, etc.
                for (int i = 0; i < pointer_depth; ++i) {
                    type_str += " *";
                }

                // References are represented as pointers in C
                if (type->is_reference) {
                    type_str += " *";
                }
            }

            // Emit variable name: "int *rint"
            /*fprintf(out, "%s %s%s", type_str.c_str(), decl->identifier->name.c_str(), array_suffix.c_str());*/
            fprintf(out, "%s %s%s", type_str.c_str(), decl->identifier->name ? decl->identifier->name : "", array_suffix.c_str());
            // ----- 2. Emit initializer -----
                if (decl->initializer) {
                    fprintf(out, " = ");

                    // Case A: Declared type is a reference (e.g., rint: &int = aint)
                    // Case B: Initializer explicitly uses UNARY_REFERENCE (e.g., pInt: ^int = &aint)
                    bool needs_address = (type && type->is_reference);

                    if (!needs_address && decl->initializer->type == AST_UNARY) {
                        auto* unary = static_cast<Ast_Unary*>(decl->initializer);
                        if (unary->op == UNARY_REFERENCE) {
                            needs_address = true;
                        }
                    }

                    if (needs_address) {
                        // If the initializer is a simple identifier, just output "&id"
                        if (decl->initializer->type == AST_IDENT) {
                            auto* id = static_cast<Ast_Ident*>(decl->initializer);
                            /*fprintf(out, "&%s", id->name.c_str());*/
                            fprintf(out, "&%s", id->name ? id->name : "");
                        }
                        // If the initializer itself is UNARY_REFERENCE, emit only its operand
                        else if (decl->initializer->type == AST_UNARY) {
                            auto* unary = static_cast<Ast_Unary*>(decl->initializer);
                            if (unary->op == UNARY_REFERENCE) {
                                if (unary->operand->type == AST_IDENT) {
                                    auto* inner_id = static_cast<Ast_Ident*>(unary->operand);
                                    //fprintf(out, "&%s", inner_id->name.c_str());
                                    fprintf(out, "&%s", inner_id->name ? inner_id->name : "");
                                } else {
                                    fprintf(out, "&(");
                                    emitExpression(out, unary->operand, indent);
                                    fprintf(out, ")");
                                }
                            } else {
                                emitExpression(out, decl->initializer, indent);
                            }
                        }
                        // Complex case — fallback
                        else {
                            fprintf(out, "&(");
                            emitExpression(out, decl->initializer, indent);
                            fprintf(out, ")");
                        }
                    } else {
                        // Normal value assignment
                        emitExpression(out, decl->initializer, indent);
                    }
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

void emitBlock(FILE* out, Ast_Block* block, int indent)
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

void generate_cpp_code(const char* filename, Ast_Block* program)
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

    fprintf(out, "/* GENERATED FILE */\n\n");
    fprintf(out, "#include <stdlib.h>\n");
    fprintf(out, "#include <stdio.h>\n\n");

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
