
const char *BOILTERPLATE_TOP =
    "/* GENERATED FILE */\n\n"
    "#include <stdlib.h>\n"
    "#include <stdio.h>\n"
    "#include <string.h>\n"
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

C_Converter::C_Converter(Pax_Interp *_interp) : interp(_interp) , _type(interp->type) {
}

void C_Converter::emitExpression(FILE* out, Ast_Expression* expr, int indent, bool _struct)
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
                case LITERAL_NULL: fprintf(out, "nullptr"); break;
                default: fprintf(out, "/* unknown literal */"); break;
            }
            break;
        }
        case AST_UNARY: {
            Ast_Unary* u = static_cast<Ast_Unary*>(expr);

            fprintf(out, "(");

            switch (u->op) {
                case UNARY_ADDRESS_OF:
                    fprintf(out, "&");
                    emitExpression(out, u->operand, indent, true);
                    break;

                case UNARY_DEREFERENCE:
                    fprintf(out, "*");
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

            fprintf(out, ")");
            break;
        }

        case AST_IDENT: {
            auto *ident = static_cast<Ast_Ident *>(expr);
            fprintf(out, "%s", ident->name);
            break;
        }

        case AST_BINARY: {
            auto *bin = static_cast<Ast_Binary *>(expr);
            fprintf(out, "("); // we will enclose binary expression in parenthesis to prove our operator precedence does work correctly and we are not just copy pasting expressions to c code

            if (bin->op == BINOP_DOT) {

                int pointer_depth = 0;
                bool should_deref = false;

                if (bin->lhs->inferred_type) {
                    Ast_Type_Definition* walker = bin->lhs->inferred_type;
                    while (walker && walker->pointed_to_type) {
                        pointer_depth++;
                        walker = walker->pointed_to_type;
                    }
                    if (walker && walker->struct_def && pointer_depth > 0) {
                        should_deref = true;
                    }
                }

                // implicit auto dereferencing
                if (should_deref) {
                    fprintf(out, "(");
                    for (int i = 0; i < pointer_depth; i++) {
                        fprintf(out, "*");
                    }
                }

                emitExpression(out, bin->lhs, indent);

                if (should_deref) {
                    fprintf(out, ")");
                }

                fprintf(out, ".");
                emitExpression(out, bin->rhs, indent);
            }
            else if (bin->op == BINOP_ARRAY_SUBSCRIPT) {
                Ast_Type_Definition* arr_type = bin->lhs->inferred_type;

                if (!arr_type) {
                    emitExpression(out, bin->lhs, indent);
                    fprintf(out, "[");
                    emitExpression(out, bin->rhs, indent);
                    fprintf(out, "]");
                }
                if (arr_type->pointed_to_type) {
                    Ast_Type_Definition* pointee = arr_type->pointed_to_type;

                    if (pointee->type == AST_ARRAY_TYPE) {
                        auto* arr = static_cast<Ast_Array_Type*>(pointee);

                        fprintf(out, "((");

                        // this part should really be inside type_to_c_string()
                        std::string elem_type_str = "void";
                        if (arr->element_type) {
                            elem_type_str = arr->element_type->to_string(*_type);
                        }
                        fprintf(out, "%s*)(*", elem_type_str.c_str());

                        emitExpression(out, bin->lhs, indent);

                        fprintf(out, ").data)[");
                        emitExpression(out, bin->rhs, indent);
                        fprintf(out, "]");
                    }
                    else {
                        emitExpression(out, bin->lhs, indent);
                        fprintf(out, "[");
                        emitExpression(out, bin->rhs, indent);
                        fprintf(out, "]");
                    }
                }
                else {
                    int ptr_depth = 0;
                    Ast_Type_Definition* base = arr_type;

                    while (base->pointed_to_type) {
                        ptr_depth++;
                        base = base->pointed_to_type;
                    }

                    if (base->type == AST_ARRAY_TYPE) {
                        auto* arr = static_cast<Ast_Array_Type*>(base);

                        // a[i] becomes ((TYPE*)a.data)[i]
                        fprintf(out, "((");

                        // this part should really be inside type_to_c_string()
                        std::string elem_type_str = "void";
                        int ptr_depth_2 = 0;
                        Ast_Type_Definition* base2 = arr->element_type;

                        while (base2->pointed_to_type) {
                            ptr_depth_2++;
                            base2 = base2->pointed_to_type;
                        }

                        elem_type_str = base2->to_string(*_type);
                        fprintf(out, "%s", elem_type_str.c_str());
                        for (int i = 0; i < ptr_depth_2; i++) {
                            fprintf(out, "*");
                        }

                        fprintf(out, "*)");


                        if (ptr_depth > 0) {
                            fprintf(out, "(");
                            for (int i = 0; i < ptr_depth; i++) {
                                fprintf(out, "*");
                            }
                        }

                        emitExpression(out, bin->lhs, indent);

                        if (ptr_depth > 0) {
                            fprintf(out, ")");
                        }

                        fprintf(out, ".data)[");
                        emitExpression(out, bin->rhs, indent);
                        fprintf(out, "]");
                    }
                    else {
                        emitExpression(out, bin->lhs, indent);
                        fprintf(out, "[");
                        emitExpression(out, bin->rhs, indent);
                        fprintf(out, "]");
                    }
                }
            } else {
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
                    case BINOP_LOGICAL_AND: fprintf(out, " && "); break;
                    case BINOP_LOGICAL_OR: fprintf(out, " || "); break;
                    default: fprintf(out, "/*BINOP OP ERROR*/"); break;
                }
                emitExpression(out, bin->rhs, indent);
            }

            fprintf(out, ")");
            break;
        }
        case AST_PROCEDURE_CALL_EXPRESSION: {
            auto* call = static_cast<Ast_Procedure_Call_Expression*>(expr);

            // // Special handling for NewArray - call helper
            // if (call->function && strcmp(call->function->name, "NewArray") == 0) {
            //     if (call->arguments && call->arguments->arguments.count > 0) {
            //         Ast_Expression* count_expr = call->arguments->arguments.data[0];

            //         std::string elem_type_str = "void";

            //         // Try to get element type from inferred_type
            //         if (expr->inferred_type && expr->inferred_type->type == AST_ARRAY_TYPE) {
            //             auto* arr = static_cast<Ast_Array_Type*>(expr->inferred_type);
            //             if (arr->element_type) {
            //                 elem_type_str = arr->element_type->to_string(*_type);
            //             }
            //         }
            //         // Or from resolved_element_type if you store it
            //         else if (call->resolved_element_type) {
            //             elem_type_str = call->resolved_element_type->to_string(*_type);
            //         }

            //         fprintf(out, "__NewArray_impl(");
            //         emitExpression(out, count_expr, indent);
            //         fprintf(out, ", sizeof(%s))", elem_type_str.c_str());
            //         break;
            //     }
            // }

            // Special cast for malloc return type
            if(call && strcmp(call->function->name, "malloc") == 0){
                type_to_c_string(out, expr->inferred_type, nullptr, false, indent);
            }
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


void C_Converter::type_to_c_string(FILE *out, Ast_Type_Definition* type, Ast_Declaration *decl, bool need_semicolon, int indent, bool should_initializer) {
    if (!type) return;

    std::string type_str;
    Ast_Type_Definition* current = type;
    int pointer_depth = 0;

    while (current->pointed_to_type) {
        pointer_depth++;
        current = current->pointed_to_type;
    }

    if (current->type == AST_ARRAY_TYPE && current->struct_def) {
        const char* struct_name = current->struct_def->name;

        if (struct_name) {
            type_str = "struct ";
            type_str += struct_name;
        } else {
            type_str = "void /* unresolved array */";
        }

    } else if (current->type == AST_ARRAY_TYPE && current->struct_def == nullptr) {
            type_str = "Static_Array";
    }
    else {
        type_str = current->to_string(*_type);
    }

    for (int i = 0; i < pointer_depth; ++i) {
        type_str += " *";
    }

    if (decl) {
        fprintf(out, "%s %s", type_str.c_str(), decl->identifier->name);

        if (decl->initializer && !should_initializer) {
            fprintf(out, " = ");

            // should not fall here normally
            // if we need a cast Dynamic_Array* to Static_Array*
            if (pointer_depth > 0 && current->type == AST_ARRAY_TYPE && current->struct_def) {
                const char* target_struct = current->struct_def->name;

                // If initializer is address-of dynamic array, cast it
                if (decl->initializer->type == AST_UNARY) {
                    auto* unary = static_cast<Ast_Unary*>(decl->initializer);
                    if (unary->op == UNARY_ADDRESS_OF && unary->operand->inferred_type) {
                        Ast_Type_Definition* operand_type = unary->operand->inferred_type;

                        if (operand_type->type == AST_ARRAY_TYPE && operand_type->struct_def) {
                            const char* source_struct = operand_type->struct_def->name;

                            // Dynamic → Static view cast
                            if (strcmp(target_struct, "Static_Array") == 0 &&
                                strcmp(source_struct, "Dynamic_Array") == 0) {
                                fprintf(out, "(struct Static_Array *)");
                            }
                        }
                    }
                }
            }

            emitExpression(out, decl->initializer, indent);
        }
    } else {
        fprintf(out, "(%s)", type_str.c_str());
    }

    if (need_semicolon) {
        fprintf(out, ";\n");
    }
}

void C_Converter::emitFunctionPrototype(FILE* out, Ast_Declaration* decl, int indent) {
    if (!decl || !decl->is_function || !decl->identifier) return;

    indentLine(out, indent);
    type_to_c_string(out, decl->return_type, decl, false, indent);
    fprintf(out, "(");
    for (int i = 0; i < decl->parameters.count; ++i) {
        auto* param = decl->parameters.data[i];
        if (i > 0) fprintf(out, ", ");
        type_to_c_string(out, param->declared_type, param, false, indent, true);
    }
    if (decl->parameters.count == 0) {
        fprintf(out, "void");
    }
    fprintf(out, ");\n");
}

void C_Converter::emitStructPrototype(FILE* out, Ast_Statement* stmt, int indent){
    if(!stmt || !stmt->expression) return;

    fprintf(out, "struct ");
    auto *struct_def = static_cast<Ast_Struct *>(stmt->expression);
    fprintf(out, "%s;\n", struct_def->name);

}
void C_Converter::emitStruct(FILE* out, Ast_Statement* stmt, int indent) {
    if(!stmt || !stmt->expression) return;

    indentLine(out, indent);
    fprintf(out, "struct ");
    auto *struct_def = static_cast<Ast_Struct *>(stmt->expression);
    fprintf(out, "%s {\n", struct_def->name);

    for (int i = 0; i < struct_def->members.count; ++i) {
        auto* member = struct_def->members.data[i];
        if(!member->declared_type) fprintf(out, "/*member not inferred\n*/");
        type_to_c_string(out, member->declared_type, member, true, indent);
    }

    fprintf(out, "};\n");
}

void C_Converter::emitStatement(FILE* out, Ast_Statement* stmt, int indent)
{
    if (!stmt) return;
    // fprintf(out, "#line %d \"%s\"\n", stmt->line_number, stmt->file_name);
    switch (stmt->type) {

        case AST_DECLARATION: {

            auto* decl = static_cast<Ast_Declaration*>(stmt);

            if (decl->is_function) {
                // fprintf(out, "#line %d \"%s\"\n", stmt->line_number, stmt->file_name);

                // fprintf(out, "\n");
                indentLine(out, indent);

                // emit return type
                type_to_c_string(out, decl->return_type, decl, false, indent);

                fprintf(out, " (");

                // emit params
                for (int i = 0; i < decl->parameters.count; ++i) {
                    auto* param = decl->parameters.data[i];

                    type_to_c_string(out, param->declared_type, param, false, indent, true);

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

            // static array declarations
            Ast_Type_Definition* base_type = decl->declared_type;
            int ptr_depth = 0;
            Ast_Type_Definition* decl_type = base_type;
            while (base_type && base_type->pointed_to_type) {
                ptr_depth++;
                base_type = base_type->pointed_to_type;
            }

            // if its not behind a pointer
            if (ptr_depth == 0 && base_type && base_type->type == AST_ARRAY_TYPE && base_type->struct_def) {

                auto* arr = static_cast<Ast_Array_Type*>(base_type);

                if (!arr->is_resizable && arr->size_expr &&
                    base_type->struct_def->name &&
                    strcmp(base_type->struct_def->name, "Static_Array") == 0) {

                    long long size = 0;
                    if (arr->size_expr->type == AST_LITERAL) {
                        Ast_Literal* lit = static_cast<Ast_Literal*>(arr->size_expr);
                        if (lit->value_type == LITERAL_NUMBER) {
                            size = lit->integer_value;
                        }
                    }


                    // this part should really be inside type_to_c_string()
                    std::string elem_type_str = "void";
                    // if (arr->element_type) {
                    //     elem_type_str = arr->element_type->to_string(*_type);
                    // }
                    int ptr_depth_2 = 0;
                    Ast_Type_Definition* base2 = arr->element_type;

                    while (base2->pointed_to_type) {
                        ptr_depth_2++;
                        base2 = base2->pointed_to_type;
                    }

                    elem_type_str = base2->to_string(*_type);

                    fprintf(out, "%s", elem_type_str.c_str());
                    for (int i = 0; i < ptr_depth_2; i++) {
                        fprintf(out, "*");
                    }

                    fprintf(out, " __data__%s[%lld];\n",
                           decl->identifier->name,
                           size);

                    indentLine(out, indent);

                    fprintf(out, "Static_Array %s;\n", decl->identifier->name);

                    indentLine(out, indent);

                    fprintf(out, "%s.data = (void *)__data__%s;\n",
                           decl->identifier->name,
                           decl->identifier->name);

                    indentLine(out, indent);

                    fprintf(out, "%s.count = %lld;\n", decl->identifier->name, size);

                    break;
                }
            }

            // Check if this is a struct with array members
            if (decl_type && decl_type->struct_def) {
                Ast_Struct* struct_def = decl_type->struct_def;

                // First, emit backing storage for any array members
                for (int i = 0; i < struct_def->members.count; i++) {
                    Ast_Declaration* member = struct_def->members.data[i];
                    if (!member || !member->declared_type) continue;

                    Ast_Type_Definition* member_type = member->declared_type;

                    if (member_type->type == AST_ARRAY_TYPE) {
                        auto* arr = static_cast<Ast_Array_Type*>(member_type);

                        if (!arr->is_resizable && arr->size_expr) {
                            long long size = 0;
                            if (arr->size_expr->type == AST_LITERAL) {
                                auto* lit = static_cast<Ast_Literal*>(arr->size_expr);
                                if (lit->value_type == LITERAL_NUMBER) {
                                    size = lit->integer_value;
                                }
                            }

                            std::string elem_type_str = "void";
                            if (arr->element_type) {
                                Ast_Type_Definition* elem = arr->element_type;
                                int elem_ptr_depth = 0;

                                while (elem->pointed_to_type) {
                                    elem_ptr_depth++;
                                    elem = elem->pointed_to_type;
                                }

                                elem_type_str = elem->to_string(*_type);
                                for (int p = 0; p < elem_ptr_depth; ++p) {
                                    elem_type_str += "*";
                                }
                            }

                            // Emit: TYPE __data__STRUCTVAR_MEMBERNAME[SIZE];
                            fprintf(out, "%s __data__%s_%s[%lld];\n",
                                   elem_type_str.c_str(),
                                   decl->identifier->name,
                                   member->identifier->name,
                                   size);

                            indentLine(out, indent);
                        }
                    }
                }
            }
            type_to_c_string(out, decl->declared_type, decl, true, indent);

            // Initialize array members if it's a struct
            if (decl_type && decl_type->struct_def) {
                Ast_Struct* struct_def = decl_type->struct_def;

                for (int i = 0; i < struct_def->members.count; i++) {
                    Ast_Declaration* member = struct_def->members.data[i];
                    if (!member || !member->declared_type) continue;

                    Ast_Type_Definition* member_type = member->declared_type;

                    if (member_type->type == AST_ARRAY_TYPE) {
                        auto* arr = static_cast<Ast_Array_Type*>(member_type);

                        if (!arr->is_resizable && arr->size_expr) {
                            long long size = 0;
                            if (arr->size_expr->type == AST_LITERAL) {
                                auto* lit = static_cast<Ast_Literal*>(arr->size_expr);
                                if (lit->value_type == LITERAL_NUMBER) {
                                    size = lit->integer_value;
                                }
                            }

                            indentLine(out, indent);

                            fprintf(out, "%s.%s.data = (void *)__data__%s_%s;\n",
                                   decl->identifier->name,
                                   member->identifier->name,
                                   decl->identifier->name,
                                   member->identifier->name);

                            indentLine(out, indent);

                            fprintf(out, "%s.%s.count = %lld;\n",
                                   decl->identifier->name,
                                   member->identifier->name,
                                   size);
                        }
                    }
                }
            }

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
            else if(stmt->is_return) { // if its not an expression but its return statement then its a return to void
                indentLine(out, indent);
                fprintf(out, "return;\n");
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

                if (ifstmt->else_block->type == AST_IF) {
                    emitStatement(out, ifstmt->else_block, indent); // else if
                } else if (ifstmt->else_block->type == AST_BLOCK) {
                    emitBlock(out, static_cast<Ast_Block*>(ifstmt->else_block), indent); // else
                }
            }
            break;
        }

        case AST_WHILE: {
            auto* while_stmt = static_cast<Ast_While*>(stmt);
            indentLine(out, indent);
            fprintf(out, "while");
            emitExpression(out, while_stmt->condition, indent);
            emitBlock(out, while_stmt->block, indent);
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

std::vector<Ast_Statement*>
C_Converter::topologically_sort_structs(const std::vector<Ast_Statement*>& structs) {

    // Build dependency map: struct -> structs it depends on
    std::map<Ast_Struct*, std::set<Ast_Struct*>> dependencies;

    for (auto* stmt : structs) {
        Ast_Struct* s = static_cast<Ast_Struct*>(stmt->expression);

        for (int i = 0; i < s->members.count; ++i) {
            Ast_Declaration* member = s->members.data[i];
            if (!member || !member->declared_type) continue;

            Ast_Type_Definition* type = member->declared_type;

            // Skip pointers members
            if (type->pointed_to_type) continue;

            // only add dependency if member is a value of another struct
            if (type->struct_def) {
                dependencies[s].insert(type->struct_def);
            }
        }
    }

    // Topological sort using DFS
    std::vector<Ast_Statement*> result;
    std::set<Ast_Struct*> visited;
    std::set<Ast_Struct*> in_progress;

    std::function<void(Ast_Statement*)> visit = [&](Ast_Statement* stmt) {
        Ast_Struct* s = static_cast<Ast_Struct*>(stmt->expression);

        if (visited.count(s)) return;

        if (in_progress.count(s)) {
            fprintf(stderr, "Error: Circular struct dependency involving '%s'\n", s->name ? s->name : "(unknown)");

            exit(1);
        }

        in_progress.insert(s);

        // Visit dependencies first (depth-first)
        if (dependencies.count(s)) {
            for (Ast_Struct* dep : dependencies.at(s)) {
                // Find statement for this dependency
                for (auto* dep_stmt : structs) {
                    if (static_cast<Ast_Struct*>(dep_stmt->expression) == dep) {
                        visit(dep_stmt);
                        break;
                    }
                }
            }
        }

        in_progress.erase(s);
        visited.insert(s);
        result.push_back(stmt);
    };

    // Visit all structs
    for (auto* stmt : structs) {
        visit(stmt);
    }

    return result;
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

    std::vector<Ast_Statement*> vars; // TEMPORARY
    std::vector<Ast_Declaration*> functions; // TEMPORARY
    std::vector<Ast_Statement*> structs; // TEMPORARY

    for (int i = 0; i < program->statements.count; i++) {
        Ast_Statement* stmt = program->statements.data[i];
        if (!stmt) continue;
        if (stmt->type == AST_DECLARATION) {
            Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
            if (decl->is_function && decl->is_function_body && !decl->is_foreign) {
                functions.push_back(decl);
            }
            else {
                if(!decl->is_foreign)
                    vars.push_back(decl);
            }
        } else if (stmt->expression && stmt->expression->type == AST_STRUCT){
            structs.push_back(stmt);
        }

    }

    std::vector<Ast_Statement*> sorted_structs = topologically_sort_structs(structs);

    fprintf(out, "/*BSS SECTION GLOBAL VARIAABLES*/\n");
    for (auto *v : vars) {
        emitStatement(out, v, 0);
    }
    fprintf(out, "\n");


    fprintf(out, "/*STRUCT FORWARD DECLARATIONS*/\n");
    for (auto *stmt : sorted_structs) {
        emitStructPrototype(out, stmt, 0);
    }
    fprintf(out, "\n");


    fprintf(out, "/*GLOBAL FUNCTION FORWARD DECLARATIONS*/\n");
    for (auto *decl : functions) {
        emitFunctionPrototype(out, decl, 0);
    }
    fprintf(out, "\n");


    fprintf(out, "/*STRUCTS DEFINITIONS*/\n");
    for (auto *stmt : sorted_structs) {
        emitStruct(out, stmt, 0);
    }
    fprintf(out, "\n");


    fprintf(out, "/*FUNCTION BODIES*/\n");
    for (auto *decl : functions) {
        auto *stmt = reinterpret_cast<Ast_Statement *>(decl);
        emitStatement(out, stmt, 0);
    }
    fprintf(out, "\n");



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

    // fprintf(out, "#line %d \"%s\"\n", mainBlock->line_number, mainBlock->file_name);
    fprintf(out, "\nvoid GENERATED_MAIN()");

    emitBlock(out, mainBlock, 0);

    fprintf(out, "\nint main(int argc, char **argv){\n");
    fprintf(out, "    GENERATED_MAIN();\n");
    fprintf(out, "    return 0;\n");
    fprintf(out, "}\n");

    fclose(out);
}
