
// DO NOT REMOVE THAT EXTRA LINE UNDER fprintf below
#ifdef _DEBUG
#define PRINT_DEBUG_INFO(out, ...) \
        fprintf(out, __VA_ARGS__) \

#else
#define PRINT_DEBUG_INFO(out, ...)
#endif

const char *BOILTERPLATE_TOP =
    "/* GENERATED FILE */\n\n"
    "#include <stdlib.h>\n"
    "#include <stdio.h>\n"
    "#include <string.h>\n"
    "#include <math.h>\n"
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

const char *WINDOWS_RUNTIME_CRASH_HANDLER_HELPER =
    "#ifdef _WIN32\n"
    "#define WIN32_LEAN_AND_MEAN\n"
    "#define NOMINMAX\n"
    "#include <windows.h>\n"
    "#endif\n"
    "#include <dbghelp.h>\n"
    "\n"
    "#pragma comment(lib, \"Dbghelp.lib\")\n"
    "\n"
    "LONG WINAPI SimpleCrashHandler(EXCEPTION_POINTERS* ep) {\n"
    "    DWORD code = ep->ExceptionRecord->ExceptionCode;\n"
    "    if (code == EXCEPTION_STACK_OVERFLOW) {\n"
    "        puts(\" \x1B[91m STACK OVERFLOW \x1B[0m\");\n"
    "        return EXCEPTION_EXECUTE_HANDLER;\n"
    "    }\n"
    "    HANDLE proc = GetCurrentProcess();\n"
    "    SymInitialize(proc, NULL, TRUE);\n"
    "    STACKFRAME64 frame = {};\n"
    "    DWORD machine = IMAGE_FILE_MACHINE_AMD64;\n"
    "    frame.AddrPC.Offset = ep->ContextRecord->Rip;\n"
    "    frame.AddrPC.Mode = AddrModeFlat;\n"
    "    printf(\"\\n \x1B[91m--- RUNTIME CRASH STACK TRACE ---\x1B[0m \\n\");\n"
    "    printf(\"\\n*******\");\n"
    "    while (StackWalk64(machine, proc, GetCurrentThread(), &frame, ep->ContextRecord, NULL, SymFunctionTableAccess64, SymGetModuleBase64, NULL)) {\n"
    "        if (frame.AddrPC.Offset == 0) break;\n"
    "        char buf[sizeof(SYMBOL_INFO) + MAX_SYM_NAME];\n"
    "        SYMBOL_INFO* sym = (SYMBOL_INFO*)buf;\n"
    "        sym->SizeOfStruct = sizeof(SYMBOL_INFO);\n"
    "        sym->MaxNameLen = MAX_SYM_NAME;\n"
    "        IMAGEHLP_LINE64 line = { sizeof(IMAGEHLP_LINE64) };\n"
    "        DWORD disp;\n"
    "        DWORD64 d64;\n"
    "        if (SymFromAddr(proc, frame.AddrPC.Offset, &d64, sym)) {\n"
    "            if (SymGetLineFromAddr64(proc, frame.AddrPC.Offset, &disp, &line))\n"
    "                printf(\"\t%s:%lu\", line.FileName, line.LineNumber);\n"
    "            printf(\"  %s\", sym->Name);\n"
    "            printf(\"\\n\");\n"
    "        }\n"
    "       if (strcmp(sym->Name, \"GENERATED_MAIN\") == 0) break;"
    "    }\n"
    "    printf(\"\\n \x1B[91m--- END OF STACK TRACE ---\x1B[0m \\n\");\n"

    "    SymCleanup(proc);\n"
    "    return EXCEPTION_EXECUTE_HANDLER;\n"
    "}\n"
    "void InstallHandler() {\n"
    "     ULONG reserve = 16*1024;\n"
    "     SetThreadStackGuarantee(&reserve);\n"
    "    SetUnhandledExceptionFilter(SimpleCrashHandler);\n"
    "}\n"
;

C_Converter::C_Converter(Pax_Interp *_interp) : interp(_interp) , _type(interp->type){
}

void C_Converter::emitExpression(FILE *out, Ast_Expression *expr, int indent, bool _struct)
{
    if(!expr) return;

    switch (expr->type){
        case AST_LITERAL: {
            auto *lit = static_cast<Ast_Literal*>(expr);
            switch (lit->value_type){
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
            Ast_Unary *u = static_cast<Ast_Unary*>(expr);

            fprintf(out, "(");

            switch (u->op){
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
            // indentLine(out, indent);
            PRINT_DEBUG_INFO(out, "\n#line %d \"%s\"\n", expr->line_number, expr->file_name);

            if (bin->lhs->type == AST_COMMA_SEPARATED_ARGS) {
                Ast_Comma_Separated_Args *lhs_args = static_cast<Ast_Comma_Separated_Args*>(bin->lhs);

                if (bin->rhs->type == AST_COMMA_SEPARATED_ARGS) {
                    Ast_Comma_Separated_Args *rhs_args = static_cast<Ast_Comma_Separated_Args*>(bin->rhs);

                    if (rhs_args->arguments.count == 1) {
                        fprintf(out, "(");
                        for (int i = 0; i < lhs_args->arguments.count; i++) {
                            emitExpression(out, lhs_args->arguments.data[i], indent);
                            fprintf(out, " = ");
                        }
                        emitExpression(out, rhs_args->arguments.data[0], indent);
                        fprintf(out, ")");
                        return;
                    }

                    fprintf(out, "(");
                    for (int i = 0; i < lhs_args->arguments.count; i++) {
                        if (i > 0) fprintf(out, ", ");
                        emitExpression(out, lhs_args->arguments.data[i], indent);
                        fprintf(out, " = ");
                        emitExpression(out, rhs_args->arguments.data[i], indent);
                    }
                    fprintf(out, ")");
                    return;
                }
            }
            fprintf(out, "("); // we will enclose binary expression in parenthesis to prove our operator precedence does work correctly and we are not just copy pasting expressions to c code

            if(bin->op == BINOP_DOT){

                int pointer_depth = 0;
                bool should_deref = false;

                if(bin->lhs->inferred_type){
                    Ast_Type_Definition *walker = bin->lhs->inferred_type;
                    while (walker && walker->pointed_to_type){
                        pointer_depth++;
                        walker = walker->pointed_to_type;
                    }
                    if(walker && walker->struct_def && pointer_depth > 0){
                        should_deref = true;
                    }
                }

                // implicit auto dereferencing
                if(should_deref){
                    fprintf(out, "(");
                    for(int i = 0; i < pointer_depth; i++){
                        fprintf(out, "*");
                    }
                }

                emitExpression(out, bin->lhs, indent);

                if(should_deref){
                    fprintf(out, ")");
                }

                fprintf(out, ".");
                emitExpression(out, bin->rhs, indent);
            }
            else if(bin->op == BINOP_ARRAY_SUBSCRIPT){
                Ast_Type_Definition *arr_type = bin->lhs->inferred_type;

                if(!arr_type){

                    emitExpression(out, bin->lhs, indent);
                    fprintf(out, "[");
                    emitExpression(out, bin->rhs, indent);
                    fprintf(out, "]");
                }
                if(arr_type->pointed_to_type){
                    Ast_Type_Definition *pointee = arr_type->pointed_to_type;

                    if(pointee->type == AST_ARRAY_TYPE){
                        auto *arr = static_cast<Ast_Array_Type*>(pointee);

                        fprintf(out, "((");

                        // this part should really be inside type_to_c_string()
                        const char *elem_type_str = "void";
                        if(arr->element_type){
                            elem_type_str = arr->element_type->to_string(*_type);
                        }
                        fprintf(out, "%s*)(*", elem_type_str);

                        emitExpression(out, bin->lhs, indent);

                        fprintf(out, ").data)[");
                        emitExpression(out, bin->rhs, indent);
                        fprintf(out, "]");

                        if(arr->element_type->name && strcmp(arr->element_type->name, "String") == 0) {  // for arrays of String type
                            if(bin->lhs->inferred_type->name && bin->rhs->inferred_type->name){
                                if(strcmp(bin->lhs->inferred_type->name, "String") != 0 && strcmp(bin->rhs->inferred_type->name, "String") != 0){
                                    fprintf(out, ".data");
                                }
                            }
                            else fprintf(out, ".data");
                        }

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
                    Ast_Type_Definition *base = arr_type;

                    while (base->pointed_to_type){
                        ptr_depth++;
                        base = base->pointed_to_type;
                    }

                    if(base->type == AST_ARRAY_TYPE){
                        auto *arr = static_cast<Ast_Array_Type*>(base);


                        // a[i] becomes ((TYPE*)a.data)[i]
                        fprintf(out, "((");

                        // this part should really be inside type_to_c_string()
                        const char *elem_type_str = "void";
                        int ptr_depth_2 = 0;
                        Ast_Type_Definition *base2 = arr->element_type;

                        while (base2->pointed_to_type) {
                            ptr_depth_2++;
                            base2 = base2->pointed_to_type;
                        }

                        elem_type_str = base2->to_string(*_type);
                        fprintf(out, "%s", elem_type_str);
                        for(int i = 0; i < ptr_depth_2; i++){
                            fprintf(out, "*");
                        }

                        fprintf(out, "*)");


                        if(ptr_depth > 0){
                            fprintf(out, "(");
                            for(int i = 0; i < ptr_depth; i++){
                                fprintf(out, "*");
                            }
                        }

                        emitExpression(out, bin->lhs, indent);

                        if(ptr_depth > 0){
                            fprintf(out, ")");
                        }

                        fprintf(out, ".data)[");
                        emitExpression(out, bin->rhs, indent);
                        fprintf(out, "]");

                    } else if(arr_type->name &&  strcmp(arr_type->name, "String") == 0){

                        // a[i] becomes ((a.data)[i])
                        fprintf(out, "(");
                        emitExpression(out, bin->lhs, indent);
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
                switch (bin->op){
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
            PRINT_DEBUG_INFO(out, "\n#line %d \"%s\"\n", expr->line_number, expr->file_name);
            
            auto *call = static_cast<Ast_Procedure_Call_Expression*>(expr);

            // Special cast for malloc return type
            if(call && strcmp(call->function->name, "malloc") == 0){
                type_to_c_string(out, expr->inferred_type, nullptr, false, indent);
            }
            fprintf(out, "%s(", call->function->name);

            Ast_Declaration *function_decl = find_function_declaration(call);


            if (!function_decl || function_decl->is_foreign) {
                if (call->arguments) {
                    bool first = true;
                    FOR(call->arguments->arguments) 
                    {
                        Ast_Expression *value = get_call_argument_value(it);

                        if(!value) continue;

                        if(!first) fprintf(out, ",");
                        emitExpression(out, value, indent);
                        first = false;
                    }
                }
                fprintf(out, ")");
                break;
            }

            
            // For each param: use its matching named argument, otherwise consume the next positional argument, otherwise use its default initializer.
            int next_positional_argument = 0;

            FOR(function_decl->parameters)
            {
                if (!it) continue; 

                Ast_Expression *argument = find_call_argument(call, it, &next_positional_argument);

                if (!argument) {
                    argument = it->initializer;
                }

                if (it_index != 0) {
                    fprintf(out, ", ");
                }

                // this should be checked in previous phases but just in case 
                if (!argument) {
                    fprintf(out, "/* missing argument %s */ 0",
                                 it->identifier && it->identifier->name ? it->identifier->name : "(unknown)");
                    continue;
                }

                emitExpression(out, argument, indent);
            }

            fprintf(out, ")");
            break;
        }

        case AST_CAST: {
            PRINT_DEBUG_INFO(out, "\n#line %d \"%s\"\n", expr->line_number, expr->file_name);

            auto *cast = static_cast<Ast_Cast*>(expr);

            type_to_c_string(out, cast->cast_expression, nullptr, false, indent);

            fprintf(out, "(");
            if (cast->expression) {
                emitExpression(out, cast->expression, indent);
            }
            fprintf(out, ")");

            break;
        }

        default:
            fprintf(out, "/* unhandled expression */");
            break;
    }
}

Ast_Expression *C_Converter::get_call_argument_value(Ast_Expression *argument)
{
    if (!argument) {
        return nullptr;
    }

    if (argument->type == AST_NAMED_ARGUMENT) {
        Ast_Named_Argument *named = static_cast<Ast_Named_Argument *>(argument);

        return named->value;
    }

    return argument;
}

Ast_Declaration *C_Converter::find_function_declaration(Ast_Procedure_Call_Expression *call)
{
    if (!call || !call->function) {
        return nullptr;
    }

    if (call->function->type != AST_IDENT) {
        return nullptr;
    }

    Ast_Ident *function_name = static_cast<Ast_Ident *>(call->function);

    if (!function_name->name) {
        return nullptr;
    }

    /*
     fix:
        The semantic pass should ideally store this directly on the call AST, call->resolved_function = decl.
    */
    Ast_Block *global_scope = interp->ast;

    if (!global_scope) {
        return nullptr;
    }

    FOR(global_scope->statements) {
        Ast_Statement *statement = it;

        if (!statement || statement->type != AST_DECLARATION) {
            continue;
        }

        Ast_Declaration *declaration = static_cast<Ast_Declaration *>(statement);

        if (!declaration->is_function || !declaration->identifier || !declaration->identifier->name){
            continue;
        }

        if (strcmp(declaration->identifier->name, function_name->name) == 0) {
            return declaration;
        }
    }

    return nullptr;
}


Ast_Expression *C_Converter::find_call_argument(Ast_Procedure_Call_Expression *call, 
                                                Ast_Declaration *parameter, int *next_positional_argument)
{
    if (!call || !parameter || !parameter->identifier ||
        !parameter->identifier->name || !next_positional_argument)
    {
        return nullptr;
    }

    if (!call->arguments) {
        return nullptr;
    }

    const char *parameter_name = parameter->identifier->name;

    /*
        Named arguments win by name. This makes a mixed call such as
        func(10, c=4.0, b=20) map correctly once semantic validation allows it.
    */
    FOR(call->arguments->arguments) {
        Ast_Expression *raw_argument = it;

        if (!raw_argument || raw_argument->type != AST_NAMED_ARGUMENT)
        {
            continue;
        }

        Ast_Named_Argument *named = static_cast<Ast_Named_Argument *>(raw_argument);

        if (!named->name || !named->name->name) {
            continue;
        }

        if (strcmp(named->name->name, parameter_name) == 0) {
            return named->value;
        }
    }

    
    // Positional arguments are consumed in source order, but named arguments do not occupy a positional slot.
    
    int seen_positional_arguments = 0;

    FOR(call->arguments->arguments) {
        Ast_Expression *raw_argument = it;

        if (!raw_argument || raw_argument->type == AST_NAMED_ARGUMENT)
        {
            continue;
        }

        if (seen_positional_arguments == *next_positional_argument) {
            ++(*next_positional_argument);
            return raw_argument;
        }

        ++seen_positional_arguments;
    }

    return nullptr;
}

void C_Converter::type_to_c_string(FILE *out, Ast_Type_Definition *type, Ast_Declaration *decl, bool need_semicolon, int indent, bool should_initializer){
    if(!type) return;

    std::string type_str;
    Ast_Type_Definition *current = type;
    int pointer_depth = 0;

    while (current->pointed_to_type){
        pointer_depth++;
        current = current->pointed_to_type;
    }
    bool struct_decl = false;
    if(current->type == AST_ARRAY_TYPE && current->struct_def){
        const char *struct_name = current->struct_def->name;

        if(struct_name){
            type_str = "struct ";
            struct_decl = true;
            type_str += struct_name;
        } else {
            type_str = "void /* unresolved array */";
        }

    } else if(current->type == AST_ARRAY_TYPE && current->struct_def == nullptr){
            type_str = "Static_Array";
    }
    else {
        type_str = current->to_string(*_type);
    }

    for(int i = 0; i < pointer_depth; ++i){
        type_str += " *";
    }

    if(decl){
        if(decl->identifier){
            fprintf(out, "%s %s", type_str.c_str(), decl->identifier->name);

            if(decl->initializer && !should_initializer){
                fprintf(out, " = ");

                // should not fall here normally
                // if we need a cast Dynamic_Array* to Static_Array*
                if(pointer_depth > 0 && current->type == AST_ARRAY_TYPE && current->struct_def){
                    const char *target_struct = current->struct_def->name;

                    // If initializer is address-of dynamic array, cast it
                    if(decl->initializer->type == AST_UNARY){
                        auto *unary = static_cast<Ast_Unary*>(decl->initializer);
                        if(unary->op == UNARY_ADDRESS_OF && unary->operand->inferred_type){
                            Ast_Type_Definition *operand_type = unary->operand->inferred_type;

                            if(operand_type->type == AST_ARRAY_TYPE && operand_type->struct_def){
                                const char *source_struct = operand_type->struct_def->name;

                                // Dynamic → Static view cast
                                if(strcmp(target_struct, "Static_Array") == 0 &&
                                    strcmp(source_struct, "Dynamic_Array") == 0){
                                    fprintf(out, "(struct Static_Array *)");
                                }
                            }
                        }
                    }
                }

                emitExpression(out, decl->initializer, indent);
            }
        }
        else {
            int current_idx = 0;

            FOR(decl->identifiers) {
                if(current_idx > 0)  // beacuse in emitStatement it adds indent ahead of time, but here for the first guy it will end up doing indent twice so just skip it for first identifier.
                    indentLine(out, indent);

                Ast_Type_Definition *this_type = nullptr;
                if (current_idx < decl->identifier_types.count &&
                    decl->identifier_types.data[current_idx]) {
                    this_type = decl->identifier_types.data[current_idx];
                } else {
                    this_type = decl->declared_type;
                }

                std::string this_type_str;
                Ast_Type_Definition *current = this_type;
                int ptr_depth_local = 0;
                while (current && current->pointed_to_type) {
                    ptr_depth_local++;
                    current = current->pointed_to_type;
                }

                if (current) {
                    if (current->type == AST_ARRAY_TYPE && current->struct_def == nullptr) {
                        this_type_str = "Static_Array";
                    } else {
                        this_type_str = current->to_string(*_type);
                    }
                } else {
                    this_type_str = "/*unknown type*/";
                }
                for (int p = 0; p < ptr_depth_local; ++p) {
                    this_type_str += " *";
                }

                // indentLine(out, indent);
                fprintf(out, "%s %s", this_type_str.c_str(), it->name);


                if ((decl->initializer || decl->initializers) && !should_initializer) {
                    fprintf(out, " = ");

                    Ast_Expression* expr_to_print = nullptr;

                    if (decl->initializers && decl->initializers->arguments.count > 1) {
                        expr_to_print = decl->initializers->arguments.data[current_idx];
                        current_idx++;
                    }
                    else {
                        expr_to_print = decl->initializers->arguments.data[current_idx];
                    }

                    if (expr_to_print) {
                        emitExpression(out, expr_to_print, indent);
                    }
                }

                fprintf(out, ";\n");
            }
        }

    } else {
        fprintf(out, "(%s)", type_str.c_str());
    }

    if(need_semicolon){
        fprintf(out, ";\n");
    }
}

void C_Converter::emitFunctionPrototype(FILE *out, Ast_Declaration *decl, int indent){
    if(!decl || !decl->is_function || !decl->identifier) return;

    indentLine(out, indent);
    type_to_c_string(out, decl->return_type, decl, false, indent);
    fprintf(out, "(");
    for(int i = 0; i < decl->parameters.count; ++i){
        auto *param = decl->parameters.data[i];
        if(i > 0) fprintf(out, ", ");
        type_to_c_string(out, param->declared_type, param, false, indent, true);
    }
    if(decl->parameters.count == 0){
        fprintf(out, "void");
    }
    fprintf(out, ");\n");
}

void C_Converter::emitStructPrototype(FILE *out, Ast_Statement *stmt, int indent){
    if(!stmt || !stmt->expression) return;

    fprintf(out, "struct ");
    auto *struct_def = static_cast<Ast_Struct *>(stmt->expression);
    fprintf(out, "%s;\n", struct_def->name);

}
void C_Converter::emitStruct(FILE *out, Ast_Statement *stmt, int indent){
    if(!stmt || !stmt->expression) return;

    fprintf(out, "struct ");
    auto *struct_def = static_cast<Ast_Struct *>(stmt->expression);
    fprintf(out, "%s {\n", struct_def->name);

    for(int i = 0; i < struct_def->members.count; ++i){
        auto *member = struct_def->members.data[i];
        if(!member->declared_type) fprintf(out, "/*member not inferred\n*/");
        indentLine(out, indent);
        type_to_c_string(out, member->declared_type, member, true, indent, true);
    }

    fprintf(out, "};\n");
}

void C_Converter::emitStatement(FILE *out, Ast_Statement *stmt, int indent, bool is_else_if)
{
    if(!stmt) return;
    switch (stmt->type){

        case AST_DECLARATION: {

            auto *decl = static_cast<Ast_Declaration*>(stmt);

            PRINT_DEBUG_INFO(out, "#line %d \"%s\"\n", stmt->line_number, stmt->file_name);
            if(decl->is_function){

                // fprintf(out, "\n");
                indentLine(out, indent);

                // emit return type
                type_to_c_string(out, decl->return_type, decl, false, indent);

                fprintf(out, " (");

                // emit params
                for(int i = 0; i < decl->parameters.count; ++i){
                    auto *param = decl->parameters.data[i];

                    type_to_c_string(out, param->declared_type, param, false, indent, true);

                    if(i + 1 < decl->parameters.count)
                        fprintf(out, ", ");
                }

                fprintf(out, ")");

                if(decl->is_function_body && decl->my_scope){
                    fprintf(out, " {\n");
                    emitBlock(out, decl->my_scope, indent);
                    fprintf(out, "}\n\n");
                } else {
                    fprintf(out, ";\n");
                }

                break;
            }

            if (decl->identifiers.count > 0 && decl->declared_type && decl->declared_type->type == AST_ARRAY_TYPE) {

                Ast_Array_Type *arr = static_cast<Ast_Array_Type*>(decl->declared_type);

                if (!arr->is_resizable && arr->size_expr && arr->size_expr->type == AST_LITERAL)
                {
                    long long size = 0;
                    Ast_Literal *lit = static_cast<Ast_Literal*>(arr->size_expr);
                    if (lit->value_type == LITERAL_NUMBER) {
                        size = lit->integer_value;
                    }

                    Ast_Type_Definition *elem = arr->element_type;
                    int elem_ptr_depth = 0;
                    while (elem && elem->pointed_to_type) {
                        elem_ptr_depth++;
                        elem = elem->pointed_to_type;
                    }

                    const char *elem_type_str = elem ? elem->to_string(*_type) : "void";

                    // For each identifier: emit backing array + Static_Array header
                    for (int i = 0; i < decl->identifiers.count; ++i) {
                        Ast_Ident *id = decl->identifiers.data[i];
                        if (!id || !id->name) continue;

                        indentLine(out, indent);
                        fprintf(out, "%s", elem_type_str);
                        for (int p = 0; p < elem_ptr_depth; ++p) {
                            fprintf(out, "*");
                        }
                        fprintf(out, " __data__%s[%lld];\n", id->name, size);

                        indentLine(out, indent);
                        fprintf(out, "Static_Array %s;\n", id->name);

                        if (!decl->my_scope->is_global_scope) {

                            indentLine(out, indent);
                            fprintf(out, "%s.data = (void *)__data__%s;\n", id->name, id->name);

                            indentLine(out, indent);
                            fprintf(out, "%s.count = %lld;\n", id->name, size);

                            if (arr->element_type && arr->element_type->struct_def) {
                                Ast_Struct *st = arr->element_type->struct_def;
                                indentLine(out, indent);
                                fprintf(out, "for(int _i=0; _i < %lld; ++_i) _init_%s(&((%s*)__data__%s)[_i]);\n",
                                        size, st->name, st->name, id->name);
                            }
                        }
                    }
                    break;
                }
            }
            indentLine(out, indent);

            // static array declarations
            Ast_Type_Definition *base_type = decl->declared_type;
            int ptr_depth = 0;
            Ast_Type_Definition *decl_type = base_type;
            while (base_type && base_type->pointed_to_type){
                ptr_depth++;
                base_type = base_type->pointed_to_type;
            }

            // if its not behind a pointer
            if(ptr_depth == 0 && base_type && base_type->type == AST_ARRAY_TYPE/* && base_type->struct_def*/){

                auto *arr = static_cast<Ast_Array_Type*>(base_type);

                if(!arr->is_resizable && arr->size_expr && arr->size_expr->type == AST_LITERAL
                    /*base_type->struct_def->name &&
                    strcmp(base_type->struct_def->name, "Static_Array") == 0*/){

                    long long size = 0;
                    // if(arr->size_expr->type == AST_LITERAL){
                        Ast_Literal *lit = static_cast<Ast_Literal*>(arr->size_expr);
                        if(lit->value_type == LITERAL_NUMBER){
                            size = lit->integer_value;
                        }
                    // }


                    // this part should really be inside type_to_c_string()
                    const char *elem_type_str = "void";

                    int ptr_depth_2 = 0;
                    Ast_Type_Definition *base2 = arr->element_type;

                    while (base2->pointed_to_type){
                        ptr_depth_2++;
                        base2 = base2->pointed_to_type;
                    }

                    elem_type_str = base2->to_string(*_type);

                    fprintf(out, "%s", elem_type_str);
                    for(int i = 0; i < ptr_depth_2; i++){
                        fprintf(out, "*");
                    }

                    fprintf(out, " __data__%s[%lld];\n",
                           decl->identifier->name,
                           size);

                    indentLine(out, indent);

                    fprintf(out, "Static_Array %s;\n", decl->identifier->name);

                    if(decl->my_scope->is_global_scope == false){
                        indentLine(out, indent);

                        fprintf(out, "%s.data = (void *)__data__%s;\n",
                               decl->identifier->name,
                               decl->identifier->name);

                        indentLine(out, indent);

                        fprintf(out, "%s.count = %lld;\n", decl->identifier->name, size);

                        if(arr->element_type && arr->element_type->struct_def){
                            Ast_Struct *st = arr->element_type->struct_def;
                            indentLine(out, indent);
                            // Loop through the BACKING array directly
                            fprintf(out, "for(int _i=0; _i < %lld; ++_i) _init_%s(&((%s*)__data__%s)[_i]);\n",
                                    size, st->name, st->name, decl->identifier->name);
                        }
                    }

                    break;
                }
            }

            // Check if this is struct with array members
            if(decl_type && decl_type->struct_def)
            {
                Ast_Struct *struct_def = decl_type->struct_def;

                for(int i = 0; i < struct_def->members.count; i++){
                    Ast_Declaration *member = struct_def->members.data[i];
                    if(!member || !member->declared_type) continue;

                    if(member->declared_type->type == AST_ARRAY_TYPE){
                        auto *arr = static_cast<Ast_Array_Type*>(member->declared_type);

                        if(!arr->is_resizable && arr->size_expr && arr->size_expr->type == AST_LITERAL){
                            long long size = static_cast<Ast_Literal*>(arr->size_expr)->integer_value;

                            std::string elem_type_str = "void";
                            if(arr->element_type){
                                Ast_Type_Definition *elem = arr->element_type;
                                int elem_ptr_depth = 0;

                                while(elem->pointed_to_type){
                                    elem_ptr_depth++;
                                    elem = elem->pointed_to_type;
                                }

                                elem_type_str = elem->to_string(*_type);
                                for(int p = 0; p < elem_ptr_depth; ++p){
                                    elem_type_str += "*";
                                }
                            }

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

            // type_to_c_string(out, decl->declared_type, decl, true, indent);

            type_to_c_string(out, decl->declared_type ? decl->declared_type : (decl->identifier_types.count > 0 ? decl->identifier_types.data[0] : nullptr),
                             decl, true, indent);
            if(!decl->my_scope->is_global_scope && ptr_depth == 0)
            {
                if(base_type && base_type->struct_def)
                {
                    Ast_Struct *struct_def = base_type->struct_def;
                    for(int i = 0; i < struct_def->members.count; i++)
                    {
                         Ast_Declaration *member = struct_def->members.data[i];

                         if(member->declared_type && member->declared_type->type == AST_ARRAY_TYPE)
                         {
                             auto *arr = static_cast<Ast_Array_Type*>(member->declared_type);

                             if(!arr->is_resizable && arr->size_expr && arr->size_expr->type == AST_LITERAL)
                             {
                                 indentLine(out, indent);

                                 fprintf(out, "%s.%s.data = (void *)__data__%s_%s;\n",
                                        decl->identifier->name, member->identifier->name,
                                        decl->identifier->name, member->identifier->name);

                                 indentLine(out, indent);

                                 fprintf(out, "%s.%s.count = %lld;\n",
                                        decl->identifier->name, member->identifier->name,
                                        static_cast<Ast_Literal*>(arr->size_expr)->integer_value);
                             }
                         }
                    }
                }

                if(base_type && base_type->struct_def){
                    Ast_Struct *st = base_type->struct_def;
                    indentLine(out, indent);
                    fprintf(out, "_init_%s(&%s);\n", st->name, decl->identifier->name);
                }
            }
            break;
        }

        case AST_STATEMENT: {

            // PRINT_DEBUG_INFO(out, "#line %d \"%s\"\n", stmt->line_number, stmt->file_name);

            if(stmt->expression){
                indentLine(out, indent);
                if(stmt->is_return == true){
                    fprintf(out, "return ");
                }
                emitExpression(out, stmt->expression, indent);

                fprintf(out, ";\n");
            }
            else if(stmt->is_return){ // if its not an expression but its return statement then its a return to void
                indentLine(out, indent);
                fprintf(out, "return;\n");
            }
            else if(stmt->block){

                PRINT_DEBUG_INFO(out, "#line %d \"%s\"\n", stmt->line_number, stmt->file_name);

                indentLine(out, indent);
                fprintf(out, "{\n");

                emitBlock(out, stmt->block, indent);
                fprintf(out, "}\n");

            }
            // else {
            //     indentLine(out, indent);
            //     fprintf(out, "/* Reached neither */\n");
            // }
            break;
        }


        case AST_IF: {

            if(!is_else_if){
                PRINT_DEBUG_INFO(out, "#line %d \"%s\"\n", stmt->line_number, stmt->file_name);
            }
            auto *ifstmt = static_cast<Ast_If*>(stmt);

            if(!is_else_if) indentLine(out, indent);

            fprintf(out, "if(");
            emitExpression(out, ifstmt->condition, indent);
            fprintf(out, ")");

            fprintf(out, "{\n");
            emitBlock(out, ifstmt->then_block, indent);
            fprintf(out, "}\n");

            if(ifstmt->else_block){
                indentLine(out, indent);
                fprintf(out, "else ");

                if(ifstmt->else_block->type == AST_IF){
                    emitStatement(out, ifstmt->else_block, indent, true); // else if
                } else if(ifstmt->else_block->type == AST_BLOCK){
                    fprintf(out, "{\n");
                    emitBlock(out, static_cast<Ast_Block*>(ifstmt->else_block), indent); // else
                    fprintf(out, "}\n");
                }
            }
            break;
        }

        case AST_WHILE: {

            PRINT_DEBUG_INFO(out, "#line %d \"%s\"\n", stmt->line_number, stmt->file_name);

            auto *while_stmt = static_cast<Ast_While*>(stmt);
            indentLine(out, indent);
            fprintf(out, "while(");
            emitExpression(out, while_stmt->condition, indent);
            fprintf(out, "){\n");
            emitBlock(out, while_stmt->block, indent);
            fprintf(out, "}\n");

            break;
        }
        case AST_BREAK: {
            indentLine(out, indent);
            fprintf(out, "break;");
            break;
        }

        default:
            indentLine(out, indent);
            fprintf(out, "// [unhandled statement]\n");
            break;
    }
}

void C_Converter::emitBlock(FILE *out, Ast_Block *block, int indent)
{
    if(!block) return;

    // fprintf(out, "{\n");

    for(int i = 0; i < block->statements.count; i++){
        Ast_Statement *stmt = block->statements.data[i];

        emitStatement(out, stmt, indent+4);
    }


    indentLine(out, indent);
    // fprintf(out, "}\n");
}

struct Struct_Dependency {
    Ast_Struct* key;
    Array<Ast_Struct*> values;
};

bool contains_struct(Array<Ast_Struct*>& arr, Ast_Struct* s) {
    for(long i = 0; i < arr.count; ++i) {
        if(arr.data[i] == s) return true;
    }
    return false;
}

Struct_Dependency* find_dependency(Array<Struct_Dependency>& deps,
                                          Ast_Struct* s) {
    for(long i = 0; i < deps.count; ++i) {
        if(deps.data[i].key == s)
            return &deps.data[i];
    }
    return nullptr;
}
void visit_struct(Ast_Statement* stmt, Array<Ast_Statement*>& structs, Array<Struct_Dependency>& dependencies,
                         Array<Ast_Struct*>& visited, Array<Ast_Struct*>& in_progress,Array<Ast_Statement*>& result)
{
    Ast_Struct *s = static_cast<Ast_Struct*>(stmt->expression);

    if(contains_struct(visited, s))
        return;

    if(contains_struct(in_progress, s)) {
        fprintf(stderr,
            "%s[%d:%d]: Circular struct dependency involving '%s'\n\n",
            s->file_name, s->line_number, s->character_number, s->name ? s->name : "(unknown)");
        fprintf(stderr, "There were errors. Exiting...\n");
        exit(1);
    }

    in_progress.push_back(s);

    Struct_Dependency* dep = find_dependency(dependencies, s);
    if(dep) {
        for(long i = 0; i < dep->values.count; ++i) {
            Ast_Struct* needed = dep->values.data[i];

            for(long j = 0; j < structs.count; ++j) {
                Ast_Statement* dep_stmt = structs.data[j];
                Ast_Struct* candidate =
                    static_cast<Ast_Struct*>(dep_stmt->expression);

                if(candidate == needed) {
                    visit_struct(dep_stmt, structs,
                                 dependencies,
                                 visited,
                                 in_progress,
                                 result);
                    break;
                }
            }
        }
    }

    for(long i = 0; i < in_progress.count; ++i) {
        if(in_progress.data[i] == s) {
            in_progress.data[i] =
                in_progress.data[in_progress.count - 1];
            in_progress.count--;
            break;
        }
    }

    visited.push_back(s);
    result.push_back(stmt);
}

Array<Ast_Statement*>
C_Converter::topologically_sort_structs(Array<Ast_Statement*>& structs, Pool* pool)
{
    Array<Struct_Dependency> dependencies(pool);

    // Build dependency map
    for(long i = 0; i < structs.count; ++i) {
        Ast_Statement* stmt = structs.data[i];
        Ast_Struct* s =
            static_cast<Ast_Struct*>(stmt->expression);

        Struct_Dependency dep;
        dep.key = s;
        dep.values = Array<Ast_Struct*>(pool);

        for(long m = 0; m < s->members.count; ++m) {
            Ast_Declaration* member = s->members.data[m];
            if(!member || !member->declared_type)
                continue;

            Ast_Type_Definition* type =
                member->declared_type;

            if(type->pointed_to_type)
                continue;

            if(type->struct_def) {
                if(!contains_struct(dep.values, type->struct_def))
                    dep.values.push_back(type->struct_def);
            }
        }

        dependencies.push_back(dep);
    }

    Array<Ast_Statement*> result(pool);
    Array<Ast_Struct*> visited(pool);
    Array<Ast_Struct*> in_progress(pool);

    for(long i = 0; i < structs.count; ++i) {
        visit_struct(structs.data[i], structs, dependencies,
                     visited, in_progress, result);
    }

    return result;
}
void C_Converter::emit_static_init_function(FILE *out, Array<Ast_Statement*> const &vars){


    fprintf(out, "void __init_global_static_arrays(){\n");

    for(int i = 0; i < vars.count; ++i){
        Ast_Declaration* decl = static_cast<Ast_Declaration*>(vars.data[i]);
        if(!decl || !decl->declared_type) continue;

        Ast_Type_Definition* base_type = decl->declared_type;
        int ptr_depth = 0;
        while (base_type && base_type->pointed_to_type){
            ptr_depth++;
            base_type = base_type->pointed_to_type;
        }

        if(ptr_depth == 0 && base_type && base_type->type == AST_ARRAY_TYPE){
            auto* arr = static_cast<Ast_Array_Type*>(base_type);
            if(!arr->is_resizable && arr->size_expr &&
               arr->size_expr->type == AST_LITERAL){

                long long size = 0;
                auto* lit = static_cast<Ast_Literal*>(arr->size_expr);
                if(lit->value_type == LITERAL_NUMBER){
                    size = lit->integer_value;
                }

                // Multi-var global: a, b, c: [N]T;
                if (decl->identifiers.count > 0) {
                    for (int k = 0; k < decl->identifiers.count; ++k) {
                        Ast_Ident *id = decl->identifiers.data[k];
                        if (!id || !id->name) continue;

                        // backing array name: __data__<name>
                        fprintf(out, "    %s.data = (void *)__data__%s;\n",
                                id->name, id->name);
                        fprintf(out, "    %s.count = %lld;\n",
                                id->name, size);
                    }
                }
                // Single-var global: a: [N]T;
                else if (decl->identifier && decl->identifier->name) {
                    fprintf(out, "    %s.data = (void *)__data__%s;\n",
                            decl->identifier->name, decl->identifier->name);
                    fprintf(out, "    %s.count = %lld;\n",
                            decl->identifier->name, size);
                }
            }
        }
    }

    fprintf(out, "}\n\n");
}

void C_Converter::emit_struct_init_helper(FILE *out, Ast_Statement *stmt){
    if(!stmt) return;

    Ast_Struct *struct_def = nullptr;

    if(stmt->type == AST_STRUCT){
        struct_def = (Ast_Struct *)stmt;
    }
    else if(stmt->type == AST_STATEMENT && stmt->expression && stmt->expression->type == AST_STRUCT){
        struct_def = (Ast_Struct *)stmt->expression;
    }
    else if(stmt->type == AST_DECLARATION){
        auto *decl = (Ast_Declaration *)stmt;
        if(decl->initializer && decl->initializer->type == AST_STRUCT){
            struct_def = (Ast_Struct *)decl->initializer;
        }
    }

    if(!struct_def) return;

    fprintf(out, "inline void _init_%s(%s* self){\n", struct_def->name, struct_def->name);

    for(int i = 0; i < struct_def->members.count; ++i){
        Ast_Declaration *member = struct_def->members.data[i];

        if(member->initializer){
            fprintf(out, "    self->%s = ", member->identifier->name);
            emitExpression(out, member->initializer);
            fprintf(out, ";\n");
        }
        else if(member->declared_type && member->declared_type->struct_def){
             Ast_Struct *inner_struct = member->declared_type->struct_def;
             fprintf(out, "    _init_%s(&self->%s);\n", inner_struct->name, member->identifier->name);
        }
        else if(member->declared_type && member->declared_type->type == AST_ARRAY_TYPE){
             auto *arr_type = (Ast_Array_Type*)member->declared_type;

             if(arr_type->element_type && arr_type->element_type->struct_def){
                 Ast_Struct *inner_struct = arr_type->element_type->struct_def;

                 fprintf(out, "    for(int _i=0; _i < ");
                 emitExpression(out, arr_type->size_expr);
                 fprintf(out, "; ++_i) ");
                 fprintf(out, "_init_%s(&((%s*)self->%s.data)[_i]);\n",
                                inner_struct->name,
                                inner_struct->name,
                                member->identifier->name);
             }
        }
    }

    fprintf(out, "}\n\n");
}

void C_Converter::generate_cpp_code(const char *filename, Ast_Block *program)
{
    FILE *out = nullptr;

#ifdef _WIN32
    fopen_s(&out, filename, "w");
#elif __linux
    out = fopen64(filename, "w");
#endif
    if(!out){
        printf("Failed to open file: %s\n", filename);
        return;
    }

    fprintf(out, "%s", BOILTERPLATE_TOP);

#ifdef _WIN32
    fprintf(out, "%s", WINDOWS_RUNTIME_CRASH_HANDLER_HELPER);
#endif

    Array<Ast_Statement*> structs;
    Array<Ast_Statement*> vars;
    Array<Ast_Declaration*> functions;

    structs = interp->pool;
    vars = interp->pool;
    functions = interp->pool;

    for(int i = 0; i < program->statements.count; i++){
        Ast_Statement *stmt = program->statements.data[i];
        if(!stmt) continue;
        if(stmt->type == AST_DECLARATION){
            Ast_Declaration *decl = static_cast<Ast_Declaration*>(stmt);
            if(decl->is_function && decl->is_function_body && !decl->is_foreign){
                functions.push_back(decl);
            }
            else {
                if(!decl->is_foreign)
                    vars.push_back(decl);
            }
        } else if(stmt->expression && stmt->expression->type == AST_STRUCT){
            structs.push_back(stmt);
        }

    }

    Array<Ast_Statement*> sorted_structs = topologically_sort_structs(structs, interp->pool);

    fprintf(out, "/*STRUCT FORWARD DECLARATIONS*/\n");
    FOR(sorted_structs){
        emitStructPrototype(out, it, 0);
    }
    fprintf(out, "\n");


    fprintf(out, "/*GLOBAL FUNCTION FORWARD DECLARATIONS*/\n");
    FOR(functions){
        emitFunctionPrototype(out, it, 0);
    }
    fprintf(out, "\n");


    fprintf(out, "/*STRUCTS DEFINITIONS*/\n");
    FOR(sorted_structs){
        emitStruct(out, it, 4);
    }
    fprintf(out, "\n");

    fprintf(out, "/*BSS SECTION GLOBAL VARIAABLES*/\n");
    FOR(vars){
        emitStatement(out, it, 0);
    }
    fprintf(out, "\n");

    emit_static_init_function(out, vars);
    FOR(sorted_structs){
        emit_struct_init_helper(out, it);
    }


    fprintf(out, "/*FUNCTION BODIES*/\n");
    FOR(functions){
        emitStatement(out, it, 0);
    }
    fprintf(out, "\n");



    Ast_Block *mainBlock = nullptr;

    for(int i = 0; i < program->statements.count; i++){
        Ast_Statement *stmt = program->statements.data[i];

        if(stmt && stmt->block && stmt->block->is_entry_point){
            mainBlock = stmt->block;
            break;
        }
    }

    if(!mainBlock){
        printf("No main block found in AST\n");
        fclose(out);
        return;
    }
    // -2 in the parameter below because of these two line void GENERATED_MAIN and init_global_
    PRINT_DEBUG_INFO(out, "#line %d \"%s\"", mainBlock->line_number-2, mainBlock->file_name);

    fprintf(out, "\nvoid GENERATED_MAIN()");
    fprintf(out, "{\n");
    fprintf(out, "    __init_global_static_arrays();\n");
    emitBlock(out, mainBlock, 0);
    fprintf(out, "}\n");

    fprintf(out, "\nint main(int argc, char **argv){\n");
#ifdef _WIN32
    fprintf(out, "    InstallHandler();\n");
#endif
    fprintf(out, "    GENERATED_MAIN();\n");
    fprintf(out, "    return 0;\n");
    fprintf(out, "}\n");

    fclose(out);
}
