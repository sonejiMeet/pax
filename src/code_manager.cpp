
#ifdef AST_NEW
#undef AST_NEW
#endif

#define AST_NEW(type) ([&]() -> type *{                                              \
    assert(interp->pool != nullptr && "Pool must not be null");                      \
    void *mem = pool_alloc_debug(interp->pool, sizeof(type), #type, "CODE_MANAGER"); \
    type *node = new (mem) type(interp->pool);                                       \
    node->file_name = interp->current_file;                                          \
    return node;                                                                     \
}())

#define FOR(type)                                             \
    for(int it_index=0; it_index < (type).count; ++it_index)  \
        for(auto *it = (type).data[it_index]; it; it=nullptr)

CodeManager::CodeManager(Pax_Interp *_interp)
{
    interp = _interp;

    scope_stack = interp->pool; // have to pass in the pool to the Array<>

    Ast_Block *block = AST_NEW(Ast_Block);
    block->is_global_scope = true;
    scope_stack.push_back(block); // global scope

    _type = interp->type;  // we use it many times, make a copy here

    // have to pass in the pool to Array<>
    unresolved_calls = interp->pool;
    unresolved_vars = interp->pool;
    unresolved_types = interp->pool;
    unresolved_member_accesses = interp->pool;
}

Ast_Literal *CodeManager::make_integer_literal(long long value){
    Ast_Literal *literal = AST_NEW(Ast_Literal);
    literal->value_type = LITERAL_NUMBER;
    literal->integer_value = value;
    return literal;
}

#define report_error(node, ...)                                 \
        report_error_impl(static_cast<Ast*>(node), __VA_ARGS__) \

void CodeManager::report_error_impl(Ast *ast, const char *fmt, ...)
{
    constexpr size_t BUFFER_SIZE = 512;
    char buffer[BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, args);
    va_end(args);

    count_errors += 1;

    const char *filename = ast->file_name;
    if (!filename || !filename[0]) {
        filename = interp->current_file ? interp->current_file : "<unknown>";
    }

    if (ast->line_number >= 0 && ast->character_number >= 0) {
        fprintf(stderr, "%s[%d:%d]: %s\n", filename, ast->line_number, ast->character_number, buffer);
    } else {
        fprintf(stderr, "%s: %s\n", filename, buffer);
    }
}

#define report_error_with_previous(node, prev, ...) \
        report_error_with_previous_impl(            \
        static_cast<Ast*>(node),                    \
        static_cast<Ast*>(prev),                    \
        __VA_ARGS__)                                \

void CodeManager::report_error_with_previous_impl(Ast *ast_node, Ast *ast_prev, const char *fmt, ...) {
    constexpr size_t BUFFER_SIZE = 512;
    char buffer[BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, args);
    va_end(args);

    count_errors += 1;

    // Ast *ast_node = static_cast<Ast*>(node);
    // Ast *ast_prev = static_cast<Ast*>(previous);

    const char *filename = ast_node->file_name;
    if (!filename || !filename[0]) {
        filename = interp->current_file ? interp->current_file : "<unknown>";
    }

    if (ast_node->line_number >= 0 && ast_node->character_number >= 0) {
        fprintf(stderr, "%s\n%s:[%d:%d]: %s.",
                        "\x1B[0;36m", filename, ast_node->line_number, ast_node->character_number, buffer);
    } else {
        fprintf(stderr, "%s\n%s: %s.", "\x1B[0;36m", filename, buffer);
    }

    const char *prev_filename = ast_prev->file_name;
    if (!prev_filename || !prev_filename[0]) {
        prev_filename = interp->current_file ? interp->current_file : "<unknown>";
    }

    if (ast_prev->line_number >= 0 && ast_prev->character_number >= 0) {
        fprintf(stderr, " Previously declared at %s:[%d:%d]\n\n%s",
                prev_filename, ast_prev->line_number, ast_prev->character_number, "\x1B[0m");
    } else {
        fprintf(stderr, " Previously declared at %s: %s\n\n", prev_filename, "\x1B[0m");
    }
}


void CodeManager::push_scope()
{
    Ast_Block *block = AST_NEW(Ast_Block);
    scope_stack.push_back(block);
}

void CodeManager::pop_scope()
{
    if (scope_stack.count) scope_stack.pop_back();
}

Ast_Declaration* CodeManager::lookup_symbol(const char* name, Ast_Block* scope)
{
    if (scope) {
        // for queued phases
        Ast_Block* current = scope;
        while (current) {
            Ast_Declaration* decl = lookup_symbol_in_block(name, current);
            if (decl) return decl;
            current = current->parent;
        }
    }

    // first pass
    for (int i = scope_stack.count - 1; i >= 0; --i) {
        Ast_Declaration* decl = lookup_symbol_in_block(name, scope_stack.data[i]);
        if (decl) return decl;
    }

    return nullptr;
}


Ast_Declaration *CodeManager::lookup_symbol_current_scope(const char *name) {
    if (!scope_stack.count) return nullptr;

    Ast_Block *block = scope_stack.get_back();
    if(!block) return nullptr;

    FOR(block->statements){
        if (!it || it->type != AST_DECLARATION) continue;

        Ast_Declaration *decl = static_cast<Ast_Declaration*>(it);

        FOR(decl->identifiers){
            if (it && strcmp(it->name, name) == 0)
                return decl;
        }

        if (decl->identifier && strcmp(decl->identifier->name, name) == 0) {
            return decl;
        }
    }
    return nullptr;
}

Ast_Declaration* CodeManager::lookup_symbol_in_block(const char* name, Ast_Block* block)
{
    if (!block) return nullptr;

    FOR(block->statements){
        if (!it || it->type != AST_DECLARATION) continue;

        Ast_Declaration *decl = static_cast<Ast_Declaration*>(it);

        FOR(decl->identifiers){
            if (it && strcmp(it->name, name) == 0)
                return decl;
        }

        if (decl->identifier && strcmp(decl->identifier->name, name) == 0)
            return decl;
    }

    return nullptr;
}

bool CodeManager::declare_variable(Ast_Declaration *decl, bool force_decl) {
    if (!decl || (!decl->identifier && decl->identifiers.count == 0)) return false;

    bool is_multi = decl->identifiers.count > 0;
    Ast_Block *current_block = scope_stack.get_back();
    if (!is_multi) {
        if (!decl->identifier) return false;

        auto *looked_up = lookup_symbol_current_scope(decl->identifier->name);
        if (!force_decl && looked_up) {
            report_error_with_previous(decl, looked_up, "Variable '%s' already declared", decl->identifier->name);
            return false;
        }

        decl->initialized = (decl->initializer != nullptr);
        decl->my_scope = current_block;
        current_block->statements.push_back(static_cast<Ast_Statement*>(decl));
        return true;

    } else {
        FOR(decl->identifiers){
            const char *name = it->name;
            auto *looked_up = lookup_symbol_current_scope(name);
            if (!force_decl && looked_up) {
                report_error_with_previous(decl, looked_up, "Variable '%s' already declared", name);
                return false;
            }
        }

        decl->initialized = (decl->initializers != nullptr);
        decl->my_scope = current_block;
        current_block->statements.push_back(static_cast<Ast_Statement*>(decl));
        return true;
    }
    return true;
}

bool CodeManager::declare_function(Ast_Declaration *decl) {
    if (!decl || !decl->identifier || !decl->is_function) return false;

    // if (decl->is_function_header) return false;

    auto *looked_up = lookup_symbol_current_scope(decl->identifier->name);
    if ( looked_up && looked_up->is_function) {
        report_error_with_previous(decl, looked_up, "Function '%s' already declared", decl->identifier->name);
        return false;
    }
    else if(looked_up && !looked_up->is_function){
        report_error_with_previous(decl, looked_up, "Redefinition of '%s', previous definition is not a function", decl->identifier->name);
        return false;
    }

    Ast_Block *current_block = scope_stack.get_back();

    decl->initialized = decl->is_function_body;

    current_block->statements.push_back(static_cast<Ast_Statement*>(decl));

    return true;
}

bool CodeManager::declare_struct(Ast_Statement *struct_stmt) {
    if (!struct_stmt) return false;
    if (!struct_stmt->expression || struct_stmt->expression->type != AST_STRUCT) return false;

    auto *struct_name = struct_stmt->type_definition->struct_def->name;

    auto *looked_up = find_struct_type_in_scopes(struct_name);
    if (looked_up) {
        report_error_with_previous(struct_stmt, looked_up, "Struct '%s' already defined.", struct_name);
        return false;
    }

    if (scope_stack.count == 0) return false;
    Ast_Block *current_block = scope_stack.get_back();

    current_block->statements.push_back(struct_stmt);
    return true;
}

ReturnCheckResult CodeManager::checkReturnPathsIf(Ast_If *ifn) {
    ReturnCheckResult badResult = {false, false};
    ReturnCheckResult then_result = ifn->then_block ? checkReturnPaths(ifn->then_block) : badResult;

    ReturnCheckResult else_result = badResult;
    if (ifn->else_block) {
        if (ifn->else_block->type == AST_IF) {
            else_result = checkReturnPathsIf(static_cast<Ast_If*>(ifn->else_block));
        } else if (ifn->else_block->type == AST_BLOCK) {
            else_result = checkReturnPaths(static_cast<Ast_Block*>(ifn->else_block));
        }
    }

    ReturnCheckResult result;
    result.has_return = then_result.has_return || else_result.has_return;
    result.all_paths_return = then_result.all_paths_return && else_result.all_paths_return;
    return result;
}

ReturnCheckResult CodeManager::checkReturnPaths(Ast_Block *block)
{
    ReturnCheckResult result = {false, false};
    if (!block) return result;

    bool fallthrough = true;

    FOR(block->statements){
        if (!it || !fallthrough) continue;

        if (it->is_return) {
            result.has_return = true;
            fallthrough = false;
            break;
        } else if (it->type == AST_IF) {
            Ast_If *ifn = static_cast<Ast_If*>(it);

            ReturnCheckResult if_result = checkReturnPathsIf(ifn);

            result.has_return |= if_result.has_return;

            if (if_result.all_paths_return) {
                fallthrough = false;
                break;
            }
        } else if (it->block && it->block->is_scoped_block) {
            // recurse normal blocks/scoped
            ReturnCheckResult block_result = checkReturnPaths(it->block);
            result.has_return |= block_result.has_return;
            if (block_result.all_paths_return) {
                fallthrough = false;
                break;
            }
        }
        // add AST_WHILE
    }

    result.all_paths_return = !fallthrough;
    return result;
}

void CodeManager::checkFunctionReturns(Ast_Declaration *decl) {
    if (decl->return_type == _type->type_def_void) return;

    ReturnCheckResult result = checkReturnPaths(decl->my_scope);

    if (!result.has_return) {
        report_error(decl, "Non-void function '%s' must have a return statement", decl->identifier->name);
        return;
    }

    if (!result.all_paths_return) {
        // checkin maybe should just be a warning instead of error, but c++ will end up complaining. Or maybe just supress that shit through supressing that warning flag?
        report_error(decl, "Not all control paths return a value in non-void function '%s'", decl->identifier->name);
    }
}

void CodeManager::resolve_idents(Ast_Block *block) {
    if (!block) return;

    bool is_global_scope = (scope_stack.count == 1);

    for (int i = 0; i < block->statements.count; i++) {

        Ast_Statement *stmt = block->statements.data[i];
        if (!stmt) continue;

        if (stmt->is_return) {
            resolve_idents_in_expr(stmt->expression);
            continue;
        }

        if (stmt->expression && stmt->expression->type == AST_STRUCT){
            declare_struct(stmt);
            resolve_idents_in_expr(stmt->expression);

        } else if (stmt->type == AST_DECLARATION){

            Ast_Declaration *decl = static_cast<Ast_Declaration*>(stmt);
            if (is_global_scope) {

                if (decl->is_function) {
                    declare_function(decl);
                    if (decl->is_function_body) {
                        if (!decl->return_type) {
                            report_error(decl, "Function '%s' must specify a return type", decl->identifier->name);
                        } else if (decl->return_type != _type->type_def_void && decl->my_scope) {
                            checkFunctionReturns(decl);
                        }
                    }
                } else {
                    declare_variable(decl);
                }

                resolve_idents_in_declaration(decl);
                if (decl->is_function && decl->my_scope && decl->is_function_body) {
                    push_scope();

                    FOR(decl->parameters){
                        declare_variable(it);
                        resolve_idents_in_declaration(it);
                    }
                    resolve_idents(decl->my_scope);

                    pop_scope();
                }
            } else {
                // Non-global scope
                if (decl->is_function) {
                    declare_function(decl);
                    if (decl->my_scope && decl->is_function_body) {
                        push_scope();
                        FOR(decl->parameters){
                            declare_variable(it);
                            resolve_idents_in_declaration(it);
                        }
                        resolve_idents(decl->my_scope);
                        pop_scope();
                    }
                } else {
                    if(decl->identifier) {
                        Ast_Declaration *is_decl = lookup_symbol_current_scope(decl->identifier->name);
                        if (!is_decl) {
                            declare_variable(decl, true);
                        }
                        else {
                            // printf("%p \n %p \n", decl, is_decl);
                            report_error_with_previous(decl, is_decl, "Variable '%s' already declared.", decl->identifier->name);
                        }
                    }
                    else {
                        bool collision = false;
                        FOR(decl->identifiers) {
                            Ast_Declaration *is_decl = lookup_symbol_current_scope(it->name);

                            if (is_decl && is_decl != decl) {
                                report_error(decl, "Variable '%s' already declared.", it->name);
                                collision = true;
                            }

                            if (!collision) {
                                declare_variable(decl, true);
                            }
                        }
                    }

                    if (!decl->is_function && decl->initializers && decl->identifiers.count > 0) {
                        FOR(decl->initializers->arguments) {
                            resolve_idents_in_expr(it, block);
                        }
                        continue;
                    }

                    bool should_queue = true;
                    if (decl->declared_type && !decl->is_function) {
                        resolve_idents_in_declaration(decl);
                        Ast_Type_Definition *base = get_base_type(decl->declared_type);

                        if (base && base->is_unresolved) {
                            should_queue = false; // already queued inside resolve_idents_in_declaration
                        }

                        if (base && base->struct_def) {
                            decl->declared_type = clone_type_definition(decl->declared_type);
                            create_type_instantiation(decl->declared_type);
                        }
                    }
                    else if (decl->declared_type && (decl->declared_type->is_unresolved || decl->declared_type->pointed_to_type))
                    {
                        Ast_Type_Definition *base = get_base_type(decl->declared_type);

                        if (base->is_unresolved && base->name) {
                            Ast_Type_Definition *def = find_struct_type_in_scopes(base->name);
                            if (def && def->struct_def) {
                                base->struct_def = def->struct_def;
                                base->is_unresolved = false;

                                decl->declared_type = clone_type_definition(decl->declared_type);
                                create_type_instantiation(decl->declared_type);
                            } else {
                                if(should_queue)
                                    push_unresolved_type(decl, base);
                            }
                        }
                    }
                    else if (decl->declared_type && decl->declared_type->type == AST_ARRAY_TYPE) {
                        auto *arr = static_cast<Ast_Array_Type*>(decl->declared_type);
                        resolve_array_types(arr, decl);
                    }
                }
            }
            continue;
        }

        if (is_global_scope) {
            if(stmt->block && !stmt->block->is_entry_point && !stmt->block->is_scoped_block) {
                report_error(stmt, "Non declaration statements in global scope are not allowed"); // this should be caught in parser but just in case...
            }
            else if (stmt->block){
                // main entry point
                push_scope();
                resolve_idents(stmt->block);
                pop_scope();
            }
            continue;
        }

        if (stmt->type == AST_IF) {
            resolve_idents_if(static_cast<Ast_If*>(stmt));
        } else if (stmt->expression) {
            resolve_idents_in_expr(stmt->expression);
        } else if (stmt->block) {
            push_scope();
            resolve_idents(stmt->block);
            pop_scope();
        } else if (stmt->type == AST_WHILE){
            Ast_While *_while = static_cast<Ast_While*>(stmt);
            if (_while->condition) resolve_idents_in_expr(_while->condition);
            if (_while->block) {
                push_scope();
                resolve_idents(_while->block);
                pop_scope();
            }
        }
    }

}

void CodeManager::resolve_idents_if(Ast_If *ifn) {
    if (ifn->condition) resolve_idents_in_expr(ifn->condition);

    if (ifn->then_block) {
        push_scope();
        resolve_idents(ifn->then_block);
        pop_scope();
    }

    if (ifn->else_block) {
        if (ifn->else_block->type == AST_IF) {
            // Recursively handle else if
            resolve_idents_if(static_cast<Ast_If*>(ifn->else_block));
        } else if (ifn->else_block->type == AST_BLOCK) {
            push_scope();
            resolve_idents(static_cast<Ast_Block*>(ifn->else_block));
            pop_scope();
        }
    }
}

Ast_Type_Definition *CodeManager::get_base_type(Ast_Type_Definition *type)
{
    if (!type) return nullptr;

    Ast_Type_Definition *t = type;
    while (true) {
        if (t->pointed_to_type) {
            t = t->pointed_to_type;
            continue;
        }
        if (t->type == AST_ARRAY_TYPE) {
            Ast_Array_Type *arr = static_cast<Ast_Array_Type*>(t);
            if (arr->element_type) {
                t = arr->element_type;
                continue;
            }
        }
        break;
    }
    return t;
}

Ast_Type_Definition *CodeManager::clone_type_definition(Ast_Type_Definition *original) {
    if (!original) return nullptr;

    // If it's a plain builtin/user type with no pointer/array, just reuse
    if (!original->pointed_to_type && original->type != AST_ARRAY_TYPE) {
        return original;
    }

    Ast_Type_Definition *clone = nullptr;

    if (original->type == AST_ARRAY_TYPE) {
        auto *orig_arr = static_cast<Ast_Array_Type*>(original);
        auto *arr_clone = AST_NEW(Ast_Array_Type);

        arr_clone->line_number      = original->line_number;
        arr_clone->character_number = original->character_number;

        arr_clone->struct_def   = original->struct_def;
        arr_clone->name         = original->name;
        arr_clone->is_reference = original->is_reference;
        arr_clone->is_unresolved = original->is_unresolved;
        arr_clone->type_instance = nullptr;

        // array-specific
        arr_clone->is_resizable = orig_arr->is_resizable;
        arr_clone->size_expr    = orig_arr->size_expr;
        arr_clone->element_type = clone_type_definition(orig_arr->element_type);

        // pointer chain above the array (if any)
        arr_clone->pointed_to_type = clone_type_definition(original->pointed_to_type);

        clone = arr_clone;
    } else {
        // Normal non-array type
        clone = AST_NEW(Ast_Type_Definition);

        clone->line_number = original->line_number;
        clone->character_number = original->character_number;

        clone->struct_def = original->struct_def;
        clone->name = original->name;
        clone->is_reference = original->is_reference;
        clone->is_unresolved = original->is_unresolved;
        clone->type_instance = nullptr;

        clone->pointed_to_type = clone_type_definition(original->pointed_to_type);
    }

    return clone;
}

void CodeManager::create_type_instantiation(Ast_Type_Definition *type) {
    if (!type) return;

    Ast_Type_Definition *base_type = type;
    while (base_type->pointed_to_type) {
        base_type = base_type->pointed_to_type;
    }

    if (!base_type->struct_def) return;
    if (base_type->type_instance) return;

    Ast_Type_Instantiation *instance = AST_NEW(Ast_Type_Instantiation);
    base_type->type_instance = instance;

    Ast_Struct *struct_def = base_type->struct_def;
    FOR(struct_def->members){
        Ast_Declaration *instance_member = AST_NEW(Ast_Declaration);
        instance_member->identifier = it->identifier;

        if (it->declared_type) {
            instance_member->declared_type = clone_type_definition(it->declared_type);

            // ensure arrays in members have struct_def set, otherwise it
            if (instance_member->declared_type->type == AST_ARRAY_TYPE &&
                !instance_member->declared_type->struct_def) {

                auto *arr = static_cast<Ast_Array_Type*>(instance_member->declared_type);
                const char *struct_name = nullptr;

                if (arr->is_resizable) {
                    struct_name = "Dynamic_Array";
                } else if (arr->size_expr) {
                    struct_name = "Static_Array";
                }

                if (struct_name) {
                    Ast_Type_Definition *array_struct = find_struct_type_in_scopes(struct_name);
                    if (array_struct && array_struct->struct_def) {
                        instance_member->declared_type->struct_def = array_struct->struct_def;
                        instance_member->declared_type->is_unresolved = false;
                    }
                }
            }
        } else if (it->initializer) {
            instance_member->declared_type = nullptr;
            instance_member->inferred = false;
        } else {
            instance_member->declared_type = nullptr;
        }

        instance_member->initializer = it->initializer;
        instance_member->initialized = false;
        instance_member->is_declaration_function_argument = false;

        if (instance_member->declared_type) {
            Ast_Type_Definition *member_base = get_base_type(instance_member->declared_type);

            if (member_base->name && !member_base->struct_def) {
                Ast_Type_Definition *resolved = find_struct_type_in_scopes(member_base->name);
                if (resolved && resolved->struct_def) {
                    member_base->struct_def = resolved->struct_def;
                    member_base->is_unresolved = false;
                }
            }

            create_type_instantiation(instance_member->declared_type);
        }

        instance->member_instances.push_back(instance_member);
    }

}

void CodeManager::try_resolve_type_on_decl(Ast_Declaration *owner, Ast_Type_Definition *&ty)
{
    if (!owner || !ty) return;

    Ast_Type_Definition *base = get_base_type(ty);
    if (!base || !base->is_unresolved || !base->name) return;

    Ast_Type_Definition *def = find_struct_type_in_scopes(base->name);
    if (def && def->struct_def) {
        base->struct_def    = def->struct_def;
        base->is_unresolved = false;

        ty = clone_type_definition(ty);
        create_type_instantiation(ty);
    } else {
        push_unresolved_type(owner, base);
    }
}

void CodeManager::resolve_idents_in_declaration(Ast_Declaration *decl)
{
    if (!decl) return;

    if (decl->initializer) {
        resolve_idents_in_expr(decl->initializer);
    }

    if (decl->declared_type) {
        if (decl->declared_type->type == AST_ARRAY_TYPE) {
            auto *arr_type = static_cast<Ast_Array_Type*>(decl->declared_type);
            resolve_array_types(arr_type, decl);
        }
        else if (decl->declared_type->pointed_to_type) {
            // pointer-to-array case
            Ast_Array_Type *arr_type = ast_static_cast<Ast_Array_Type>(decl->declared_type, AST_ARRAY_TYPE);

            if (!arr_type) {
                Ast_Type_Definition *walker = decl->declared_type;
                while (walker) {
                    if (walker->type == AST_ARRAY_TYPE) {
                        arr_type = static_cast<Ast_Array_Type*>(walker);
                        break;
                    }
                    walker = walker->pointed_to_type;
                }
            }

            if (arr_type && !arr_type->struct_def) {
                resolve_array_types(arr_type, decl);
            }
        }

        try_resolve_type_on_decl(decl, decl->declared_type);
    }

    if (decl->return_type) {
        try_resolve_type_on_decl(decl, decl->return_type);
    }
}

Ast_Type_Definition *CodeManager::find_struct_type_in_scopes(const char *name) const {
    if (!name) return nullptr;

    for (int i = (int)scope_stack.count - 1; i >= 0; --i) {
        Ast_Block *block = scope_stack.data[i];

        if (!block) continue;

        FOR(block->statements){
            if (!it) continue;
            if (it->expression && it->expression->type == AST_STRUCT && it->type_definition) {
                Ast_Struct *expr = static_cast<Ast_Struct*>(it->expression);
                if (expr->name && strcmp(expr->name, name) == 0) {
                    return it->type_definition;
                }
            }
        }
    }
    return nullptr;
}

// same as AST_NEW but we dont have file_name so can't set it
#define UNRESOLVED_NEW(type) ([&]() -> type* {                  \
    assert(interp->pool != nullptr && "Pool must not be null"); \
    void *mem = pool_alloc_debug(interp->pool, sizeof(type), #type, "UNRESOLVED_CODE_MANAGER");         \
    return new (mem) type;                                      \
}())

void CodeManager::resolve_unresolved_vars()
{
    Array<Unresolved_Variable*> still_unresolved;
    still_unresolved = interp->pool;

    FOR(unresolved_vars) {
        Ast_Declaration *decl = lookup_symbol(it->ident->name);
        if (!decl) {
            Unresolved_Variable *u = UNRESOLVED_NEW(Unresolved_Variable);
            u->ident = it->ident;
            u->my_scope = it->my_scope;
            still_unresolved.push_back(u);
            continue;
        }
    }

    unresolved_vars = still_unresolved;

    if (scope_stack.count == 1 && unresolved_vars.count != 0) {
        FOR(unresolved_vars){
            report_error(it->ident, "Use of undeclared variable '%s'", it->ident->name);
        }
    }
}


void CodeManager::resolve_unresolved_calls()
{
    Array<Unresolved_Call*> still_unresolved;
    still_unresolved = interp->pool;

    FOR(unresolved_calls) {
        auto *my_scope = it->my_scope;
        Ast_Procedure_Call_Expression *call = it->call;

        auto *fn = static_cast<Ast_Ident*>(call->function);
        Ast_Declaration *decl= lookup_symbol(fn->name);

        if (!decl) {
            Unresolved_Call *u = UNRESOLVED_NEW(Unresolved_Call);
            u->call = it->call;
            u->my_scope = it->my_scope;
            still_unresolved.push_back(u);
            continue;
        }

        if (!decl->is_function) {
            report_error(call, "'%s' is not a function", fn->name);
            continue;
        }

        int call_arg_count = call->arguments ? call->arguments->arguments.count : 0;
        int decl_arg_count = decl->parameters.count;
        if (call_arg_count != decl_arg_count) {
            report_error(call, "Function '%s' expects %d arguments, but %d were provided",
                         fn->name, decl_arg_count, call_arg_count);
            continue;
        }
        // we infer args later on....
    }

    unresolved_calls = still_unresolved;

    if (scope_stack.count == 1 && unresolved_calls.count != 0) {
        FOR(unresolved_calls) {
            auto *fn = static_cast<Ast_Ident*>(it->call->function);
            report_error(it->call, "Call to undeclared function '%s'", fn->name);
        }
        // we wont clear it, i think its fine
    }
}

void CodeManager::resolve_unresolved_types()
{
    Array<Unresolved_Type*> still_unresolved;
    still_unresolved = interp->pool;

    FOR(unresolved_types){
        Ast_Type_Definition *base = it->base_type;

        if (!base) continue;
        if (!base->is_unresolved) continue;
        if (base->struct_def) { base->is_unresolved = false; continue; }
        if (!base->name) {
            Unresolved_Type *u = UNRESOLVED_NEW(Unresolved_Type);
            u->decl = it->decl;
            u->base_type = it->base_type;
            still_unresolved.push_back(u); continue;
        }

        if (Ast_Type_Definition *def = find_struct_type_in_scopes(base->name)) {
            if (def->struct_def) {
                base->struct_def = def->struct_def;
                base->is_unresolved = false;

                it->decl->declared_type = clone_type_definition(it->decl->declared_type);
                create_type_instantiation(it->decl->declared_type);

                continue;
            }
        } else {
            report_error(it->decl, "Undeclared type '%s'", it->base_type->name);
        }
        // still_unresolved.push_back(u);
    }

    unresolved_types = still_unresolved;
}


// int temp = 0;
void CodeManager::resolve_unresolved_member_accesses() {
    Array<Unresolved_Member_Access*> still_unresolved;
    still_unresolved = interp->pool;

    FOR(unresolved_member_accesses){
        Ast_Declaration *field = resolve_member_access(it->dot_expr, it->my_scope, false, true,/*should_infer=*/true);
        if (!field) {
            auto *u = UNRESOLVED_NEW(Unresolved_Member_Access);
            u->dot_expr = it->dot_expr;
            u->assignment_expr = it->assignment_expr;
            u->my_scope = it->my_scope;

            still_unresolved.push_back(u);
            continue;
        }
        // printf("[%d,%d]\n", u.line_number, u.character_number);
        // printf("Numer %d, resolved to member at: %p, name: %s\n", ++temp, field, field->identifier->name);

        if (it->assignment_expr) {
            // printf("Numer %d, inside if at: %p\n", temp, u.assignment_expr);

            scope_stack.push_back(it->my_scope);
            resolve_idents_in_expr(it->assignment_expr->rhs);
            field->initializer = it->assignment_expr->rhs;
            field->initialized = true;

            scope_stack.pop_back();
        }
    }

    unresolved_member_accesses = still_unresolved;
    if (unresolved_member_accesses.count != 0) {
        FOR(unresolved_member_accesses){
            report_error(it->dot_expr, "Cannot resolve member access: base expression has unknown type");
        }
    }
}


void CodeManager::resolve_array_types(Ast_Array_Type *array_type, Ast_Declaration *decl) {

    Ast_Type_Definition *type = decl->declared_type;

    // Static array ([N]int) or abstract array (^[]int, []int)
    if (!array_type->is_resizable) {

        // evaluate constant size expression if it's an identifier
        if (array_type->size_expr && array_type->size_expr->type == AST_IDENT) {
            auto *ident = static_cast<Ast_Ident*>(array_type->size_expr);
            Ast_Declaration *size_decl = lookup_symbol(ident->name);

            if (!size_decl) {
                report_error(array_type->size_expr, "Undefined identifier '%s' in array size", ident->name);
                return;
            }

            if (size_decl->initializer->type != AST_LITERAL) {
                report_error(array_type->size_expr, "'%s' must be a constant for array size", ident->name);
                return;
            }

            // Replace with the constant's literal value
            if (size_decl->initializer && size_decl->initializer->type == AST_LITERAL) {
                auto *lit = static_cast<Ast_Literal*>(size_decl->initializer);
                if (lit->value_type == LITERAL_NUMBER) {
                    array_type->size_expr = size_decl->initializer;  // Point to the literal
                } else {
                    report_error(array_type->size_expr, "Array size must be an integer constant");
                    return;
                }
            } else {
                report_error(array_type->size_expr, "Cannot determine constant value of '%s'", ident->name);
                return;
            }
        } else if (array_type->size_expr && array_type->size_expr->type == AST_LITERAL){ // if its just pure expression instead
            auto *ident = static_cast<Ast_Ident*>(decl->identifier);
            auto *lit = static_cast<Ast_Literal*>(array_type->size_expr);

            if(lit->integer_value <= 0){
                report_error(array_type, "Array size evaluates to non-positive constant in '%s'", ident->name);
            }


        }

        Ast_Type_Definition *static_array_def = find_struct_type_in_scopes("Static_Array");

        if(!static_array_def) {
            report_error(decl, "Static_Array is not defined.");
            return;
        }
        array_type->struct_def = static_array_def->struct_def;
        type->is_unresolved = false;
        create_type_instantiation(type);
    }
    else if (array_type->is_resizable) {
        Ast_Type_Definition *array_def = find_struct_type_in_scopes("Dynamic_Array");

        type->struct_def = array_def->struct_def;
        type->is_unresolved = false;
        create_type_instantiation(type);
    }
}

Ast_Declaration *find_struct_member(Ast_Type_Definition *struct_type, const char *member_name) {
    if (!struct_type || !struct_type->struct_def || !member_name) return nullptr;
    Ast_Struct *struct_def = struct_type->struct_def;

    FOR(struct_def->members){
        if (it && it->identifier && it->identifier->name && strcmp(it->identifier->name, member_name) == 0) {
            return it;
        }
    }
    return nullptr;
}

inline void CodeManager::push_unresolved_var(Ast_Ident *ident, Ast_Block *my_scope){
    Unresolved_Variable *u = UNRESOLVED_NEW(Unresolved_Variable);
    u->ident = ident;
    u->my_scope = my_scope;
    unresolved_vars.push_back(u);
}


inline void CodeManager::push_unresolved_type(Ast_Declaration *decl, Ast_Type_Definition *base_type){
    Unresolved_Type *u = UNRESOLVED_NEW(Unresolved_Type);
    u->decl = decl;
    u->base_type = base_type;
    unresolved_types.push_back(u);
}

inline void CodeManager::push_unresolved_member_access(Ast_Binary *dot_expr, Ast_Binary *assignment_expr){
    Unresolved_Member_Access *u = UNRESOLVED_NEW(Unresolved_Member_Access);;
    u->dot_expr = dot_expr;
    u->assignment_expr = assignment_expr;
    u->my_scope = scope_stack.get_back();
    unresolved_member_accesses.push_back(u);
}

inline void CodeManager::push_unresolved_call(Ast_Procedure_Call_Expression *call){
    Unresolved_Call *u = UNRESOLVED_NEW(Unresolved_Call);
    u->call = call;
    u->my_scope = scope_stack.get_back();
    unresolved_calls.push_back(u);
}


Ast_Declaration *CodeManager::resolve_member_access(Ast_Binary *dot_expr, Ast_Block *my_scope, bool skip_init_check, bool skip_queuing, bool should_infer) {
    Ast_Type_Definition *base_type = nullptr;
    Ast_Declaration *base_decl = nullptr;
    Ast_Type_Instantiation *current_instance = nullptr;

    Ast_Binary *base_dot = static_cast<Ast_Binary*>(dot_expr->lhs);
    if (dot_expr->lhs->type == AST_BINARY) {
        if (base_dot->op == BINOP_DOT) {
            Ast_Declaration *nested_field = resolve_member_access(base_dot, my_scope, skip_init_check, true, should_infer); // skip_queuing at first bad resolve to avoid sub dot_expr nodes being pushed to unresolved queue
            if (!nested_field) {
                if(!skip_queuing){
                    push_unresolved_member_access(dot_expr);
                }
                return nullptr;
            }

            base_type = nested_field->declared_type;

            // idk if this is neccessary? when we have a locally declared type being passed by value and returned through pointer it breaks saying base expressioon has unknown type. BUT maybe there shoudl still be a check when in the future we implement heap alloc....
            if (!base_type && nested_field->initializer && should_infer) {
                scope_stack.push_back(my_scope);
                infer_types_expr(&nested_field->initializer);
                scope_stack.pop_back();

                base_type = nested_field->initializer->inferred_type;
                if (base_type) {
                    nested_field->declared_type = base_type;
                }
            }


            if (base_type) {
                dot_expr->lhs->inferred_type = base_type;
            }

            if (!base_type) {
                if(!skip_queuing){
                    push_unresolved_member_access(dot_expr);
                }
                return nullptr;
            }

            Ast_Type_Definition *check_resolved = base_type;
            while (check_resolved->pointed_to_type){
                if (check_resolved->pointed_to_type) {
                    check_resolved = check_resolved->pointed_to_type;
                    continue;
                }
            }
            if (check_resolved->is_unresolved) {
                if(!skip_queuing){
                    push_unresolved_member_access(dot_expr);
                }
                return nullptr;
            }

            if (!skip_init_check && base_type && base_type->pointed_to_type) {
                if (!nested_field->initialized) {
                    report_error(dot_expr, "Cannot access member through uninitialized pointer member '%s'",
                                    nested_field->identifier ? nested_field->identifier->name : "(unknown)");
                    return nullptr;
                }
            }

            // Auto-dereference to get to the struct type
            while (base_type->pointed_to_type) {
                base_type = base_type->pointed_to_type;
            }


            // Get instance AFTER dereferencing
            current_instance = base_type->type_instance;
        }
    }



    if (!base_type && dot_expr->lhs->type == AST_IDENT) {
        Ast_Ident *base_name = static_cast<Ast_Ident*>(dot_expr->lhs);
        base_decl = lookup_symbol(base_name->name, my_scope);
        if (!base_decl) {
            if (should_infer) {
                base_decl = lookup_symbol(base_name->name, scope_stack.data[0]);

            }
        }
        if (!base_decl) {
                if (!skip_queuing) push_unresolved_member_access(dot_expr);
                return nullptr;
        }

        base_type = base_decl->declared_type;


        if (!base_type && base_decl->initializer && should_infer) // the declarations with explicit type given that dot_expr depend on need to be inferred first. should_infer is only set during reslolve_unresolved_member_accesses_queue()
        {
            scope_stack.push_back(my_scope);
            infer_types_expr(&base_decl->initializer);
            scope_stack.pop_back();

            base_type = base_decl->initializer->inferred_type;
            if (base_type) {
                dot_expr->lhs->inferred_type = base_type;
            }
        }

        if (!base_type) {
            if(!skip_queuing){
                push_unresolved_member_access(dot_expr);
            }
            return nullptr;
        }

        if (!skip_init_check && base_type && base_type->pointed_to_type) {
            bool should_skip = base_decl->is_declaration_function_argument || base_decl->is_declaration_passed_through_function;

            if (!base_decl->initialized && !should_skip) {
                report_error(dot_expr, "Cannot access member through uninitialized pointer '%s'", base_decl->identifier->name);
                return nullptr;
            }
        }

        while (base_type->pointed_to_type) {
            base_type = base_type->pointed_to_type;
        }

        current_instance = base_type->type_instance;
    }

    if (!base_type) {

        if (!dot_expr->lhs->inferred_type) {
            resolve_idents_in_expr(dot_expr->lhs, my_scope);
        }

        if (should_infer) {
            scope_stack.push_back(my_scope);
            infer_types_expr(&dot_expr->lhs);
            scope_stack.pop_back();
        }
        base_type = dot_expr->lhs->inferred_type;

        if (!base_type || base_type == _type->type_def_dummy || base_type->is_unresolved) {
            if (!skip_queuing) {
                push_unresolved_member_access(dot_expr);
            }
            return nullptr;
        }

        // Deref pointers
        while (base_type->pointed_to_type) {
            base_type = base_type->pointed_to_type;
        }

        if (!base_type->struct_def || base_type->is_unresolved) {
            if (!skip_queuing) {
                push_unresolved_member_access(dot_expr);
            }
            return nullptr;
        }

        current_instance = base_type->type_instance;
    }

    //  queue for later pass if no struct definition was found
    if (!base_type->struct_def) {
        if (base_type->is_unresolved) {
            if(!skip_queuing){
                push_unresolved_member_access(dot_expr);
            }
        }
        return nullptr;
    }

    // this will happen for function return type so create the instance from existing function
    if (!current_instance && base_type->struct_def) {
        create_type_instantiation(base_type);
        current_instance = base_type->type_instance;
    }

    Ast_Ident *member_id = static_cast<Ast_Ident*>(dot_expr->rhs);

    if (current_instance) {
        FOR(current_instance->member_instances){
            if (it && it->identifier && it->identifier->name && strcmp(it->identifier->name, member_id->name) == 0) {
                // Set the inferred type on this dot expression
                if (it->declared_type) {
                    dot_expr->inferred_type = it->declared_type;
                }
                return it;
            }
        }
    } else {
        report_error(dot_expr, "INTERNAL, no struct definition was found. We should not be here!"); // TEMPORARY
        return nullptr;
    }

    Ast_Struct *sd = base_type->struct_def;
    report_error(member_id, "Struct '%s' has no member '%s'", sd->name ? sd->name : "(unknown)", member_id->name);
    return nullptr;
}


void CodeManager::resolve_idents_in_expr(Ast_Expression *expr, Ast_Block *my_scope)
{
    if (!expr) return;

    switch (expr->type) {
    case AST_IDENT: {
        auto *id = static_cast<Ast_Ident*>(expr);
        Ast_Declaration *decl = lookup_symbol(id->name, my_scope);
        if (!decl) {
            push_unresolved_var(id, scope_stack.get_back());
        }

        break;
    }

    case AST_LITERAL:
        break;

    case AST_UNARY: {
        auto *u = static_cast<Ast_Unary*>(expr);
        if (!u->operand) break;

        resolve_idents_in_expr(u->operand, my_scope);

        break;
    }


    case AST_BINARY: {
        auto *b = static_cast<Ast_Binary*>(expr);

        if (b->op == BINOP_ASSIGN) {
            if (b->lhs->type == AST_COMMA_SEPARATED_ARGS) {
                auto *lhs_args = static_cast<Ast_Comma_Separated_Args*>(b->lhs);

                if (b->rhs->type == AST_COMMA_SEPARATED_ARGS) {
                    auto *rhs_args = static_cast<Ast_Comma_Separated_Args*>(b->rhs);

                    if (rhs_args->arguments.count == 1) {
                        Ast_Expression *single_rhs = rhs_args->arguments.data[0];
                        resolve_idents_in_expr(single_rhs, my_scope);

                        FOR(lhs_args->arguments){
                            resolve_idents_in_expr(it, my_scope);

                            if (it->type == AST_IDENT) {
                                Ast_Ident *l_ident = static_cast<Ast_Ident*>(it);
                                Ast_Declaration *decl = lookup_symbol(l_ident->name);
                                if (decl) decl->initialized = true;
                            }
                        }
                        return;
                    }

                    if (lhs_args->arguments.count != rhs_args->arguments.count) {
                        report_error(b, "Number of variables on LHS (%d) does not match number of values on RHS (%d)",
                                     lhs_args->arguments.count, rhs_args->arguments.count);
                        return;
                    }

                    for (int i = 0; i < lhs_args->arguments.count; i++) {
                        Ast_Expression *l_expr = lhs_args->arguments.data[i];
                        Ast_Expression *r_expr = rhs_args->arguments.data[i];

                        resolve_idents_in_expr(l_expr, my_scope);
                        resolve_idents_in_expr(r_expr, my_scope);

                        if (l_expr->type == AST_IDENT) {
                            Ast_Ident *l_ident = static_cast<Ast_Ident*>(l_expr);
                            Ast_Declaration *decl = lookup_symbol(l_ident->name);
                            if (decl) decl->initialized = true;
                        }
                    }
                    return;

                } else {
                    resolve_idents_in_expr(b->rhs, my_scope);

                    FOR(lhs_args->arguments) {
                        resolve_idents_in_expr(it, my_scope);

                        if (it->type == AST_IDENT) {
                            Ast_Ident *l_ident = static_cast<Ast_Ident*>(it);
                            Ast_Declaration *decl = lookup_symbol(l_ident->name);
                            if (decl) decl->initialized = true;
                        }
                    }
                    return;
                }
            }

            if(b->lhs->type == AST_BINARY){
                Ast_Binary *lhs_binary = static_cast<Ast_Binary *>(b->lhs);
                if (lhs_binary->op == BINOP_ARRAY_SUBSCRIPT) {
                    // Check array identifier
                    if (lhs_binary->lhs->type == AST_IDENT) {
                        Ast_Ident *ident = static_cast<Ast_Ident*>(lhs_binary->lhs);
                        Ast_Declaration *decl = lookup_symbol(ident->name);

                        if (!decl) {
                            report_error(ident, "Use of undeclared variable '%s'", ident->name);
                            return;
                        }
                    }

                    // Resolve subscript and rhs
                    resolve_idents_in_expr(b->lhs, my_scope);
                    resolve_idents_in_expr(b->rhs, my_scope);
                    return;
                }
                else if (lhs_binary->op == BINOP_DOT) {
                    Ast_Declaration *lhs_field = resolve_member_access(lhs_binary, scope_stack.get_back(), false, false, false);

                    if (!lhs_field) {
                        // TEMPORARY, we only pass the assignment_expr for this Unresolved_Member_Access queue
                        push_unresolved_member_access(lhs_binary, b);
                        return;
                    }

                    resolve_idents_in_expr(b->rhs, my_scope);

                    lhs_field->initializer = b->rhs;
                    lhs_field->initialized = true;

                    if (lhs_field->declared_type && lhs_field->declared_type->pointed_to_type) {

                        if (b->rhs->type == AST_UNARY) {
                            Ast_Unary *rhs_unary = static_cast<Ast_Unary*>(b->rhs);
                            if (rhs_unary->op == UNARY_ADDRESS_OF && rhs_unary->operand) {
                                Ast_Declaration *rhs_decl = nullptr;

                                if (rhs_unary->operand->type == AST_IDENT) {
                                    Ast_Ident *rhs_id = static_cast<Ast_Ident*>(rhs_unary->operand);
                                    rhs_decl = lookup_symbol(rhs_id->name);
                                }
                                else if (rhs_unary->operand->type == AST_BINARY) {
                                    Ast_Binary *rhs_member = static_cast<Ast_Binary*>(rhs_unary->operand);
                                    if (rhs_member->op == BINOP_DOT) {
                                        rhs_decl = resolve_member_access(rhs_member, scope_stack.get_back(), false);
                                    }
                                }

                                if (rhs_decl && rhs_decl->declared_type) {
                                    Ast_Type_Definition *pointed_to = lhs_field->declared_type->pointed_to_type;
                                    Ast_Type_Definition *rhs_base = rhs_decl->declared_type;
                                    while (rhs_base->pointed_to_type) {
                                        rhs_base = rhs_base->pointed_to_type;
                                    }

                                    if (rhs_base->struct_def) {
                                        pointed_to->struct_def = rhs_base->struct_def;
                                        pointed_to->type_instance = rhs_base->type_instance;
                                        pointed_to->is_unresolved = false;
                                    }
                                }
                            }
                        }
                    }

                    b->lhs->inferred_type = lhs_field->declared_type;
                }
            }
            else if (Ast_Ident *lhs_ident = ast_static_cast<Ast_Ident>(b->lhs, AST_IDENT)) {

                resolve_idents_in_expr(b->rhs, my_scope);

                Ast_Declaration *is_decl = lookup_symbol(lhs_ident->name);
                if (!is_decl) {
                    push_unresolved_var(lhs_ident, scope_stack.get_back());
                } else {
                    is_decl->initialized = true;
                }
            }
            else if (Ast_Unary *lhs_unary = ast_static_cast<Ast_Unary>(b->lhs, AST_UNARY)) {
                resolve_idents_in_expr(b->rhs, my_scope);
                if (lhs_unary->op == UNARY_DEREFERENCE) {
                    resolve_idents_in_expr(lhs_unary->operand, my_scope);
                } else {
                    report_error(lhs_unary, "Unsupported unary operation on LHS of assignment");
                }
            }
            else {
                report_error(b, "Left-hand side of assignment must be an identifier or dereferenced pointer");
            }
        }
        else if (b->op == BINOP_DOT) {
            Ast_Declaration *field = resolve_member_access(b, scope_stack.get_back(), false, true);
            if (field) {
                b->inferred_type = field->declared_type;
            }
             // //comment this out for now probably don't need to queue it for resolve since resolve_member_access should have queued it if it was necesssary
             // else {
             //     push_unresolved_member_access(b);
             // }

        }
        else {
            if (b->lhs) resolve_idents_in_expr(b->lhs, my_scope);
            if (b->rhs) resolve_idents_in_expr(b->rhs, my_scope);
        }
        break;
    }

    case AST_PROCEDURE_CALL_EXPRESSION: {
        auto *call = static_cast<Ast_Procedure_Call_Expression*>(expr);

        if (call->function)
        {
            auto *fn = static_cast<Ast_Ident*>(call->function);
            if(!fn->name) return;

            if(strcmp(fn->name, "sizeof") == 0) return;

            if(strcmp(fn->name, "printf") == 0){
                FOR(call->arguments->arguments){
                    resolve_idents_in_expr(it, my_scope);
                }
                return;
            }

            Ast_Declaration *decl= lookup_symbol(fn->name);
            if (!decl) {

                if (call->arguments) {
                    // even if we haven't found the function declartion yet, we can still resolve the arguments and let the second pass handle function decl
                    FOR(call->arguments->arguments){
                        resolve_idents_in_expr(it, my_scope);
                    }
                }
                push_unresolved_call(call);
            } else if (!decl->is_function) {
                report_error(fn, "'%s' is not a function", fn->name);
            }
            else {
                // Check parameter count
                int call_arg_count = call->arguments ? call->arguments->arguments.count : 0;
                int decl_arg_count = decl->parameters.count;
                if (call_arg_count != decl_arg_count) {
                    report_error(fn, "Function '%s' expects %d arguments, but %d were provided",
                                 fn->name, decl_arg_count, call_arg_count);
                } else if (call->arguments) {

                    FOR(call->arguments->arguments){
                        resolve_idents_in_expr(it, my_scope);
                    }
                }
            }
        }

        if (call->arguments) {
            FOR(call->arguments->arguments){
                // flagging the tag for assuming that the local uninitialized pointer of struct/member
                // is atleast being to a proc call (in the hopes that function will initialize it)
                if (Ast_Unary *addr = ast_static_cast<Ast_Unary>(it, AST_UNARY)) {
                    if (addr->op == UNARY_ADDRESS_OF) {

                        if (Ast_Ident *var = ast_static_cast<Ast_Ident>(addr->operand, AST_IDENT)) {
                            Ast_Declaration *decl = lookup_symbol(var->name);
                            if (decl) {
                                decl->is_declaration_passed_through_function = true;
                            }
                        }
                        else if (Ast_Binary *dot = ast_static_cast<Ast_Binary>(addr->operand, AST_BINARY)) {
                            if (dot->op == BINOP_DOT && dot->lhs->type == AST_IDENT) {
                                Ast_Ident *base_id = static_cast<Ast_Ident*>(dot->lhs);
                                Ast_Declaration *base_decl = lookup_symbol(base_id->name);
                                if (base_decl) {
                                    base_decl->is_declaration_passed_through_function = true;
                                }
                            }
                        }
                    }
                }
            }
        }
        break;
    }

    case AST_COMMA_SEPARATED_ARGS: {
        auto *args = static_cast<Ast_Comma_Separated_Args*>(expr);

        FOR(args->arguments){
            resolve_idents_in_expr(it, my_scope);
        }
        break;
    }

    case AST_STRUCT: {
        auto *s = static_cast<Ast_Struct *>(expr);
        if(!s) return;

        FOR(s->members){

            if(!it) break;
            Ast_Type_Definition *base_type = it->declared_type;

            while(base_type && base_type->pointed_to_type){
                if(base_type->pointed_to_type) {
                    base_type = base_type->pointed_to_type;
                }
            }

            if(base_type && base_type->is_unresolved){
                auto *struct_n = base_type->name;
                Ast_Declaration *struct_ = lookup_symbol(struct_n);
                auto *def_ = reinterpret_cast<Ast_Type_Definition*>(struct_);
                if(struct_) {
                    base_type->struct_def = def_->pointed_to_type->struct_def;
                    base_type->is_unresolved = false;
                } else {
                    push_unresolved_type(it, base_type);
                }
            }
        }
        break;
    }

    default: break;
    }
}


char *CodeManager::type_to_string(Ast_Type_Definition *type) {

    if (!type) {
        return {};
    }

    std::string type_str;
    Ast_Type_Definition *base_type = type;
    int pointer_depth = 0;

    // Count pointers
    while (base_type->pointed_to_type) {
        base_type = base_type->pointed_to_type;
        pointer_depth++;
    }

    // Add pointer prefix
    for (int i = 0; i < pointer_depth; ++i) {
        type_str += "^";
    }


    if (type->is_reference) {
        type_str = "&";
    }

    // Handle arrays
    if (base_type->type == AST_ARRAY_TYPE) {
        auto *arr = static_cast<Ast_Array_Type*>(base_type);

        // Get element type string recursively
        char *elem_str = type_to_string(arr->element_type);

        // Format array part
        if (arr->is_resizable) {
            type_str += "[..]";
        } else if (arr->size_expr && arr->size_expr->type == AST_LITERAL) {
            auto *lit = static_cast<Ast_Literal*>(arr->size_expr);
            if (lit->value_type == LITERAL_NUMBER) {
                type_str += "[" + std::to_string(lit->integer_value) + "]";
            } else {
                type_str += "[?]";
            }
        } else {
            type_str += "[?]";  // Size expression not yet evaluated
        }

        type_str += elem_str;
    } else {
        type_str += base_type->to_string(*_type);
    }

    return pool_strdup(interp->pool, type_str.c_str());
}


void CodeManager::infer_types_return(Ast_Statement *ret, Ast_Declaration *func_decl) {
    if (!ret || !func_decl) return;

    Ast_Type_Definition *func_return_type = func_decl->return_type ? func_decl->return_type : _type->type_def_void;

    if (!ret->expression && func_return_type != _type->type_def_void) {
        report_error(ret,
                     "Return statement in function '%s' must return a value of type %s",
                     func_decl->identifier->name,
                     func_return_type == _type->type_def_int ? "int" :
                     func_return_type == _type->type_def_float ? "float" :
                     func_return_type == _type->type_def_bool ? "bool" : "unknown");
        return;
    }

    if (ret->expression && func_return_type == _type->type_def_void) {
        report_error(ret,
                     "Void function '%s' cannot return a value",
                     func_decl->identifier->name);
        return;
    }

    if (ret->expression) {
        infer_types_expr(&ret->expression);
        Ast_Type_Definition *return_expr_type = ret->expression->inferred_type;
        if (!return_expr_type) {
            report_error(ret,
                         "Could not infer type of return expression in function '%s'",
                         func_decl->identifier->name);
            return;
        }

        if (!check_that_types_match(func_return_type, return_expr_type)) {
            report_error(ret,
                         "Return type mismatch in function '%s': expected %s, got %s",
                         func_decl->identifier->name,
                         func_return_type == _type->type_def_int ? "int" :
                         func_return_type == _type->type_def_float ? "float" :
                         func_return_type == _type->type_def_bool ? "bool" : "void",
                         return_expr_type == _type->type_def_int ? "int" :
                         return_expr_type == _type->type_def_float ? "float" :
                         return_expr_type == _type->type_def_bool ? "bool" : "unknown");
        }
    }
}


void CodeManager::infer_types_expr(Ast_Expression **expr_ptr)
{
    if (!expr_ptr) return;
    Ast_Expression *expr = *expr_ptr;
    if(expr->inferred_type && expr->type != AST_BINARY) return;  // this saves us unnessary checks

    switch (expr->type) {
        case AST_LITERAL: {
            Ast_Literal *lit = static_cast<Ast_Literal *>(expr);
            if(lit->value_type == LITERAL_NUMBER){
                expr->inferred_type = _type->type_def_s64;
            } else if(lit->value_type == LITERAL_FLOAT){
                expr->inferred_type = _type->type_def_float;
            } else if(lit->value_type == LITERAL_STRING){
                expr->inferred_type = _type->type_def_string;
            } else if(lit->value_type == LITERAL_TRUE){
                expr->inferred_type = _type->type_def_bool;
            } else if(lit->value_type == LITERAL_FALSE){
                expr->inferred_type = _type->type_def_bool;
            } else if(lit->value_type == LITERAL_NULL){
                expr->inferred_type = _type->type_def_null;
            } else {
                report_error(expr, "Internal: unhandled type of literal.");
            }
            return;
        }
        case AST_IDENT: {
            Ast_Ident *id = static_cast<Ast_Ident *>(expr);
            Ast_Declaration *decl = lookup_symbol(id->name);

            if (!decl) {
                report_error(id, "Use of undeclared identifier '%s'", id->name);
                expr->inferred_type = _type->type_def_dummy;
                break;
            }

            // Functions unchanged
            if (decl->is_function) {
                expr->inferred_type = decl->return_type;
                break;
            }

            if (decl->identifiers.count > 0) {
                Ast_Type_Definition* t = nullptr;

                FOR(decl->identifiers){
                    if (it && strcmp(it->name, id->name) == 0) {
                        if (it_index < decl->identifier_types.count && decl->identifier_types.data[it_index]) {
                            t = decl->identifier_types.data[it_index]; // comes from initializers or declared_type
                        }
                        break;
                    }
                }

                if (!t) t = decl->declared_type;
                if (!t) t = _type->type_def_dummy;
                expr->inferred_type = t;
                break;
            }

            if (decl->declared_type) {
                expr->inferred_type = decl->declared_type;
            } else if (decl->initialized && !decl->inferred && decl->initializer) {
                infer_types_expr(&decl->initializer);
                expr->inferred_type = decl->initializer->inferred_type;
            } else {
                expr->inferred_type = _type->type_def_dummy;
            }
            break;
        }

        case AST_UNARY: {
            Ast_Unary *u = static_cast<Ast_Unary *>(expr);
            if (!u->operand) {
                expr->inferred_type = _type->type_def_dummy;
                return;
            }

            infer_types_expr(&u->operand);
            Ast_Type_Definition *operandType = u->operand->inferred_type;
            if (!operandType) {
                report_error(u, "Could not determine type of operand for unary expression");
                expr->inferred_type = _type->type_def_dummy;
                break;
            }

            Ast_Type_Definition *resultType = AST_NEW(Ast_Type_Definition);
            switch (u->op) {
            case UNARY_DEREFERENCE: {
                if (!operandType->pointed_to_type || operandType == _type->type_def_dummy) {
                    report_error(u, "Cannot dereference non-pointer type");
                    expr->inferred_type = _type->type_def_dummy;
                    break;
                }

                if (Ast_Ident *operand_ident = ast_static_cast<Ast_Ident>(u->operand, AST_IDENT)) {
                    Ast_Declaration *decl = lookup_symbol(operand_ident->name);
                    if (decl && !decl->initialized && !decl->is_declaration_function_argument) {
                        report_error(u, "Cannot dereference uninitialized pointer '%s'", operand_ident->name);
                        expr->inferred_type = _type->type_def_dummy;
                        break;
                    }
                }
                else if (Ast_Binary *operand_dot = ast_static_cast<Ast_Binary>(u->operand, AST_BINARY)) {
                    if (operand_dot->op == BINOP_DOT) {
                        Ast_Block *current_scope = scope_stack.count > 0 ? scope_stack.get_back() : nullptr;
                        Ast_Declaration *member = resolve_member_access(operand_dot, current_scope, false);

                        // should skip when a type is that uninitialized pointer is being passed to function call in this case we assume that that function will initialize it so dont throw error.
                        bool should_skip = false;
                        if (operand_dot->lhs->type == AST_IDENT) {
                            Ast_Ident *base_id = static_cast<Ast_Ident*>(operand_dot->lhs);
                            Ast_Declaration *base_var = lookup_symbol(base_id->name);

                            if (base_var && base_var->is_declaration_passed_through_function) {
                                should_skip = true;
                            }
                        }

                        if (member && !member->initialized && !should_skip) {
                            report_error(u, "Cannot dereference uninitialized pointer member '%s'",
                                member->identifier ? member->identifier->name : "(unknown)");
                            expr->inferred_type = _type->type_def_dummy;
                            break;
                        }
                    }
                }

                expr->inferred_type = operandType->pointed_to_type;
                break;
            }
            case UNARY_ADDRESS_OF:
                resultType->pointed_to_type = operandType;
                expr->inferred_type = resultType;
                break;
            case UNARY_NEGATE:
            case UNARY_NOT:
                expr->inferred_type = operandType;
                break;

            default:
                report_error(u, "Unknown unary operator");
                expr->inferred_type = _type->type_def_dummy;
            }
            break;
        }

        case AST_BINARY: {

            Ast_Binary *b = static_cast<Ast_Binary *>(expr);
            switch (b->op) {
                case BINOP_ARRAY_SUBSCRIPT: {
                    infer_types_expr(&b->lhs);
                    infer_types_expr(&b->rhs);

                    Ast_Type_Definition *array_type = b->lhs->inferred_type;
                    if (!array_type) {
                        report_error(b, "Cannot subscript expression with unknown type");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    //  if it's a POINTER to an array first
                    // e.g., b: ^[]int, then b[0] needs to dereference b first
                    if (array_type->pointed_to_type) {
                        Ast_Type_Definition *pointee = array_type->pointed_to_type;

                        if (pointee->type == AST_ARRAY_TYPE) {
                            auto *arr = static_cast<Ast_Array_Type*>(pointee);

                            // Validate index is integer
                            Ast_Type_Definition *index_type = b->rhs->inferred_type;
                            if (!index_type) {
                                report_error(b, "Array index has unknown type");
                                expr->inferred_type = _type->type_def_dummy;
                                return;
                            }

                            bool is_integer = (index_type == _type->type_def_int ||
                                              index_type == _type->type_def_s8 ||
                                              index_type == _type->type_def_s16 ||
                                              index_type == _type->type_def_s32 ||
                                              index_type == _type->type_def_s64 ||
                                              index_type == _type->type_def_u8 ||
                                              index_type == _type->type_def_u16 ||
                                              index_type == _type->type_def_u32 ||
                                              index_type == _type->type_def_u64);

                            if (!is_integer) {
                                report_error(b->rhs, "Array index must be an integer type");
                                expr->inferred_type = _type->type_def_dummy;
                                return;
                            }

                            if (!arr->element_type) {
                                report_error(b, "Array type has no element type");
                                expr->inferred_type = _type->type_def_dummy;
                                return;
                            }

                            expr->inferred_type = arr->element_type;
                            return;
                        }

                        // report_error(b, "Cannot subscript non array type"); // @Temporary, this should be checked during resolution
                        return;
                    }

                    while (array_type->pointed_to_type && array_type->pointed_to_type->type != AST_ARRAY_TYPE) {
                        array_type = array_type->pointed_to_type;
                    }

                    if (array_type->type == AST_ARRAY_TYPE) {
                        auto *arr = static_cast<Ast_Array_Type*>(array_type);

                        Ast_Type_Definition *index_type = b->rhs->inferred_type;
                        if (!index_type) {
                            report_error(b, "Array index has unknown type");
                            expr->inferred_type = _type->type_def_dummy;
                            return;
                        }

                        bool is_integer = (index_type == _type->type_def_int ||
                                          index_type == _type->type_def_s8 ||
                                          index_type == _type->type_def_s16 ||
                                          index_type == _type->type_def_s32 ||
                                          index_type == _type->type_def_s64 ||
                                          index_type == _type->type_def_u8 ||
                                          index_type == _type->type_def_u16 ||
                                          index_type == _type->type_def_u32 ||
                                          index_type == _type->type_def_u64);

                        if (!is_integer) {
                            report_error(b->rhs, "Array index must be an integer type");
                            expr->inferred_type = _type->type_def_dummy;
                            return;
                        }

                        // Bounds check for static arrays with constant indices
                        if (!arr->is_resizable && arr->size_expr && arr->size_expr->type == AST_LITERAL) {
                            auto *size_lit = static_cast<Ast_Literal*>(arr->size_expr);
                            if (size_lit->value_type == LITERAL_NUMBER) {
                                long long array_size = size_lit->integer_value;
                                if (b->rhs->type == AST_LITERAL) {
                                    auto *index_lit = static_cast<Ast_Literal*>(b->rhs);
                                    if (index_lit->value_type == LITERAL_NUMBER) {
                                        long long index = index_lit->integer_value;
                                        if (index < 0) {
                                            report_error(b->rhs, "Array index cannot be negative (got %lld)", index);
                                            expr->inferred_type = _type->type_def_dummy;
                                            return;
                                        }
                                        if (index >= array_size) {
                                            report_error(b->rhs,
                                                       "Array index %lld is out of bounds for array of size %lld",
                                                       index, array_size);
                                            expr->inferred_type = _type->type_def_dummy;
                                            return;
                                        }
                                    }
                                }
                            }
                        }

                        if (!arr->element_type) {
                            report_error(b, "Array type has no element type");
                            expr->inferred_type = _type->type_def_dummy;
                            return;
                        }

                        expr->inferred_type = arr->element_type;
                        return;
                    }

                    // Kind of a hack, must clean this up at some point
                    if(array_type && array_type->name && strcmp(array_type->name, "String") == 0){
                        expr->inferred_type = array_type;
                        return;
                    }

                    report_error(b, "Cannot subscript non-array type");
                    expr->inferred_type = _type->type_def_dummy;
                }
                case BINOP_ADD:
                case BINOP_SUB:
                case BINOP_MUL:
                case BINOP_DIV:
                case BINOP_LESS:
                case BINOP_GREATER:
                case BINOP_LESS_EQUAL:
                case BINOP_GREATER_EQUAL:
                case BINOP_EQ:
                case BINOP_NEQ: {
                    infer_types_expr(&b->lhs);
                    infer_types_expr(&b->rhs);

                    Ast_Type_Definition *lt = b->lhs->inferred_type;
                    Ast_Type_Definition *rt = b->rhs->inferred_type;

                    if(!lt || !rt || lt == _type->type_def_dummy || rt == _type->type_def_dummy){
                        report_error(expr, "Cannot infer types for operands in binary operation");
                        expr->inferred_type = _type->type_def_dummy;
                        break;
                    }

                    if (b->op == BINOP_ADD || b->op == BINOP_SUB) {
                        // Handle numeric types
                        if (lt == _type->type_def_float || rt == _type->type_def_float) {
                            expr->inferred_type = _type->type_def_float;
                        } else if (lt == _type->type_def_float32 || rt == _type->type_def_float32) {
                            expr->inferred_type = _type->type_def_float32;
                        } else if (lt == _type->type_def_float64 || rt == _type->type_def_float64) {
                            expr->inferred_type = _type->type_def_float64;
                        } else if (lt == _type->type_def_int && rt == _type->type_def_int) {
                            expr->inferred_type = _type->type_def_int;
                        } else if (lt == _type->type_def_s64 && rt == _type->type_def_s64) {
                            expr->inferred_type = _type->type_def_s64;
                        } else if (lt == _type->type_def_int && rt == _type->type_def_s64) {
                            expr->inferred_type = _type->type_def_s64;
                        } else if (lt == _type->type_def_s64 && rt == _type->type_def_int) {
                            expr->inferred_type = _type->type_def_s64;
                        }
                        // pointer arithmetic
                        else if (lt && lt->pointed_to_type && (rt == _type->type_def_int || rt == _type->type_def_s64)) {
                            expr->inferred_type = lt;
                        } else {
                            report_error(b, "Type error in binary arithmetic types incompatible, Expected '%s' Got '%s'", type_to_string(lt),type_to_string(rt));
                            expr->inferred_type = _type->type_def_dummy;
                        }
                        break;
                    }

                    else if (b->op == BINOP_MUL || b->op == BINOP_DIV) {
                        if (lt == _type->type_def_float || rt == _type->type_def_float) {
                            expr->inferred_type = _type->type_def_float;
                        } else if (lt == _type->type_def_float32 || rt == _type->type_def_float32) {
                            expr->inferred_type = _type->type_def_float32;
                        } else if (lt == _type->type_def_float64 || rt == _type->type_def_float64) {
                            expr->inferred_type = _type->type_def_float64;
                        } else if (lt == _type->type_def_int && rt == _type->type_def_int) {
                            expr->inferred_type = _type->type_def_int;
                        } else if (lt == _type->type_def_s64 && rt == _type->type_def_s64) {
                            expr->inferred_type = _type->type_def_s64;
                        } else if (lt == _type->type_def_int && rt == _type->type_def_s64) {
                            expr->inferred_type = _type->type_def_s64;
                        } else if (lt == _type->type_def_s64 && rt == _type->type_def_int) {
                            expr->inferred_type = _type->type_def_s64;
                        } else {
                            report_error(b, "Type error in binary arithmetic: operand types incompatible");
                            expr->inferred_type = _type->type_def_dummy;
                        }
                        break;
                    }


                    else {
                        expr->inferred_type = _type->type_def_bool;
                        break;
                    }
                }
                case BINOP_DOT: {
                    infer_types_expr(&b->lhs);

                    Ast_Type_Definition *base_type = b->lhs->inferred_type;

                    if (!base_type) {
                        report_error(b->lhs, "Cannot determine type of base expression");
                        expr->inferred_type = _type->type_def_dummy;
                        break;
                    }

                    Ast_Type_Definition *dereferenced = base_type;
                    while (dereferenced->pointed_to_type) {
                        dereferenced = dereferenced->pointed_to_type;
                    }

                    if (!dereferenced->struct_def) {
                        report_error(b->lhs, "Member access requires a struct");
                        expr->inferred_type = _type->type_def_dummy;
                        break;
                    }

                    Ast_Ident *member = static_cast<Ast_Ident*>(b->rhs);

                    Ast_Type_Definition *member_type = nullptr;
                    Ast_Declaration *member_decl = nullptr;
                    if (dereferenced->type_instance) {
                        FOR(dereferenced->type_instance->member_instances){
                            if (it && it->identifier && it->identifier->name &&
                                strcmp(it->identifier->name, member->name) == 0) {
                                member_type = it->declared_type;
                                member_decl = it;
                                break;
                            }
                        }
                    } else {
                        Ast_Struct *sd = dereferenced->struct_def;
                        FOR(sd->members){
                            if (it && it->identifier && it->identifier->name &&
                                strcmp(it->identifier->name, member->name) == 0) {
                                member_type = it->declared_type;
                                member_decl = it;
                                break;
                            }
                        }
                    }

                    if (!member_type) {
                        if (member_decl && !member_decl->declared_type && member_decl->initializer) {
                            infer_types_expr(&member_decl->initializer);
                            member_decl->declared_type = member_decl->initializer->inferred_type;
                            member_type = member_decl->declared_type;
                        }
                        if (!member_type) {
                            report_error(member, "Struct '%s' has no member '%s'", dereferenced->struct_def->name, member->name);
                            expr->inferred_type = _type->type_def_dummy;
                        } else {
                            expr->inferred_type = member_type;
                        }
                    } else {
                        expr->inferred_type = member_type;
                    }
                    break;
                }


                case BINOP_ASSIGN: {
                    if (b->lhs->type == AST_COMMA_SEPARATED_ARGS) {
                        Ast_Comma_Separated_Args *lhs_args = static_cast<Ast_Comma_Separated_Args*>(b->lhs);

                        if (b->rhs->type == AST_COMMA_SEPARATED_ARGS) {
                            Ast_Comma_Separated_Args *rhs_args = static_cast<Ast_Comma_Separated_Args*>(b->rhs);

                            if (rhs_args->arguments.count == 1) {
                                Ast_Expression *single_rhs = rhs_args->arguments.data[0];
                                infer_types_expr(&single_rhs);
                                Ast_Type_Definition *rhsType = single_rhs->inferred_type;

                                if (rhsType == _type->type_def_dummy) {
                                    report_error(b, "Right-hand side of assignment has unknown type");
                                    expr->inferred_type = _type->type_def_dummy;
                                    return;
                                }

                                for (int i = 0; i < lhs_args->arguments.count; i++) {
                                    Ast_Expression *l_expr = lhs_args->arguments.data[i];
                                    infer_types_expr(&l_expr);

                                    if (l_expr->inferred_type == _type->type_def_dummy) {
                                        continue;
                                    }

                                    if (!check_that_types_match(l_expr->inferred_type, rhsType)) {
                                        report_error(b, "Type mismatch in multiple assignment for variable at position %d. Expected '%s', got '%s'",
                                                     i + 1, type_to_string(l_expr->inferred_type), type_to_string(rhsType));
                                    }
                                }
                                expr->inferred_type = rhsType;
                                return;
                            }

                            // Normal Pairwise checking
                            if (lhs_args->arguments.count != rhs_args->arguments.count) {
                                report_error(b, "Type Inference Error: Number of variables on LHS (%d) does not match number of values on RHS (%d)",
                                             lhs_args->arguments.count, rhs_args->arguments.count);
                                expr->inferred_type = _type->type_def_dummy;
                                return;
                            }

                            for (int i = 0; i < lhs_args->arguments.count; i++) {
                                Ast_Expression *l_expr = lhs_args->arguments.data[i];
                                Ast_Expression *r_expr = rhs_args->arguments.data[i];

                                infer_types_expr(&l_expr);
                                infer_types_expr(&r_expr);

                                if (l_expr->inferred_type == _type->type_def_dummy || r_expr->inferred_type == _type->type_def_dummy) {
                                    continue;
                                }

                                if (!check_that_types_match(l_expr->inferred_type, r_expr->inferred_type)) {
                                    report_error(b, "Type mismatch in multiple assignment at position %d. Expected '%s', got '%s'",
                                                 i + 1, type_to_string(l_expr->inferred_type), type_to_string(r_expr->inferred_type));
                                }
                            }

                            expr->inferred_type = rhs_args->arguments.get_back()->inferred_type;
                            return;

                        } else {
                            infer_types_expr(&b->rhs);
                            Ast_Type_Definition *rhsType = b->rhs->inferred_type;

                            if (rhsType == _type->type_def_dummy) {
                                report_error(b, "Right-hand side of assignment has unknown type");
                                expr->inferred_type = _type->type_def_dummy;
                                return;
                            }

                            bool is_rhs_single_value = true;

                            if (is_rhs_single_value) {
                                for (int i = 0; i < lhs_args->arguments.count; i++) {
                                    Ast_Expression *l_expr = lhs_args->arguments.data[i];
                                    infer_types_expr(&l_expr);

                                    if (l_expr->inferred_type == _type->type_def_dummy) {
                                        continue;
                                    }

                                    if (!check_that_types_match(l_expr->inferred_type, rhsType)) {
                                        report_error(b, "Type mismatch in multiple assignment for variable at position %d. Expected '%s', got '%s'",
                                                     i + 1, type_to_string(l_expr->inferred_type), type_to_string(rhsType));
                                    }
                                }

                                expr->inferred_type = rhsType;
                                return;
                            }
                        }
                    }
                    infer_types_expr(&b->rhs);
                    Ast_Type_Definition *rhsType = b->rhs->inferred_type;
                    if (rhsType == _type->type_def_dummy) {
                        report_error(b, "Right-hand side of assignment has unknown type");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    Ast_Type_Definition *lhsType = nullptr;

                    if (Ast_Binary *lhs_dot = ast_static_cast<Ast_Binary>(b->lhs, AST_BINARY)) {
                        if (lhs_dot->op == BINOP_DOT) {
                            infer_types_expr(&b->lhs);  // infer the member access

                            if (!lhs_dot->inferred_type || lhs_dot->inferred_type == _type->type_def_dummy) {
                                report_error(lhs_dot, "Cannot determine type of member access");
                                expr->inferred_type = _type->type_def_dummy;
                                break;
                            }

                            lhsType = lhs_dot->inferred_type;

                            if (!check_that_types_match(lhsType, rhsType)) {
                                report_error(b, "Type mismatch in member assignment. Expected '%s' Got '%s'", type_to_string(lhsType), type_to_string(rhsType));
                            }
                        } else {
                            infer_types_expr(&b->lhs);
                            // report_error(b, "Invalid left-hand side of assignment");
                            // expr->inferred_type = _type->type_def_dummy;
                            // break;
                        }
                    }
                    else if (Ast_Ident *lhs_ident = ast_static_cast<Ast_Ident>(b->lhs, AST_IDENT)) {
                        Ast_Declaration *is_decl = lookup_symbol(lhs_ident->name);
                        if (!is_decl) {
                            report_error(lhs_ident, "Undeclared variable '%s'", lhs_ident->name);
                            expr->inferred_type = _type->type_def_dummy;
                            break;
                        }

                        if (!is_decl->declared_type) {
                            is_decl->declared_type = rhsType;
                        } else if (!check_that_types_match(is_decl->declared_type, rhsType)) {
                            report_error(lhs_ident, "Type mismatch in assignment to '%s'. Expected '%s' Got '%s'",
                                          lhs_ident->name, type_to_string(is_decl->declared_type), type_to_string(rhsType));
                        }
                        // is_decl->initializer = b->rhs; // Don't do this
                        is_decl->initialized = true;
                        lhsType = is_decl->declared_type;
                    }


                    else if (Ast_Unary *lhs_unary = ast_static_cast<Ast_Unary>(b->lhs, AST_UNARY)) {
                        if (lhs_unary->op == UNARY_DEREFERENCE) {
                            // Infer the type of what we're dereferencing
                            infer_types_expr(&lhs_unary->operand);
                            Ast_Type_Definition *operandType = lhs_unary->operand->inferred_type;

                            if (!operandType)
                            {
                                report_error(lhs_unary, "Cannot determine type of LHS unary dereferenced expression.");
                                expr->inferred_type = _type->type_def_dummy;
                                return;

                            }

                            if(!operandType->pointed_to_type) {
                                report_error(lhs_unary, "Cannot dereference non-pointer expression of type '%s'", type_to_string(operandType));
                                expr->inferred_type = _type->type_def_dummy;
                                return;
                            }


                            lhsType = operandType->pointed_to_type;


                            // Check for uninitialized pointer
                            if (Ast_Ident *pointer_ident = ast_static_cast<Ast_Ident>(lhs_unary->operand, AST_IDENT)) {
                                Ast_Declaration *pointer_decl = lookup_symbol(pointer_ident->name);
                                if (pointer_decl && !pointer_decl->initialized && !pointer_decl->is_declaration_function_argument) {
                                    report_error(lhs_unary, "Cannot dereference uninitialized pointer '%s'", pointer_ident->name);
                                    expr->inferred_type = _type->type_def_dummy;
                                    return;
                                }
                            }


                            if (!check_that_types_match(lhsType, rhsType)) {
                                report_error(lhs_unary, "Type mismatch: cannot assign '%s' to dereferenced pointer of type '%s'",
                                                type_to_string(rhsType), type_to_string(lhsType));

                                expr->inferred_type = _type->type_def_dummy;
                                return;
                            }


                        } else {
                            report_error(lhs_unary, "Unsupported unary operation on LHS of assignment");
                            expr->inferred_type = _type->type_def_dummy;
                            return;
                        }
                    }

                    else {
                        report_error(b, "Left-hand side of assignment must be an identifier or a dereferenced pointer");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    expr->inferred_type = lhsType;
                    break;
                }

                case BINOP_LOGICAL_AND:
                case BINOP_LOGICAL_OR:{
                    infer_types_expr(&b->lhs);
                    infer_types_expr(&b->rhs);
                    expr->inferred_type = _type->type_def_bool;
                    break;
                }

                default:
                    report_error(b, "Unknown binary operator in type inference");

                    expr->inferred_type = _type->type_def_dummy;
                    return;
            }
            break;
        }

        case AST_STRUCT: {
            auto *s = static_cast<Ast_Struct *>(expr);
            if(!s) return;

            Ast_Block *temp_scope = AST_NEW(Ast_Block);

            FOR(s->members){
                temp_scope->statements.push_back(it);
            }
            scope_stack.push_back(temp_scope);

            FOR(s->members){
                if(it) {
                    infer_types_decl(it);
                }
            }

            scope_stack.pop_back();
            break;
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression *call = static_cast<Ast_Procedure_Call_Expression *>(expr);

            Ast_Type_Definition *return_type = nullptr;

            if (call->function) {
                Ast_Ident *fn = static_cast<Ast_Ident *>(call->function);

                 if (fn->name && strcmp(fn->name, "sizeof") == 0) {
                    int arg_count = call->arguments ? call->arguments->arguments.count : 0;
                    if (arg_count != 1) {
                        report_error(fn, "sizeof() expects exactly 1 argument");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    Ast_Expression *arg = call->arguments->arguments.data[0];

                    if (arg->type != AST_IDENT) {
                        report_error(arg, "sizeof() argument must be a type identifier");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    auto *type_ident = static_cast<Ast_Ident*>(arg);

                    Ast_Type_Definition *resolved = resolve_type_by_name(type_ident->name);
                    if (!resolved) {
                        Ast_Declaration *var = lookup_symbol(type_ident->name);
                        if (var) {
                            report_error(arg,
                                "'%s' is a value/variable, but sizeof() expects a type name",
                                type_ident->name);
                        } else {
                            report_error(arg, "Unknown type '%s' used in sizeof()", type_ident->name);
                        }
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    expr->inferred_type = _type->type_def_s64;
                    return;
                } else if (fn->name && strcmp(fn->name, "malloc") == 0) {

                    Ast_Type_Definition *ptr_to_void = AST_NEW(Ast_Type_Definition);
                    ptr_to_void->pointed_to_type = _type->type_def_void;
                    Ast_Type_Definition *return_type = ptr_to_void; // we need to create it

                    if (call->arguments && call->arguments->arguments.count > 0) {
                        Ast_Expression *arg0 = call->arguments->arguments.data[0];

                        Ast_Type_Definition *T = extract_sizeof_type(arg0);

                        if (T) {
                             // FOUND IT! We want to return ^T
                             Ast_Type_Definition *ptr_to_T = AST_NEW(Ast_Type_Definition);
                             ptr_to_T->pointed_to_type = T;
                             return_type = ptr_to_T;
                        }
                    }

                    // Still infer arguments normally (malloc expects s64/int)
                    if (call->arguments) {
                        FOR(call->arguments->arguments){
                            infer_types_expr(&it);
                        }
                    }

                    expr->inferred_type = return_type;
                    return;
                }
                else {
                    Ast_Declaration *is_decl = lookup_symbol(fn->name);

                    if (is_decl && is_decl->is_function) {
                        return_type = is_decl->return_type ? is_decl->return_type : _type->type_def_void;

                        if (is_decl->return_type && !check_that_types_match(return_type, is_decl->return_type)) {
                            report_error(fn, "Function '%s' return type mismatch", fn->name);
                        }


                        if (call->arguments) {
                            FOR(call->arguments->arguments){
                                infer_types_expr(&it);
                                Ast_Type_Definition *arg_type = it->inferred_type;
                                Ast_Type_Definition *param_type = is_decl->parameters.data[it_index]->declared_type;
                                if (!check_that_types_match(param_type, arg_type)) {
                                    if (!can_implicitly_convert_const(it, param_type)) {
                                        report_error(it, "Type mismatch for argument %d in call to '%s'. Expected '%s', Got '%s'",
                                                            it_index + 1, fn->name, type_to_string(param_type), type_to_string(arg_type));
                                    } else {
                                        it->inferred_type = param_type;
                                    }
                                }
                            }
                        }

                    } else {
                        return_type = _type->type_def_void;
                    }
                }

                // else {
                    // return_type = _type->type_def_void;
                // }
            }


            if (call->arguments)
            {
                FOR(call->arguments->arguments){
                    infer_types_expr(&it);
                }
            }
            // If this call is part of an assignment, check the LHS type
            expr->inferred_type = return_type;
            if (expr->inferred_type == _type->type_def_dummy) {
                expr->inferred_type = return_type;
            } else if (!check_that_types_match(expr->inferred_type, return_type)) {
                report_error(expr, "Type mismatch: function call return type does not match expected type. Expected '%s' Got '%s'",
                                    type_to_string(expr->inferred_type), type_to_string(return_type));
            }

            break;
        }

        case AST_CAST: {
            Ast_Cast *c = static_cast<Ast_Cast*>(expr);

            if (c->expression) {
                infer_types_expr(&c->expression);
            }

            // for casting is not checked
            expr->inferred_type = c->cast_expression;
            break;
        }
        default:
            expr->inferred_type = _type->type_def_dummy;
            break;
    }
}

bool CodeManager::check_that_types_fit(long long value, Ast_Type_Definition *target){

    if(!target) return false;

    if(target == _type->type_def_int) return value >= -2147483648ll && value <= 2147483647ll;
    else if(target == _type->type_def_s8) return value >= -128ll && value <= 127ll;
    else if(target == _type->type_def_s16) return value >= -32768ll && value <= 32767ll;
    else if(target == _type->type_def_s32) return value >= -2147483648ll && value <= 2147483648ll;
    else if(target == _type->type_def_s64) return true;
    else if(target == _type->type_def_u8) return value >= 0 &&  value <= 255ull;
    else if(target == _type->type_def_u16) return value >= 0 && value <= 65535ull;
    else if(target == _type->type_def_u32) return value >= 0 && value <= 4294967295ull;
    else if(target == _type->type_def_u64) return value >= 0;

    return false;
}


bool CodeManager::check_that_types_fit(double value, Ast_Type_Definition *target) {
    if (!target) return false;

    // --- Integer targets: must be in range and integral ---
    if (target == _type->type_def_int)
        return value >= -2147483648.0 && value <= 2147483647.0 && floor(value) == value;
    if (target == _type->type_def_s8)
        return value >= -128.0 && value <= 127.0 && floor(value) == value;
    if (target == _type->type_def_s16)
        return value >= -32768.0 && value <= 32767.0 && floor(value) == value;
    if (target == _type->type_def_s32)
        return value >= -2147483648.0 && value <= 2147483647.0 && floor(value) == value;
    if (target == _type->type_def_s64)
        return floor(value) == value; // whole number fits

    if (target == _type->type_def_u8)
        return value >= 0.0 && value <= 255.0 && floor(value) == value;
    if (target == _type->type_def_u16)
        return value >= 0.0 && value <= 65535.0 && floor(value) == value;
    if (target == _type->type_def_u32)
        return value >= 0.0 && value <= 4294967295.0 && floor(value) == value;
    if (target == _type->type_def_u64)
        return value >= 0.0 && floor(value) == value;

    // --- Floating-point targets ---
    if (target == _type->type_def_float32 || target == _type->type_def_float) {
        // IEEE 754 float range
        const double max_f32 = 3.402823466e38;
        const double min_f32 = -max_f32;
        return value >= min_f32 && value <= max_f32;
    }

    if (target == _type->type_def_float64) {
        // any double fits
        return true;
    }

    return false;
}
long long CodeManager::wrap_integer_to_type(long long value, Ast_Type_Definition *target) {
    if (!target) return value;

    if (target == _type->type_def_u8)  return (unsigned long long)value & 0xFFull;
    if (target == _type->type_def_u16) return (unsigned long long)value & 0xFFFFull;
    if (target == _type->type_def_u32) return (unsigned long long)value & 0xFFFFFFFFull;
    if (target == _type->type_def_u64) return (unsigned long long)value;

    if (target == _type->type_def_s8)  return (long long)((int8_t)value);
    if (target == _type->type_def_s16) return (long long)((int16_t)value);
    if (target == _type->type_def_s32) return (long long)((int32_t)value);
    if (target == _type->type_def_s64) return value;

    if (target == _type->type_def_int) return (long long)((int)value);

    return value;
}



void CodeManager::infer_types_decl(Ast_Declaration *decl) {
    if (!decl) return;

    if (decl->identifiers.count > 0) {
        int n = decl->identifiers.count;
        int init_count = decl->initializers ? decl->initializers->arguments.count : 0;

        // a, b, c: int;
        // a, b, c: int = 1, 2, 3;
        // a, b, c := 1, 2, 3;
        // a, b, c := 1;
        if (decl->initializers) {
            if (init_count != 1 && init_count != n) {
                report_error(decl,
                             "Number of initializers (%d) must match number of identifiers (%d) or be a single value",
                             init_count, n);
                return;
            }
        }

        decl->identifier_types.count = 0;

        for (int i = 0; i < n; ++i) {
            Ast_Ident *id = decl->identifiers.data[i];
            Ast_Expression *init_expr = nullptr;

            if (decl->initializers) {
                init_expr = (init_count == 1)
                            ? decl->initializers->arguments.data[0]   // same expr for all
                            : decl->initializers->arguments.data[i];  // per‑ident expr
            }

            Ast_Type_Definition *init_type = nullptr;

            if (init_expr) {
                Ast_Expression *expr = init_expr;
                infer_types_expr(&expr);
                init_type = expr->inferred_type;

                if (!init_type) {
                    report_error(decl, "Could not infer type for variable '%s' from initializer.", id->name);
                    decl->identifier_types.push_back(_type->type_def_dummy);
                    continue;
                }
            }

            Ast_Type_Definition *decl_type = decl->declared_type;

            if (decl_type) {
                // Explicit type: reuse your literal‑fit logic, but per identifier.
                // We *don’t* want to mutate decl->initializer here since we’re using initializers list.
                if (init_expr && init_type && init_type != _type->type_def_dummy &&
                    !check_that_types_match(decl_type, init_type)) {
                    report_error(decl,
                        "Type mismatch in initializer for '%s', Expected '%s' Got '%s'",
                        id->name,
                        type_to_string(decl_type),
                        type_to_string(init_type));
                }
                decl->identifier_types.push_back(decl_type);
            } else {
                // Inferred type: a, b, c := ...
                if (!init_type || init_type == _type->type_def_dummy) {
                    report_error(decl, "Cannot infer type for variable '%s' without valid initializer.", id->name);
                    decl->identifier_types.push_back(_type->type_def_dummy);
                } else {
                    decl->identifier_types.push_back(init_type);
                }
            }
        }

        return;
    }

    if (decl->initializer) {
        Ast_Expression *expr = decl->initializer;
        infer_types_expr(&expr);
        Ast_Type_Definition *init_type = expr->inferred_type;
        if(!init_type) {
            report_error(decl, "Could not infer type for variable '%s' from intitializer.",decl->identifier->name);
            return;
        }


        Ast_Type_Definition *decl_type = decl->declared_type;
        if (decl_type) {

            // this is temoporay when we want to check if types with UNARY_NEGATE fit
            long long signed_value = 0;
            double signed_float = 0.0;
            bool is_literal_number = false;
            bool is_float_number = false;
            bool is_unary_negate = false;

            Ast_Literal *lit = static_cast<Ast_Literal *>(expr);

            if (expr->type == AST_LITERAL) {
                lit = static_cast<Ast_Literal*>(expr);
                if (lit->value_type == LITERAL_NUMBER) {
                    signed_value = lit->integer_value;
                    is_literal_number = true;
                }
                else if(lit->value_type == LITERAL_FLOAT) {
                    signed_float = lit->float_value;
                    is_float_number = true;
                }

            }
            else if (expr->type == AST_UNARY) {
                auto *u = static_cast<Ast_Unary*>(expr);
                if (u->op == UNARY_NEGATE && u->operand && u->operand->type == AST_LITERAL) {
                    is_unary_negate = true;
                    lit = static_cast<Ast_Literal*>(u->operand);
                    if (lit->value_type == LITERAL_NUMBER) {
                        signed_value = -lit->integer_value;  // to pass the negated value into check_that_types_fit()
                        is_literal_number = true;
                    } else if (lit->value_type == LITERAL_FLOAT) {
                    signed_float = -lit->float_value;
                    is_float_number = true;
                    }

                }
            }

            if (is_literal_number && check_that_types_fit(signed_value, decl_type)) {
                // allow implicit narrowing
                expr->inferred_type = decl_type;
                init_type = decl_type;
            }
            else if (is_float_number && check_that_types_fit(signed_float, decl_type)) {
                // allow implicit narrowing
                expr->inferred_type = decl_type;
                init_type = decl_type;
            }
            else if (is_literal_number) {
                if (!check_that_types_fit(signed_value, decl_type)) {
                    long long wrapped = wrap_integer_to_type(signed_value, decl_type); // TEMPORARY Is this a good idea?
                    // TEMPORARY WE CAN implement the flag during compilation to only throw this warning if the user wants it thrown or not
                    printf("Warning: Constant overflow in identifier '%s'. %lld wrapped to %lld for type '%s'\n",
                           decl->identifier->name, signed_value, wrapped, type_to_string(decl_type));

                    signed_value = wrapped;
                }

                // Replace the expr tree with a single literal (collapse unary)
                if (is_unary_negate) {
                    expr = make_integer_literal(signed_value);
                    decl->initializer = expr;
                } else {
                    expr = make_integer_literal(signed_value);
                    decl->initializer = expr;
                }

                expr->inferred_type = decl_type;
                init_type = decl_type;

                // report_error(decl, "Initializer cannot fit the declared variable '%s'", decl->identifier->name);
            }


            // explicitly typed
            if (!decl->declared_type->struct_def) {
                if (!check_that_types_match(decl->declared_type, init_type)) {

                    // Special case: ^int assigned to ^^int
                    if (init_type && init_type->pointed_to_type &&
                        decl->declared_type->pointed_to_type &&
                        check_that_types_match(decl->declared_type->pointed_to_type, init_type)) {
                        // Allow this
                    }
                    else {
                        report_error(decl,
                            "Type mismatch in initializer for '%s', Expected '%s' Got '%s'",
                            decl->identifier ? decl->identifier->name : "null",
                            type_to_string(decl->declared_type),
                            type_to_string(init_type));
                    }
                }
            }

        } else {
            // not declared with type so infer it through initializer's type instead
            decl->declared_type = init_type;
            Ast_Declaration *is_decl= lookup_symbol(decl->identifier->name);
            if(is_decl) is_decl->return_type = init_type;
        }
    } else {
        auto inf = decl->declared_type;
        if(inf == _type->type_def_int) {
            decl->initializer = make_integer_literal(-24);
        }
        else if(inf == _type->type_def_s8) {
            decl->initializer = make_integer_literal(-6);
        }

    }

}

void CodeManager::infer_types_if(Ast_If *ifn, Ast_Declaration *my_func) {
    if (ifn->condition) {
        Ast_Expression *cond = ifn->condition;
        infer_types_expr(&cond);
    }

    if (ifn->then_block) {
        push_scope();
        infer_types_block(ifn->then_block, my_func);
        pop_scope();
    }

    if (ifn->else_block) {
        if (ifn->else_block->type == AST_IF) {
            // Recursively handle else if
            infer_types_if(static_cast<Ast_If*>(ifn->else_block), my_func);
        } else if (ifn->else_block->type == AST_BLOCK) {
            push_scope();
            infer_types_block(static_cast<Ast_Block*>(ifn->else_block), my_func);
            pop_scope();
        }
    }
}

void CodeManager::infer_types_block(Ast_Block *block, Ast_Declaration *my_func)
{
    assert(block);

    for (int i = 0; i < block->statements.count; i++) {

        Ast_Statement *stmt = block->statements.data[i];

        if (!stmt) continue;

        if (stmt->is_return) {
            assert(my_func);
            if (my_func) {
                infer_types_return(stmt, my_func);
            } else {
                report_error(stmt, "Return statement outside of function body");
            }
        } else if (stmt->expression && stmt->expression->type == AST_STRUCT){
            infer_types_expr(&stmt->expression);

        } else if (stmt->type == AST_DECLARATION) {
            Ast_Declaration *decl = static_cast<Ast_Declaration*>(stmt);

            if(decl->is_function) {
                if (decl->my_scope && decl->is_function_body) {
                    push_scope();

                    FOR(decl->parameters){
                        declare_variable(it);
                    }
                    infer_types_block(decl->my_scope, decl); // Infer types in function body
                    pop_scope();
                }

            }
            else {

                bool is_global = (block->is_global_scope || (decl->my_scope && decl->my_scope->is_global_scope));
                bool is_multi  = (decl->identifiers.count > 0);

                if (!is_global) {
                    if (is_multi) {
                        declare_variable(decl);
                    } else {

                        if (decl->identifier && decl->identifier->name) {
                            Ast_Declaration *is_decl = lookup_symbol_current_scope(decl->identifier->name);
                            if (!is_decl) {
                                declare_variable(decl);
                            }
                        }
                    }
                }
                infer_types_decl(decl);
            }

        } else if (stmt->expression) {
            Ast_Expression *expr = stmt->expression;
            infer_types_expr(&expr);
        } else if (stmt->block) {
            push_scope();
            infer_types_block(stmt->block);
            pop_scope();
        } else if (stmt->type == AST_IF) {
            infer_types_if(static_cast<Ast_If *>(stmt), my_func);
        }else if (stmt->type == AST_WHILE){
            Ast_While *_while = static_cast<Ast_While*>(stmt);
            if (_while->condition) infer_types_expr(&_while->condition);
            if (_while->block) {
                push_scope();
                infer_types_block(_while->block, my_func);
                pop_scope();
            }
        }


    }
}


bool CodeManager::check_that_types_match(Ast_Type_Definition *wanted, Ast_Type_Definition *have, bool is_pointer) {
    if (!wanted || !have || wanted == _type->type_def_dummy || have == _type->type_def_dummy)
        return false;

    if (wanted == have) return true;

    if (wanted == _type->type_def_any || have == _type->type_def_any)
        return true;

    if (wanted->pointed_to_type && wanted->pointed_to_type == _type->type_def_void) {
        if (have->pointed_to_type) return true;
        if (have == _type->type_def_null) return true;
    }

    if (have->pointed_to_type && have->pointed_to_type == _type->type_def_void) {
        if (wanted->pointed_to_type) return true;
    }

    if (have == _type->type_def_null && wanted->pointed_to_type) {
        return true;
    }

    Ast_Array_Type *wa = as_array_type(wanted);
    Ast_Array_Type *ha = as_array_type(have);

    if (wa || ha) {
        if (wanted->pointed_to_type && ha) {
            return check_that_types_match(wanted->pointed_to_type, ha->element_type);
        }

        if (wa && have->pointed_to_type) {
            return false;
        }

        if (!wa || !ha) return false;

        if (!wa->element_type || !ha->element_type) return false;

        if (wa->is_resizable && ha->is_resizable) {
            return check_that_types_match(wa->element_type, ha->element_type);
        }

        if (!wa->is_resizable && !ha->is_resizable) {
            if (wa->size_expr && ha->size_expr &&
                wa->size_expr->type == AST_LITERAL &&
                ha->size_expr->type == AST_LITERAL) {

                auto *wl = static_cast<Ast_Literal*>(wa->size_expr);
                auto *hl = static_cast<Ast_Literal*>(ha->size_expr);

                if (wl->value_type == LITERAL_NUMBER &&
                    hl->value_type == LITERAL_NUMBER &&
                    wl->integer_value != hl->integer_value) {
                    return false;
                }
            }

            return check_that_types_match(wa->element_type, ha->element_type);
        }

        return false;
    }

    if (wanted->pointed_to_type && have->pointed_to_type) {
        bool pointee_match = check_that_types_match(wanted->pointed_to_type, have->pointed_to_type, true);
        if (is_pointer && !pointee_match) return false;
        return pointee_match;
    } else if (wanted->pointed_to_type || have->pointed_to_type) {
        return false;
    }

    if (wanted->struct_def && have->struct_def && wanted->struct_def == have->struct_def) {
        return true;
    }

    // No implicit promotions allowed INSIDE a pointer type
    if (is_pointer) return false;

    // Float promotions
    if ((wanted == _type->type_def_float || wanted == _type->type_def_float32) && have == _type->type_def_s64) return true;
    if ((wanted == _type->type_def_float || wanted == _type->type_def_float32) && have == _type->type_def_int) return true;

    if (wanted == _type->type_def_int && have == _type->type_def_s64) return true;
    if (wanted == _type->type_def_int && (have == _type->type_def_float || have == _type->type_def_float32)) return true;

    // Signed integer promotions
    if (wanted == _type->type_def_s16 && have == _type->type_def_s8) return true;
    if (wanted == _type->type_def_s32 && (have == _type->type_def_s8 || have == _type->type_def_s16)) return true;
    if (wanted == _type->type_def_s64 && (have == _type->type_def_s8 || have == _type->type_def_s16 || have == _type->type_def_s32)) return true;

    // Unsigned integer promotions
    if (wanted == _type->type_def_u16 && have == _type->type_def_u8) return true;
    if (wanted == _type->type_def_u32 && (have == _type->type_def_u8 || have == _type->type_def_u16)) return true;
    if (wanted == _type->type_def_u64 && (have == _type->type_def_u8 || have == _type->type_def_u16 || have == _type->type_def_u32)) return true;

    return false;
}

inline bool CodeManager::can_implicitly_convert_const(Ast_Expression *expr, Ast_Type_Definition *target) {
    if (!expr || expr->type != AST_LITERAL) return false;

    Ast_Literal *lit = static_cast<Ast_Literal*>(expr);
    if (lit->value_type != LITERAL_NUMBER) return false;

    long long value = lit->integer_value;

    return check_that_types_fit(value, target);
}


Ast_Type_Definition *CodeManager::extract_sizeof_type(Ast_Expression *expr) {
    if (!expr) return nullptr;

    // when no expression with sizeof()
    if (expr->type == AST_PROCEDURE_CALL_EXPRESSION) {
        auto *call = static_cast<Ast_Procedure_Call_Expression*>(expr);
        auto *fn = static_cast<Ast_Ident*>(call->function);

        if (fn->name && strcmp(fn->name, "sizeof") == 0) {
            if (call->arguments && call->arguments->arguments.count == 1) {
                Ast_Expression *type_arg = call->arguments->arguments.data[0];
                if (type_arg->type == AST_IDENT) {
                    auto *type_id = static_cast<Ast_Ident*>(type_arg);
                    return resolve_type_by_name(type_id->name);
                }
            }
        }
    }

    // when with an expression
    else if (expr->type == AST_BINARY) {
        auto *bin = static_cast<Ast_Binary*>(expr);
        if (bin->op == BINOP_MUL) {
            Ast_Type_Definition *left = extract_sizeof_type(bin->lhs);
            if (left) return left;

            Ast_Type_Definition *right = extract_sizeof_type(bin->rhs);
            if (right) return right;
        }
    }

    return nullptr;
}


// This is pretty badd........
Ast_Type_Definition *CodeManager::resolve_type_by_name(const char *name) {

    if (strcmp(name, "int") == 0) return interp->type->type_def_int;
    else if (strcmp(name, "s64") == 0) return interp->type->type_def_s64;
    else if (strcmp(name, "u8") == 0) return interp->type->type_def_u8;
    else if (strcmp(name, "float") == 0) return interp->type->type_def_float;
    else if (strcmp(name, "string") == 0) return interp->type->type_def_string;
    else if (strcmp(name, "bool") == 0) return interp->type->type_def_bool;
    else if (strcmp(name, "void") == 0) return interp->type->type_def_void;
    else if (strcmp(name, "Any") == 0) return interp->type->type_def_any;

    Ast_Type_Definition *struct_type = find_struct_type_in_scopes(name);
    if (struct_type) {
        return struct_type;
    }

    return nullptr;
}
