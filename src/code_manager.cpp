#include "code_manager.h"
#include "interp.h"
#include "tools.h"

#include <cstdarg> // for variadic function
#include <cstring> // for linux strlen
#include <math.h> // for linux

#define AST_NEW(type) ([&]() -> type* {                   \
    assert(interp->pool != nullptr && "Pool must not be null");         \
    void* mem = pool_alloc(interp->pool, sizeof(type));                \
    type* node = new (mem) type(interp->pool);                         \
    node->file_name = interp->current_file;                    \
    return node;                                               \
}())

CodeManager::CodeManager(Pax_Interp *_interp)
{
    interp = _interp;

    scope_stack = interp->pool; // have to pass in the pool to the Array<>
    Ast_Block *block = AST_NEW(Ast_Block);
    scope_stack.push_back(block); // global scope

    _type = interp->type;
}

Ast_Literal *CodeManager::make_integer_literal(long long value){
    Ast_Literal *literal = AST_NEW(Ast_Literal);
    literal->value_type = LITERAL_NUMBER;
    literal->integer_value = value;
    return literal;
}

template<typename T>
void CodeManager::report_error(T type, const char* fmt, ...)
{
    constexpr size_t BUFFER_SIZE = 512;
    char buffer[BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, args);
    va_end(args);

    count_errors += 1;

    Ast *ast = static_cast<Ast *>(type);

    const char* filename = ast->file_name;
    if (!filename || !filename[0]) {
        filename = interp->current_file ? interp->current_file : "<unknown>";
    }

    if (ast->line_number >= 0 && ast->character_number >= 0) {
        fprintf(stderr, "%s: Semantic Error[%d:%d]: %s\n", filename, ast->line_number, ast->character_number, buffer);
    } else {
        fprintf(stderr, "%s: Semantic Error: %s\n", filename, buffer);
    }
}

template<typename T, typename P>
void CodeManager::report_error_with_previous(T node, P previous, const char* fmt, ...) {
    constexpr size_t BUFFER_SIZE = 512;
    char buffer[BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, args);
    va_end(args);

    count_errors += 1;

    Ast* ast_node = static_cast<Ast*>(node);
    Ast* ast_prev = static_cast<Ast*>(previous);

    const char* filename = ast_node->file_name;
    if (!filename || !filename[0]) {
        filename = interp->current_file ? interp->current_file : "<unknown>";
    }

    if (ast_node->line_number >= 0 && ast_node->character_number >= 0) {
        fprintf(stderr, "%s\n%s:[%d:%d]: %s.",
                        "\x1B[0;36m", filename, ast_node->line_number, ast_node->character_number, buffer);
    } else {
        fprintf(stderr, "%s\n%s: %s.", "\x1B[0;36m", filename, buffer);
    }

    const char* prev_filename = ast_prev->file_name;
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
    if (!scope_stack.empty()) scope_stack.pop_back();
}

Ast_Declaration *CodeManager::lookup_symbol(const char *name, Ast_Block *scope) {
    for (int i = (int)scope_stack.size() - 1; i >= 0; --i) {

        Ast_Block* block = scope ? scope : scope_stack.data[i]; // this is for the case where we can't rely on scope_stack.pop() during resolve_member_access, we have to pass the scope of the unresolved dot expression that was saved during initial queuing. Its kinda messy i dont wanna deal with it right now ugh,.........
        if(!block) continue;

        for (int j = 0; j < block->statements.count; ++j) {
            Ast_Statement* stmt = block->statements.data[j];
            if (!stmt) continue;

            if(stmt->type == AST_DECLARATION){
                Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
                if (decl->identifier && strcmp(decl->identifier->name, name) == 0) {
                    return decl;
                }
            }

            if(stmt->expression && stmt->expression->type == AST_STRUCT){
                auto *str_def = static_cast<Ast_Struct *>(stmt->expression);
                if(str_def && strcmp(str_def->name, name) == 0){
                    return static_cast<Ast_Declaration *>(stmt);
                }
            }
        }
    }
    return nullptr;
}


Ast_Declaration* CodeManager::lookup_symbol_current_scope(const char* name) {
    if (scope_stack.empty()) return nullptr;

    Ast_Block* block = scope_stack.pop();
    if(!block) return nullptr;

    for (int j = 0; j < block->statements.count; ++j) {
        Ast_Statement* stmt = block->statements.data[j];
        if (!stmt || stmt->type != AST_DECLARATION) continue;

        Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
        if (decl->identifier && strcmp(decl->identifier->name, name) == 0) {
            return decl;
        }
    }
    return nullptr;
}

bool CodeManager::declare_variable(Ast_Declaration* decl, bool force_decl) {
    if (!decl || !decl->identifier) return false;

    auto *looked_up = lookup_symbol_current_scope(decl->identifier->name);
    if (!force_decl && looked_up) {
        report_error_with_previous(decl, looked_up, "Variable '%s' already declared", decl->identifier->name);
        return false;
    }

    Ast_Block* current_block = scope_stack.pop();

    decl->initialized = (decl->initializer != nullptr);

    current_block->statements.push_back(static_cast<Ast_Statement*>(decl));

    return true;
}

bool CodeManager::declare_function(Ast_Declaration* decl) {
    if (!decl || !decl->identifier || !decl->is_function) return false;

    // if (decl->is_function_header) return false;

    auto *looked_up = lookup_symbol_current_scope(decl->identifier->name);
    if ( looked_up && looked_up->is_function) {
        report_error_with_previous(decl, looked_up, "Function '%s' already declared", decl->identifier->name);
        return false;
    }
    else if(looked_up && !looked_up->is_function){
        report_error(decl, "Redefinition of '%s', previous definition is not a function", decl->identifier->name);
        return false;
    }

    Ast_Block* current_block = scope_stack.pop();

    decl->initialized = decl->is_function_body;

    current_block->statements.push_back(static_cast<Ast_Statement*>(decl));

    return true;
}

bool CodeManager::declare_struct(Ast_Statement* struct_stmt) {
    if (!struct_stmt) return false;
    if (!struct_stmt->expression || struct_stmt->expression->type != AST_STRUCT) return false;

    auto *struct_name = struct_stmt->type_definition->struct_def->name;
    auto *looked_up = lookup_symbol(struct_name);
    if (looked_up) {
        report_error_with_previous(struct_stmt, looked_up, "Struct '%s' already defined.", struct_name);
        return false;
    }

    if (scope_stack.count == 0) return false;
    Ast_Block* current_block = scope_stack.data[scope_stack.count - 1];

    current_block->statements.push_back(struct_stmt);
    return true;
}


ReturnCheckResult CodeManager::checkReturnPaths(Ast_Block* block)
{
    ReturnCheckResult result = {false, false};
    if (!block) return result;

    bool fallthrough = true;

    for (size_t i = 0; i < block->statements.count; ++i) {
        Ast_Statement* stmt = block->statements.data[i];
        if (!stmt || !fallthrough) continue;

        if (stmt->is_return) {
            result.has_return = true;
            fallthrough = false;
            break;
        } else if (stmt->type == AST_IF) {
            Ast_If* ifn = static_cast<Ast_If*>(stmt);
            ReturnCheckResult badResult = {false, false};
            ReturnCheckResult then_result = ifn->then_block ? checkReturnPaths(ifn->then_block) : badResult;

            //  add check for else_if_blocks once implemeneted
            ReturnCheckResult else_result = ifn->else_block ? checkReturnPaths(ifn->else_block) : badResult;

            result.has_return |= then_result.has_return || else_result.has_return;

            if (then_result.all_paths_return && else_result.all_paths_return) {
                fallthrough = false;
                break;
            }
        } else if (stmt->block && stmt->block->is_scoped_block) {
            // recurse normal blocks/scoped
            ReturnCheckResult block_result = checkReturnPaths(stmt->block);
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

void CodeManager::checkFunctionReturns(Ast_Declaration* decl) {
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

void CodeManager::resolve_idents(Ast_Block* block) {
    if (!block) return;

    bool is_global_scope = (scope_stack.size() == 1);

    for (int i = 0; i < block->statements.count; i++) {

        Ast_Statement* stmt = block->statements.data[i];
        if (!stmt) continue;

        if (stmt->is_return) {
            resolve_idents_in_expr(stmt->expression);
            continue;
        }

        if (stmt->expression && stmt->expression->type == AST_STRUCT){
            declare_struct(stmt);
            resolve_idents_in_expr(stmt->expression);

        } else if (stmt->type == AST_DECLARATION){

            Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
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

                // Resolve initializers and function bodies
                resolve_idents_in_declaration(decl);
                if (decl->is_function && decl->my_scope && decl->is_function_body) {
                    push_scope();
                    for (int j = 0; j < decl->parameters.count; ++j) {
                        declare_variable(decl->parameters.data[j]);
                        resolve_idents_in_declaration(decl->parameters.data[j]);
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
                        for (int j = 0; j < decl->parameters.count; ++j) {
                            Ast_Declaration* param = decl->parameters.data[j];
                            declare_variable(param);
                            resolve_idents_in_declaration(param);
                        }
                        resolve_idents(decl->my_scope);
                        pop_scope();
                    }

                } else {
                    Ast_Declaration* is_decl = lookup_symbol_current_scope(decl->identifier->name);
                    if (!is_decl) {
                        declare_variable(decl, true);
                    }
                    else {
                        report_error(decl, "Variable '%s' already declared.", decl->identifier->name);
                    }

                    if (decl->declared_type && !decl->is_function) {
                        Ast_Type_Definition* base = decl->declared_type;
                        while (base->pointed_to_type || base->element_type) {
                            if (base->pointed_to_type) { base = base->pointed_to_type; continue; }
                            if (base->element_type) { base = base->element_type; continue; }
                        }

                        if (base->struct_def) {
                            decl->declared_type = clone_type_definition(decl->declared_type);
                            create_type_instantiation(decl->declared_type);
                        }
                    }

                    if (decl->initializer /* && decl->initializer->type == AST_PROCEDURE_CALL_EXPRESSION */) {
                        resolve_idents_in_declaration(decl);
                    }
                    // if declared_type is_unresolved user type (struct), try to resolve it
                    else if (decl->declared_type && decl->declared_type->is_unresolved || (decl->declared_type && decl->declared_type->pointed_to_type)) {

                        Ast_Type_Definition* base = decl->declared_type;
                        while (base->pointed_to_type || base->element_type) {
                            if (base->pointed_to_type) { base = base->pointed_to_type; continue; }
                            if (base->element_type)     { base = base->element_type;     continue; }
                        }

                        if (base->is_unresolved && base->name) {
                            Ast_Type_Definition* def = find_struct_type_in_scopes(base->name);
                            if (def && def->struct_def) {

                                base->struct_def = def->struct_def;
                                base->is_unresolved = false;

                                decl->declared_type = clone_type_definition(decl->declared_type);
                                create_type_instantiation(decl->declared_type);
                            } else {
                                // unresolvable at first pass, so queue it
                                push_unresolved_type(decl, base);
                            }
                        }
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
                push_scope();
                resolve_idents(stmt->block);
                pop_scope();
            }
            continue;
        }

        if (stmt->type == AST_IF) {
            Ast_If* ifn = static_cast<Ast_If*>(stmt);
            if (ifn->condition) resolve_idents_in_expr(ifn->condition);
            if (ifn->then_block) {
                push_scope();
                resolve_idents(ifn->then_block);
                pop_scope();
            }
            if (ifn->else_block) {
                push_scope();
                resolve_idents(ifn->else_block);
                pop_scope();
            }
        } else if (stmt->expression) {
            resolve_idents_in_expr(stmt->expression);
        } else if (stmt->block) {
            push_scope();
            if (stmt->block->is_scoped_block) {
                push_scope();
            }
            resolve_idents(stmt->block);
            if (stmt->block->is_scoped_block) {
                pop_scope();
            }
            pop_scope();
        }
    }

}


Ast_Type_Definition* CodeManager::clone_type_definition(Ast_Type_Definition* original) {
    if (!original) return nullptr;

    if (!original->pointed_to_type /* && !original->element_type && original->array_kind == ARRAY_NONE */) {
        return original;
    }

    Ast_Type_Definition* clone = AST_NEW(Ast_Type_Definition);

    clone->line_number = original->line_number;
    clone->character_number = original->character_number;

    clone->struct_def = original->struct_def;
    clone->name = original->name;
    clone->is_reference = original->is_reference;
    clone->is_unresolved = original->is_unresolved;
    clone->type_instance = nullptr;

    clone->pointed_to_type = clone_type_definition(original->pointed_to_type);
    clone->array_kind = original->array_kind;
    clone->element_type = clone_type_definition(original->element_type);
    clone->static_array_size = original->static_array_size;

    return clone;
}

void CodeManager::create_type_instantiation(Ast_Type_Definition* type) {
    if (!type) return;

    Ast_Type_Definition* base_type = type;
    while (base_type->pointed_to_type) {
        base_type = base_type->pointed_to_type;
    }

    if (!base_type->struct_def) return;
    if (base_type->type_instance) return;

    Ast_Type_Instantiation* instance = AST_NEW(Ast_Type_Instantiation);
    base_type->type_instance = instance; // this must be set first

    Ast_Struct* struct_def = base_type->struct_def;
    for (int i = 0; i < struct_def->members.count; ++i) {
        Ast_Declaration* def_member = struct_def->members.data[i];

        Ast_Declaration* instance_member = AST_NEW(Ast_Declaration);
        instance_member->identifier = def_member->identifier;

        // **Handle inferred types (no declared_type)**
        if (def_member->declared_type) {
            instance_member->declared_type = clone_type_definition(def_member->declared_type);
        } else if (def_member->initializer) {
            // Member has inferred type - don't clone, share the inferred type
            // The type will be inferred from initializer later
            instance_member->declared_type = nullptr;
            instance_member->inferred = false;
        } else {
            instance_member->declared_type = nullptr;
        }

        instance_member->initializer = def_member->initializer;
        instance_member->initialized = false;
        instance_member->is_declaration_function_argument = false;

        // recurse and create non pointer type struct instances within struct definitions
        if (instance_member->declared_type) {
            Ast_Type_Definition* member_base = instance_member->declared_type;
            while (member_base->pointed_to_type || member_base->element_type) {
                if (member_base->pointed_to_type) { member_base = member_base->pointed_to_type; continue; }
                /*if (member_base->element_type) { member_base = member_base->element_type; continue; } */
            }

            if (member_base->name && !member_base->struct_def) {
                Ast_Type_Definition* resolved = find_struct_type_in_scopes(member_base->name);
                if (resolved && resolved->struct_def) {
                    member_base->struct_def = resolved->struct_def;
                    member_base->is_unresolved = false;
                }
            }

            create_type_instantiation(instance_member->declared_type);
        }

        // else if (instance_member->initializer && !instance_member->declared_type) {
        //     // infer_types_expr(&instance_member->initializer);
        //     // instance_member->declared_type = instance_member->initializer->inferred_type;
        // }

        instance->member_instances.push_back(instance_member);
    }

}



void CodeManager::resolve_idents_in_declaration(Ast_Declaration* decl)
{
    if (!decl) return;

    if (decl->initializer) {
        resolve_idents_in_expr(decl->initializer);
    }

    // declared_type -> enqueue base unresolved
    if (decl->declared_type) {
        Ast_Type_Definition* t = decl->declared_type;
        while (t->pointed_to_type || t->element_type) {
            if (t->pointed_to_type) { t = t->pointed_to_type; continue; }
            if (t->element_type)     { t = t->element_type;     continue; }
        }
        if (t->is_unresolved && t->name) {
            push_unresolved_type(decl, t);
        }
    }

    // return_type -> enqueue if you support it on functions
    if (decl->return_type) {
        Ast_Type_Definition* t = decl->return_type;
        while (t->pointed_to_type || t->element_type) {
            if (t->pointed_to_type) { t = t->pointed_to_type; continue; }
            if (t->element_type)     { t = t->element_type;     continue; }
        }
        if (t->is_unresolved && t->name) {
            push_unresolved_type(decl, t);
        }
    }
}

Ast_Type_Definition* CodeManager::find_struct_type_in_scopes(const char* name) const {
    if (!name) return nullptr;
    for (int si = (int)scope_stack.size() - 1; si >= 0; --si) {
        Ast_Block* b = scope_stack.data[si];
        if (!b) continue;
        for (int i = 0; i < b->statements.count; ++i) {
            Ast_Statement* s = b->statements.data[i];
            if (!s) continue;
            if (s->expression && s->expression->type == AST_STRUCT && s->type_definition) {
                Ast_Struct* st = static_cast<Ast_Struct*>(s->expression);
                if (st->name && strcmp(st->name, name) == 0) {
                    return s->type_definition;
                }
            }
        }
    }
    return nullptr;
}


void CodeManager::resolve_unresolved_vars() {
    std::vector<CM_Unresolved_Variable> still_unresolved;

    for (const auto& unresolved : unresolved_vars) {
        Ast_Declaration* decl = lookup_symbol(unresolved.ident->name);
        if (!decl) {
            still_unresolved.push_back(unresolved);
            continue;
        }
    }

    unresolved_vars = still_unresolved;

    if (scope_stack.size() == 1 && !unresolved_vars.empty()) {
        for (const auto& unresolved : unresolved_vars) {
            report_error(unresolved.ident, "Use of undeclared variable '%s'", unresolved.ident->name);
        }
        unresolved_vars.clear();
    }
}
void CodeManager::resolve_unresolved_calls() {
    std::vector<CM_Unresolved_Call> still_unresolved;

    for (const auto& unresolved : unresolved_calls) {
        Ast_Procedure_Call_Expression* call = unresolved.call;
        auto *fn = static_cast<Ast_Ident*>(call->function);
        Ast_Declaration* decl= lookup_symbol(fn->name);

        if (!decl) {
            still_unresolved.push_back(unresolved);
            continue;
        }

        if (!decl->is_function) {
            report_error(call, "'%s' is not a function", fn->name);
            continue;
        }

        if (call->arguments) {
            for (int i = 0; i < call->arguments->arguments.count; ++i) {
                Ast_Expression* arg = call->arguments->arguments.data[i];
                scope_stack.push_back(unresolved.my_scope);
                resolve_idents_in_expr(arg);
                scope_stack.pop_back();

            }
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

    if (scope_stack.size() == 1 && !unresolved_calls.empty()) {
        for (const auto& unresolved : unresolved_calls) {
            auto *fn = static_cast<Ast_Ident*>(unresolved.call->function);
            report_error(unresolved.call, "Call to undeclared function '%s'", fn->name);
        }
        unresolved_calls.clear();
    }
}

void CodeManager::resolve_unresolved_types() {
    std::vector<CM_Unresolved_Type> still_unresolved;

    for (const auto& u : unresolved_types) {
        Ast_Type_Definition* base = u.base_type;
        if (!base) continue;
        if (!base->is_unresolved) continue;
        if (base->struct_def) { base->is_unresolved = false; continue; }
        if (!base->name) { still_unresolved.push_back(u); continue; }

        if (Ast_Type_Definition* def = find_struct_type_in_scopes(base->name)) {
            if (def->struct_def) {
                base->struct_def = def->struct_def;
                base->is_unresolved = false;

                u.decl->declared_type = clone_type_definition(u.decl->declared_type);
                create_type_instantiation(u.decl->declared_type);

                continue;
            }
        } else {
            report_error(u.decl, "Unresolved type '%s'", u.base_type->name);
        }
        still_unresolved.push_back(u);
    }

    unresolved_types.swap(still_unresolved);
}


// int temp = 0;
void CodeManager::resolve_unresolved_member_accesses() {
    std::vector<CM_Unresolved_Member_Access> still_unresolved;

    for (const auto& u : unresolved_member_accesses) {
        Ast_Declaration* field = resolve_member_access(u.dot_expr, u.my_scope, false, true,/*should_infer=*/true);
        if (!field) {
            still_unresolved.push_back(u);
            continue;
        }
        // printf("[%d,%d]\n", u.line_number, u.character_number);
        // printf("Numer %d, resolved to member at: %p, name: %s\n", ++temp, field, field->identifier->name);

        if (u.assignment_expr) {
            // printf("Numer %d, inside if at: %p\n", temp, u.assignment_expr);

            scope_stack.push_back(u.my_scope);
            resolve_idents_in_expr(u.assignment_expr->rhs);
            field->initializer = u.assignment_expr->rhs;
            field->initialized = true;

            scope_stack.pop_back();
        }
    }

    unresolved_member_accesses.swap(still_unresolved);
    if (!unresolved_member_accesses.empty()) {
        for (const auto& u : unresolved_member_accesses) {
            report_error(u.dot_expr, "Cannot resolve member access: base expression has unknown type");
        }
        unresolved_member_accesses.clear();
    }
}


static Ast_Declaration* find_struct_member(Ast_Type_Definition* struct_type, const char* member_name) {
    if (!struct_type || !struct_type->struct_def || !member_name) return nullptr;
    Ast_Struct* sd = struct_type->struct_def;
    for (int i = 0; i < sd->members.count; ++i) {
        Ast_Declaration* m = sd->members.data[i];
        if (m && m->identifier && m->identifier->name && strcmp(m->identifier->name, member_name) == 0) {
            return m;
        }
    }
    return nullptr;
}
void CodeManager::push_unresolved_type(Ast_Declaration *decl, Ast_Type_Definition *base_type){
    CM_Unresolved_Type u;
    u.decl = decl;
    u.base_type = base_type;
    u.line_number = decl->line_number;
    u.character_number = decl->character_number;
    unresolved_types.push_back(u);
}

void CodeManager::push_unresolved_member_access(Ast_Binary *dot_expr){
    CM_Unresolved_Member_Access u;
    u.dot_expr = dot_expr;
    // u.parent_expr = expr;
    u.line_number = dot_expr->line_number;
    u.character_number = dot_expr->character_number;
    u.my_scope = scope_stack.pop();
    unresolved_member_accesses.push_back(u);
}



Ast_Declaration* CodeManager::resolve_member_access(Ast_Binary* dot_expr, Ast_Block *my_scope, bool skip_init_check, bool skip_queuing, bool should_infer) {
    Ast_Type_Definition* base_type = nullptr;
    Ast_Declaration* base_decl = nullptr;
    Ast_Type_Instantiation* current_instance = nullptr;

    if (dot_expr->lhs->type == AST_BINARY) {
        Ast_Binary* base_dot = static_cast<Ast_Binary*>(dot_expr->lhs);
        if (base_dot->op == BINOP_DOT) {
            Ast_Declaration* nested_field = resolve_member_access(base_dot, my_scope, skip_init_check, true, should_infer); // skip_queuing at first bad resolve to avoid sub dot_expr nodes being pushed to unresolved queue
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

            Ast_Type_Definition* check_resolved = base_type;
            while (check_resolved->pointed_to_type /* || check_resolved->element_type */) {
                if (check_resolved->pointed_to_type) {
                    check_resolved = check_resolved->pointed_to_type;
                    continue;
                }
                // if (check_resolved->element_type) {
                //     check_resolved = check_resolved->element_type;
                //     continue;
                // }
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
        Ast_Ident* base_name = static_cast<Ast_Ident*>(dot_expr->lhs);
        base_decl = lookup_symbol(base_name->name, my_scope);

        if (!base_decl) {
            report_error(base_name, "Undeclared member variable '%s'", base_name->name);
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
            resolve_idents_in_expr(dot_expr->lhs);
        }
        base_type = dot_expr->lhs->inferred_type;

        if (!base_type) {
            report_error(dot_expr->lhs, "Cannot determine type of base expression");
            return nullptr;
        }

        while (base_type->pointed_to_type) {
            base_type = base_type->pointed_to_type;
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

    Ast_Ident* member_id = static_cast<Ast_Ident*>(dot_expr->rhs);

    if (current_instance) {
        for (int i = 0; i < current_instance->member_instances.count; ++i) {
            Ast_Declaration* m = current_instance->member_instances.data[i];
            if (m && m->identifier && m->identifier->name && strcmp(m->identifier->name, member_id->name) == 0) {
                // Set the inferred type on this dot expression
                if (m->declared_type) {
                    dot_expr->inferred_type = m->declared_type;
                }
                return m;
            }
        }
    } else {
        report_error(dot_expr, "INTERNAL, no struct definition was found. We should not be here!"); // TEMPORARY
        return nullptr;
    }

    Ast_Struct* sd = base_type->struct_def;
    report_error(member_id, "Struct '%s' has no member '%s'", sd->name ? sd->name : "(unknown)", member_id->name);
    return nullptr;
}


void CodeManager::resolve_idents_in_expr(Ast_Expression* expr)
{
    if (!expr) return;

    switch (expr->type) {
    case AST_IDENT: {
        auto *id = static_cast<Ast_Ident*>(expr);
        Ast_Declaration* decl = lookup_symbol(id->name);
        if (!decl) {
            CM_Unresolved_Variable unresolved;
            unresolved.ident = id;
            unresolved.my_scope = scope_stack.pop();
            unresolved.line_number = id->line_number;
            unresolved.character_number = id->character_number;
            unresolved_vars.push_back(unresolved);
        }

        break;
    }

    case AST_LITERAL:
        break;

    case AST_UNARY: {
        auto *u = static_cast<Ast_Unary*>(expr);
        if (!u->operand) break;

        resolve_idents_in_expr(u->operand);

        break;
    }


    case AST_BINARY: {
        auto *b = static_cast<Ast_Binary*>(expr);

        if (b->op == BINOP_ASSIGN) {

            if (Ast_Binary* lhs_dot = ast_static_cast<Ast_Binary>(b->lhs, AST_BINARY)) {
                if (lhs_dot->op == BINOP_DOT) {
                    Ast_Declaration* lhs_field = resolve_member_access(lhs_dot, scope_stack.pop(), false, true);

                    if (!lhs_field) {
                        // TEMPORARY, we only pass the assignment_expr for this Unresolved_Member_Access queue
                        CM_Unresolved_Member_Access u;
                        u.dot_expr = lhs_dot;
                        u.assignment_expr = b;
                        u.my_scope = scope_stack.pop();
                        u.line_number = lhs_dot->line_number;
                        u.character_number = lhs_dot->character_number;
                        unresolved_member_accesses.push_back(u);
                        return;
                    }

                    resolve_idents_in_expr(b->rhs);

                    lhs_field->initializer = b->rhs;
                    lhs_field->initialized = true;

                    if (lhs_field->declared_type && lhs_field->declared_type->pointed_to_type) {

                        if (b->rhs->type == AST_UNARY) {
                            Ast_Unary* rhs_unary = static_cast<Ast_Unary*>(b->rhs);
                            if (rhs_unary->op == UNARY_ADDRESS_OF && rhs_unary->operand) {
                                Ast_Declaration* rhs_decl = nullptr;

                                if (rhs_unary->operand->type == AST_IDENT) {
                                    Ast_Ident* rhs_id = static_cast<Ast_Ident*>(rhs_unary->operand);
                                    rhs_decl = lookup_symbol(rhs_id->name);
                                }
                                else if (rhs_unary->operand->type == AST_BINARY) {
                                    Ast_Binary* rhs_member = static_cast<Ast_Binary*>(rhs_unary->operand);
                                    if (rhs_member->op == BINOP_DOT) {
                                        rhs_decl = resolve_member_access(rhs_member, scope_stack.pop(), false);
                                    }
                                }

                                if (rhs_decl && rhs_decl->declared_type) {
                                    Ast_Type_Definition* pointed_to = lhs_field->declared_type->pointed_to_type;
                                    Ast_Type_Definition* rhs_base = rhs_decl->declared_type;
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
            else if (Ast_Ident* lhs_ident = ast_static_cast<Ast_Ident>(b->lhs, AST_IDENT)) {

                resolve_idents_in_expr(b->rhs);

                Ast_Declaration *is_decl = lookup_symbol(lhs_ident->name);
                if (!is_decl) {
                    CM_Unresolved_Variable unresolved;
                    unresolved.ident = lhs_ident;
                    unresolved.my_scope = scope_stack.pop();
                    unresolved.line_number = lhs_ident->line_number;
                    unresolved.character_number = lhs_ident->character_number;
                    unresolved_vars.push_back(unresolved);
                } else {
                    // is_decl->initializer = b->rhs;
                    is_decl->initialized = true;
                }
            }
            else if (Ast_Unary* lhs_unary = ast_static_cast<Ast_Unary>(b->lhs, AST_UNARY)) {
                resolve_idents_in_expr(b->rhs);
                if (lhs_unary->op == UNARY_DEREFERENCE) {
                    resolve_idents_in_expr(lhs_unary->operand);
                } else {
                    report_error(lhs_unary, "Unsupported unary operation on LHS of assignment");
                }
            }
            else {
                report_error(b, "Left-hand side of assignment must be an identifier or dereferenced pointer");
            }
        }
        else if (b->op == BINOP_DOT) {
            Ast_Declaration* field = resolve_member_access(b, scope_stack.pop(), false, true);
            if (field) {
                b->inferred_type = field->declared_type;
            }
            else {
                push_unresolved_member_access(b);
            }

        }
        else {
            if (b->lhs) resolve_idents_in_expr(b->lhs);
            if (b->rhs) resolve_idents_in_expr(b->rhs);
        }
        break;
    }

    case AST_PROCEDURE_CALL_EXPRESSION: {
        auto *call = static_cast<Ast_Procedure_Call_Expression*>(expr);

        if (call->function)
        {
            auto *fn = static_cast<Ast_Ident*>(call->function);

            if (fn->name && strcmp(fn->name, "printf") != 0){

                Ast_Declaration *decl= lookup_symbol(fn->name);
                if (!decl) {
                    CM_Unresolved_Call unresolved;
                    unresolved.call = call;
                    unresolved.my_scope = scope_stack.pop();
                    unresolved.line_number = fn->line_number;
                    unresolved.character_number = fn->character_number;
                    unresolved_calls.push_back(unresolved);
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

                        for (int i = 0; i < call_arg_count; ++i) {
                            Ast_Expression* arg = call->arguments->arguments.data[i];
                            resolve_idents_in_expr(arg);
                        }
                    }
                }
            } else {  // we want to resolve args in printf too
                for (int i = 0; i < call->arguments->arguments.count; ++i) {
                    Ast_Expression* arg = call->arguments->arguments.data[i];
                    resolve_idents_in_expr(arg);
                }

            }
        }

        if (call->arguments) {
            for (int i = 0; i < call->arguments->arguments.count; ++i) {
                Ast_Expression* arg = call->arguments->arguments.data[i];

                // flagging the tag for assuming that the local uninitialized pointer of struct/member
                // is atleast being to a proc call (in the hopes that function will initialize it)
                if (Ast_Unary* addr = ast_static_cast<Ast_Unary>(arg, AST_UNARY)) {
                    if (addr->op == UNARY_ADDRESS_OF) {

                        if (Ast_Ident* var = ast_static_cast<Ast_Ident>(addr->operand, AST_IDENT)) {
                            Ast_Declaration* decl = lookup_symbol(var->name);
                            if (decl) {
                                decl->is_declaration_passed_through_function = true;
                            }
                        }
                        else if (Ast_Binary* dot = ast_static_cast<Ast_Binary>(addr->operand, AST_BINARY)) {
                            if (dot->op == BINOP_DOT && dot->lhs->type == AST_IDENT) {
                                Ast_Ident* base_id = static_cast<Ast_Ident*>(dot->lhs);
                                Ast_Declaration* base_decl = lookup_symbol(base_id->name);
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

        for (int i = 0; i < args->arguments.count; ++i) {
            Ast_Expression *a = args->arguments.data[i];
            resolve_idents_in_expr(a);
        }
        break;
    }

    case AST_STRUCT: {
        auto *s = static_cast<Ast_Struct *>(expr);
        if(!s) return;

        for(int i = 0; i < s->members.count; ++i){
            Ast_Declaration *decl = s->members.data[i];

            if(decl) {
                Ast_Type_Definition *base_type = decl->declared_type;
                while(base_type && base_type->pointed_to_type){
                    if(base_type->pointed_to_type) {
                        base_type = base_type->pointed_to_type;
                    }
                }
                if(base_type && base_type->is_unresolved){
                    auto *struct_n = base_type->name;
                    Ast_Declaration *struct_ = lookup_symbol(struct_n);
                    auto* def_ = reinterpret_cast<Ast_Type_Definition*>(struct_);
                    if(struct_) {
                        base_type->struct_def = def_->pointed_to_type->struct_def;
                        base_type->is_unresolved = false;
                    } else {
                        push_unresolved_type(decl, base_type);
                    }
                }
                continue;
            }
        }
        break;
    }

    default: break;
    }
}


char* CodeManager::type_to_string(Ast_Type_Definition* type) {

    if (!type) {
        return pool_strdup(interp->pool, "unknown");
    }

    Ast_Type_Definition* base_type = type;
    int pointer_depth = 0;
    while (base_type->pointed_to_type) {
        base_type = base_type->pointed_to_type;
        pointer_depth++;
    }

    std::string type_str;  // Temporary replace with char *

    for (int i = 0; i < pointer_depth; ++i) {
        type_str += "^";
    }

    if (type->is_reference) {
        type_str = "&";
    }

    if (type->array_kind == ARRAY_STATIC && type->element_type) {

        char* element_str = type_to_string(type->element_type);
        type_str += element_str;
        type_str += "[" + std::to_string(type->static_array_size) + "]";
    } else {
        type_str += base_type->to_string(*_type);
    }

    return pool_strdup(interp->pool, type_str.c_str());
}

void CodeManager::infer_types_return(Ast_Statement* ret, Ast_Declaration* func_decl) {
    if (!ret || !func_decl) return;

    Ast_Type_Definition* func_return_type = func_decl->return_type ? func_decl->return_type : _type->type_def_void;

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
        Ast_Type_Definition* return_expr_type = ret->expression->inferred_type;
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


void CodeManager::infer_types_expr(Ast_Expression** expr_ptr)
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

            if (decl)
            {
                if (decl->is_function) {
                    expr->inferred_type = decl->return_type;
                }else if (decl->declared_type) {
                    expr->inferred_type = decl->declared_type;
                } else if (decl->initialized && !decl->inferred) {
                    infer_types_expr(&decl->initializer);
                    expr->inferred_type = decl->initializer->inferred_type;
                } else {
                    expr->inferred_type = _type->type_def_dummy;
                }
            } else {
                // report_error(id, "Use of undeclared identifier '%s'", id->name);  // COME BACK TO THIS SHIT
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
                report_error(u,
                    "Could not determine type of operand for unary expression");
                expr->inferred_type = _type->type_def_dummy;

            }

            Ast_Type_Definition* resultType = AST_NEW(Ast_Type_Definition);
            switch (u->op) {
            case UNARY_DEREFERENCE: {
                if (!operandType->pointed_to_type || operandType == _type->type_def_dummy) {
                    report_error(u, "Cannot dereference non-pointer type");
                    expr->inferred_type = _type->type_def_dummy;
                    break;
                }

                if (Ast_Ident* operand_ident = ast_static_cast<Ast_Ident>(u->operand, AST_IDENT)) {
                    Ast_Declaration* decl = lookup_symbol(operand_ident->name);
                    if (decl && !decl->initialized && !decl->is_declaration_function_argument) {
                        report_error(u, "Cannot dereference uninitialized pointer '%s'", operand_ident->name);
                        expr->inferred_type = _type->type_def_dummy;
                        break;
                    }
                }
                else if (Ast_Binary* operand_dot = ast_static_cast<Ast_Binary>(u->operand, AST_BINARY)) {
                    if (operand_dot->op == BINOP_DOT) {
                        Ast_Block* current_scope = scope_stack.size() > 0 ? scope_stack.pop() : nullptr;
                        Ast_Declaration* member = resolve_member_access(operand_dot, current_scope, false);

                        // should skip when a type is that uninitialized pointer is being passed to function call in this case we assume that that function will initialize it so dont throw error.
                        bool should_skip = false;
                        if (operand_dot->lhs->type == AST_IDENT) {
                            Ast_Ident* base_id = static_cast<Ast_Ident*>(operand_dot->lhs);
                            Ast_Declaration* base_var = lookup_symbol(base_id->name);

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
                        }else if (lt == _type->type_def_float64 || rt == _type->type_def_float64) {
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

                    Ast_Type_Definition* base_type = b->lhs->inferred_type;

                    if (!base_type) {
                        report_error(b->lhs, "Cannot determine type of base expression");
                        expr->inferred_type = _type->type_def_dummy;
                        break;
                    }

                    Ast_Type_Definition* dereferenced = base_type;
                    while (dereferenced->pointed_to_type) {
                        dereferenced = dereferenced->pointed_to_type;
                    }

                    if (!dereferenced->struct_def) {
                        report_error(b->lhs, "Member access requires a struct");
                        expr->inferred_type = _type->type_def_dummy;
                        break;
                    }

                    Ast_Ident* member = static_cast<Ast_Ident*>(b->rhs);

                    Ast_Type_Definition* member_type = nullptr;
                    Ast_Declaration *member_decl = nullptr;
                    if (dereferenced->type_instance) {
                        for (int i = 0; i < dereferenced->type_instance->member_instances.count; ++i) {
                            Ast_Declaration* m = dereferenced->type_instance->member_instances.data[i];
                            if (m && m->identifier && m->identifier->name &&
                                strcmp(m->identifier->name, member->name) == 0) {
                                member_type = m->declared_type;
                                member_decl = m;
                                break;
                            }
                        }
                    } else {
                        Ast_Struct* sd = dereferenced->struct_def;
                        for (int i = 0; i < sd->members.count; ++i) {
                            Ast_Declaration* m = sd->members.data[i];
                            if (m && m->identifier && m->identifier->name &&
                                strcmp(m->identifier->name, member->name) == 0) {
                                member_type = m->declared_type;
                                member_decl = m;
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
                    infer_types_expr(&b->rhs);
                    Ast_Type_Definition *rhsType = b->rhs->inferred_type;
                    if (rhsType == _type->type_def_dummy) {
                        report_error(b, "Right-hand side of assignment has unknown type");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    Ast_Type_Definition *lhsType = nullptr;

                    // Case 2: Member assignment s.grade = ... (NEW)
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
                                report_error(b, "Type mismatch in member assignment. Expected '%s' Got '%s'",
                                                 type_to_string(lhsType), type_to_string(rhsType));
                            }
                        } else {
                            report_error(b, "Invalid left-hand side of assignment");
                            expr->inferred_type = _type->type_def_dummy;
                            break;
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
                        // is_decl->initializer = b->rhs;
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

            Ast_Block* temp_scope = AST_NEW(Ast_Block);
            for(int i = 0; i < s->members.count; ++i){
                temp_scope->statements.push_back(s->members.data[i]);
            }
            scope_stack.push_back(temp_scope);

            for(int i = 0; i < s->members.count; ++i){
                Ast_Declaration *decl = s->members.data[i];
                if(decl) {
                    infer_types_decl(decl);
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
                if (fn->name && strcmp(fn->name, "printf") != 0 && strcmp(fn->name, "sizeof") != 0&& strcmp(fn->name, "malloc") != 0) {
                    Ast_Declaration *is_decl = lookup_symbol(fn->name);

                    if (is_decl && is_decl->is_function) {
                        return_type = is_decl->return_type ? is_decl->return_type : _type->type_def_void;

                        if (is_decl->return_type && !check_that_types_match(return_type, is_decl->return_type)) {
                            report_error(fn, "Function '%s' return type mismatch", fn->name);
                        }

                        int call_arg_count = call->arguments ? call->arguments->arguments.count : 0;

                        if (call->arguments) {

                            for (int i = 0; i < call_arg_count; ++i) {
                                Ast_Expression* arg = call->arguments->arguments.data[i];

                                infer_types_expr(&arg);
                                Ast_Type_Definition* arg_type = arg->inferred_type;
                                Ast_Type_Definition* param_type = is_decl->parameters.data[i]->declared_type;
                                // if (!check_that_types_match(param_type, arg_type)) {
                                //     report_error(fn, "Type mismatch for argument %d in call to '%s'. Expected '%s', Got '%s'"
                                //                         ,i + 1, fn->name,type_to_string(param_type), type_to_string(arg_type));
                                // }

                                if (!check_that_types_match(param_type, arg_type)) {
                                    // Try implicit constant conversion (e.g., s64 literal -> u8/u16/u32/u64, s32 etc.)
                                    if (!can_implicitly_convert_const(arg, param_type)) {
                                        report_error(fn,
                                            "Type mismatch for argument %d in call to '%s'. Expected '%s', Got '%s'",
                                            i + 1, fn->name, type_to_string(param_type), type_to_string(arg_type));
                                    } else {
                                        // Optional: update the literal's inferred_type to param_type
                                        arg->inferred_type = param_type;
                                    }
                                }
                            }
                        }

                    } else {
                        return_type = _type->type_def_void;
                    }
                } else if (fn->name && strcmp(fn->name, "sizeof") == 0) {
                    // 1) Exactly one argument
                    int arg_count = call->arguments ? call->arguments->arguments.count : 0;
                    if (arg_count != 1) {
                        report_error(fn, "sizeof() expects exactly 1 argument");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    Ast_Expression* arg = call->arguments->arguments.data[0];

                    // 2) Must be an identifier, used as a type name
                    if (arg->type != AST_IDENT) {
                        report_error(arg, "sizeof() argument must be a type identifier");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    auto* type_ident = static_cast<Ast_Ident*>(arg);

                    // 3) Resolve builtin or struct type by name
                    Ast_Type_Definition* resolved = resolve_type_by_name(type_ident->name);
                    if (!resolved) {
                        // Optional: distinguish “variable” vs “no symbol at all”
                        Ast_Declaration* var = lookup_symbol(type_ident->name);
                        if (var) {
                            report_error(arg,
                                "'%s' is a value/variable, but sizeof() expects a type name",
                                type_ident->name);
                        } else {
                            report_error(arg,
                                "Unknown type '%s' used in sizeof()",
                                type_ident->name);
                        }
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                    }

                    // All good: sizeof(T) has type s64
                    expr->inferred_type = _type->type_def_s64;
                    return;
                } else if (fn->name && strcmp(fn->name, "malloc") == 0) {

                    // Default return type is ^void
                    Ast_Type_Definition* return_type = _type->type_def_void; // or create one

                    if (call->arguments && call->arguments->arguments.count > 0) {
                        Ast_Expression* arg0 = call->arguments->arguments.data[0];

                        // USE THE HELPER HERE
                        Ast_Type_Definition* T = extract_sizeof_type(arg0);

                        if (T) {
                             // FOUND IT! We want to return ^T
                             Ast_Type_Definition* ptr_to_T = AST_NEW(Ast_Type_Definition);
                             ptr_to_T->pointed_to_type = T;
                             return_type = ptr_to_T;
                        }
                    }

                    // Still infer arguments normally (malloc expects s64/int)
                    if (call->arguments) {
                        for (int i = 0; i < call->arguments->arguments.count; ++i) {
                            Ast_Expression* arg = call->arguments->arguments.data[i];
                            infer_types_expr(&arg);
                        }
                    }

                    expr->inferred_type = return_type;
                    return;
                }


                else {
                    return_type = _type->type_def_void;
                }
            }


            if (call->arguments)
            {
                for (int i = 0; i < call->arguments->arguments.count; ++i)
                {
                    Ast_Expression* p = call->arguments->arguments.data[i];
                    infer_types_expr(&p);
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



void CodeManager::infer_types_decl(Ast_Declaration* decl) {
    if (!decl) return;

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

                    if (init_type && init_type->pointed_to_type &&
                        decl->declared_type->pointed_to_type && check_that_types_match(decl->declared_type->pointed_to_type, init_type)) {
                    // ^int assigned to ^^int
                    // this is ugly code but can't bothered to factor it right now
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


void CodeManager::infer_types_block(Ast_Block* block, Ast_Declaration *my_func)
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

                    for (int j = 0; j < decl->parameters.count; ++j) {
                        declare_variable(decl->parameters.data[j]);
                    }
                    infer_types_block(decl->my_scope, decl); // Infer types in function body
                    pop_scope();
                }

            } else {

                Ast_Declaration* is_decl = lookup_symbol_current_scope(decl->identifier ? decl->identifier->name : "");
                if (!is_decl) {
                    declare_variable(decl);
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
            Ast_If *ifn = static_cast<Ast_If *>(stmt);
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
                push_scope();
                infer_types_block(ifn->else_block, my_func);
                pop_scope();
            }
        }

    }
}


bool CodeManager::check_that_types_match(Ast_Type_Definition *wanted, Ast_Type_Definition* have, bool is_pointer) {
    if (!wanted || !have || wanted == _type->type_def_dummy || have == _type->type_def_dummy) return false;

    if (wanted == have) return true;

    // treat `Any` as "accept any type" right now only bounded to sizeof()
    if (wanted == _type->type_def_any || have == _type->type_def_any) return true;


    // VOID POINTER LOGIC
    // Is 'wanted' a generic ^void
    // then match any pointer type of 'have' with ^void
    if (wanted->pointed_to_type && wanted->pointed_to_type == _type->type_def_void) {
        if (have->pointed_to_type) return true;
        if (have == _type->type_def_null) return true;
    }
    if (have->pointed_to_type && have->pointed_to_type == _type->type_def_void) {
        if (wanted->pointed_to_type) return true;
    }
    if (have == _type->type_def_null && wanted->pointed_to_type) return true;

    // Array checks
    if (wanted->array_kind != ARRAY_NONE || have->array_kind != ARRAY_NONE) {
        if (wanted->array_kind != have->array_kind ||
            (wanted->array_kind == ARRAY_STATIC && wanted->static_array_size != have->static_array_size) ||
            !wanted->element_type || !have->element_type) {
            return false;
        }
        return check_that_types_match(wanted->element_type, have->element_type);
    }


    if (wanted->pointed_to_type || have->pointed_to_type) {
        if (!wanted->pointed_to_type || !have->pointed_to_type) return false;

        bool pointee_match = check_that_types_match(wanted->pointed_to_type, have->pointed_to_type, true);
        if (is_pointer && !pointee_match) return false;

        return pointee_match;
    }
    else if ((wanted->struct_def && have->struct_def) && (wanted->struct_def == have->struct_def)) {
        return true;
    }

    // No implicit promotions allowed INSIDE a pointer type
    if (is_pointer) return false;

    // float promotions
    if ((wanted == _type->type_def_float || wanted == _type->type_def_float32) && have == _type->type_def_s64) return true;
    if (wanted == _type->type_def_int && have == _type->type_def_s64) return true;
    if ((wanted == _type->type_def_float || wanted == _type->type_def_float32) && have == _type->type_def_int) return true;

    // signed integer promotions
    if (wanted == _type->type_def_s16 && have == _type->type_def_s8) return true;
    if (wanted == _type->type_def_s32 && (have == _type->type_def_s8 || have == _type->type_def_s16)) return true;
    if (wanted == _type->type_def_s64 && (have == _type->type_def_s8 || have == _type->type_def_s16 || have == _type->type_def_s32)) return true;

    // unsigned integer promotions
    if (wanted == _type->type_def_u16 && have == _type->type_def_u8) return true;
    if (wanted == _type->type_def_u32 && (have == _type->type_def_u8 || have == _type->type_def_u16)) return true;
    if (wanted == _type->type_def_u64 && (have == _type->type_def_u8 || have == _type->type_def_u16 || have == _type->type_def_u32)) return true;

    // signed/unsigned
    if (wanted == _type->type_def_s32 && have == _type->type_def_u32) return true;
    if (wanted == _type->type_def_u32 && have == _type->type_def_s32) return true;
    if (wanted == _type->type_def_s64 && have == _type->type_def_u64) return true;
    if (wanted == _type->type_def_u64 && have == _type->type_def_s64) return true;

    // float32/float64
    if (wanted == _type->type_def_float64 && have == _type->type_def_float32) return true;
    if (wanted == _type->type_def_float64 && have == _type->type_def_s32) return true;
    if (wanted == _type->type_def_float64 && have == _type->type_def_s64) return true;

    // bool/int
    if (wanted == _type->type_def_s32 && have == _type->type_def_bool) return true;

    return false;
}

inline bool CodeManager::can_implicitly_convert_const(Ast_Expression* expr, Ast_Type_Definition* target) {
    if (!expr || expr->type != AST_LITERAL) return false;

    Ast_Literal* lit = static_cast<Ast_Literal*>(expr);
    if (lit->value_type != LITERAL_NUMBER) return false;

    long long value = lit->integer_value;

    return check_that_types_fit(value, target);
}


Ast_Type_Definition* CodeManager::extract_sizeof_type(Ast_Expression* expr) {
    if (!expr) return nullptr;

    // when no expression with sizeof()
    if (expr->type == AST_PROCEDURE_CALL_EXPRESSION) {
        auto* call = static_cast<Ast_Procedure_Call_Expression*>(expr);
        auto* fn = static_cast<Ast_Ident*>(call->function);

        if (fn->name && strcmp(fn->name, "sizeof") == 0) {
            if (call->arguments && call->arguments->arguments.count == 1) {
                Ast_Expression* type_arg = call->arguments->arguments.data[0];
                if (type_arg->type == AST_IDENT) {
                    auto* type_id = static_cast<Ast_Ident*>(type_arg);
                    return resolve_type_by_name(type_id->name);
                }
            }
        }
    }

    // when with an expression
    else if (expr->type == AST_BINARY) {
        auto* bin = static_cast<Ast_Binary*>(expr);
        if (bin->op == BINOP_MUL) {
            Ast_Type_Definition* left = extract_sizeof_type(bin->lhs);
            if (left) return left;

            Ast_Type_Definition* right = extract_sizeof_type(bin->rhs);
            if (right) return right;
        }
    }

    return nullptr;
}


// This is pretty badd........
Ast_Type_Definition* CodeManager::resolve_type_by_name(const char* name) {

    if (strcmp(name, "int") == 0) return interp->type->type_def_int;
    else if (strcmp(name, "s64") == 0) return interp->type->type_def_s64;
    else if (strcmp(name, "u8") == 0) return interp->type->type_def_u8;
    else if (strcmp(name, "float") == 0) return interp->type->type_def_float;
    else if (strcmp(name, "string") == 0) return interp->type->type_def_string;
    else if (strcmp(name, "bool") == 0) return interp->type->type_def_bool;
    else if (strcmp(name, "void") == 0) return interp->type->type_def_void;
    else if (strcmp(name, "Any") == 0) return interp->type->type_def_any;

    Ast_Type_Definition* struct_type = find_struct_type_in_scopes(name);
    if (struct_type) {
        return struct_type;
    }

    return nullptr;
}
