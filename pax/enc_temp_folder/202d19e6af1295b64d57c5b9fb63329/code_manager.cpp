#include "code_manager.h"

#include <cstdarg> // for variadic function
#include <cstring> // for linux strlen
#include <math.h> // for linux

#define AST_NEW(pool, type) ([&]() -> type* {                   \
    assert(pool != nullptr && "Pool must not be null");         \
    void* mem = pool_alloc(pool, sizeof(type));                \
    type* node = new (mem) type(pool);                         \
    return node;                                               \
}())

CodeManager::CodeManager(Pool* pool, Def_Type *type) : ast_pool(pool)
{
    scope_stack = ast_pool;
    Ast_Block *block = AST_NEW(ast_pool, Ast_Block);
    scope_stack.push_back(block); // global scope
    _type = type;
}

Ast_Literal *CodeManager::make_integer_literal(long long value){
    Ast_Literal *literal = AST_NEW(ast_pool, Ast_Literal);
    literal->value_type = LITERAL_NUMBER;
    literal->integer_value = value;
    return literal;
}


char *CodeManager::pool_strdup(Pool* pool, const char* str) {
    size_t len = strlen(str) + 1;
    char* p = (char*)pool_alloc(pool, len);
    memcpy(p, str, len);
    //printf("pool_strdup %d\"%.*s\"\n", len, len, p);
    return p;
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
    if (ast->line_number >= 0 && ast->character_number >= 0) {
        fprintf(stderr, "Semantic Error[%d:%d]: %s\n", ast->line_number, ast->character_number, buffer);
    } else {
        fprintf(stderr, "Semantic Error: %s\n", buffer);
    }
}

void CodeManager::report_error(int row, int col, const char* fmt, ...)
{
    constexpr size_t BUFFER_SIZE = 512;
    char buffer[BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, args);
    va_end(args);

    count_errors += 1;

    if (row >= 0 && col >= 0) {
        fprintf(stderr, "Semantic Error[%d:%d]: %s\n", row, col, buffer);
    } else {
        fprintf(stderr, "Semantic Error: %s\n", buffer);
    }
}

void CodeManager::push_scope()
{
    Ast_Block *block = AST_NEW(ast_pool, Ast_Block);
    scope_stack.push_back(block);
}

void CodeManager::pop_scope()
{
    if (!scope_stack.empty()) scope_stack.pop_back();
}

bool CodeManager::is_function_parameter(const char* name) {
    if (scope_stack.empty()) return false;

    // start with innermost
    for (int i = (int)scope_stack.size() - 1; i >= 0; --i) {
        Ast_Block *block = scope_stack.data[i];

        if(!block) continue;
        // look for func decl in this scope
        for (int j = 0; j < block->statements.count; ++j) {
                Ast_Statement *stmt = block->statements.data[j];
                if(!stmt || stmt->type == AST_DECLARATION) continue;

                Ast_Declaration *decl = static_cast<Ast_Declaration *>(stmt);

                if (decl && decl->is_function && decl->is_function_body) {

                // go through function's parameter lists
                for (int i = 0; i < decl->parameters.count; ++i) {
                    Ast_Declaration* param = decl->parameters.data[i];
                    if (param && param->identifier && strcmp(param->identifier->name, name) == 0) {
                        return true;
                    }
                }
                return false;
            }
        }
    }
    return false;
}

Ast_Declaration* CodeManager::lookup_symbol(const char* name, Ast_Block *scope) {
    for (int i = (int)scope_stack.size() - 1; i >= 0; --i) {
        Ast_Block* block;
        if(scope) block = scope;
        else block = scope_stack.data[i];
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

    if (!force_decl) {
        if (lookup_symbol_current_scope(decl->identifier->name)) {
            report_error(decl, "Variable '%s' already declared in this scope", decl->identifier->name);
            return false;
        }
    }

    Ast_Block* current_block = scope_stack.pop();

    decl->initialized = (decl->initializer != nullptr);

    current_block->statements.push_back(static_cast<Ast_Statement*>(decl));

    return true;
}

bool CodeManager::declare_function(Ast_Declaration* decl) {
    if (!decl || !decl->identifier || !decl->is_function) return false;

    if (decl->is_function_header) return false;

    if (lookup_symbol_current_scope(decl->identifier->name) && decl->is_function_body) {
        report_error(decl, "Function '%s' already declared in this scope", decl->identifier->name);
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

    if (lookup_symbol(struct_stmt->type_definition->struct_def) && decl->is_function_body) {
        report_error(decl, "Function '%s' already declared in this scope", decl->identifier->name);
        return false;
    }

    if (scope_stack.count == 0) return false;
    Ast_Block* current_block = scope_stack.data[scope_stack.count - 1];

    // Make the struct visible in this scope
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
        if (!stmt) continue;

        if (!fallthrough) {
            continue;
        }

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
    if (decl->return_type == _type->type_def_void) {
        return;
    }

    ReturnCheckResult result = checkReturnPaths(decl->my_scope);

    if (!result.has_return) {
        report_error(decl, "Non-void function '%s' must have a return statement", decl->identifier->name);
        return;
    }

    if (!result.all_paths_return) {
        // checkin maybe should just be a warning
        report_error(decl, "Not all control paths return a value in non-void function '%s'", decl->identifier->name);
    }
}

void CodeManager::resolve_idents(Ast_Block* block) {
    if (!block) return;

    bool is_global_scope = (scope_stack.size() == 1);

    for (int i = 0; i < block->statements.count; i++) {

        Ast_Statement* stmt = block->statements.data[i];

        if (!stmt) {
            continue;
        }

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
                            declare_variable(decl->parameters.data[j]);
                        }
                        resolve_idents(decl->my_scope);
                        pop_scope();
                    }

                } else {
                    Ast_Declaration* is_decl = lookup_symbol_current_scope(decl->identifier->name);
                    if (!is_decl) {
                        declare_variable(decl, true);
                    }

                    // Resolve initializer identifiers if it’s a call (your existing behavior)
                    if (decl->initializer && decl->initializer->type == AST_PROCEDURE_CALL_EXPRESSION) {
                        resolve_idents_in_declaration(decl);
                    }
                    // Try to resolve declared_type if it was parsed as an unresolved user type
                    else if (decl->declared_type && decl->declared_type->is_unresolved || (decl->declared_type && decl->declared_type->pointed_to_type)) {

                        // Walk to base type node (unwrap pointers/arrays) once
                        Ast_Type_Definition* base = decl->declared_type;
                        while (base->pointed_to_type || base->element_type) {
                            if (base->pointed_to_type) { base = base->pointed_to_type; continue; }
                            if (base->element_type)     { base = base->element_type;     continue; }
                        }

                        if (base->is_unresolved && base->name) {
                            // Attempt immediate resolution against visible scopes
                            Ast_Type_Definition* def = find_struct_type_in_scopes(base->name);
                            if (def && def->struct_def) {
                                // Patch the node in-place for immediate inference later
                                base->struct_def = def->struct_def;
                                base->is_unresolved = false;
                            } else {
                                // Could not resolve now; queue for the batch pass
                                CM_Unresolved_Type u;
                                u.decl = decl;
                                u.base_type = base;
                                u.line_number = decl->line_number;
                                u.character_number = decl->character_number;
                                unresolved_types.push_back(u);
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


void CodeManager::resolve_idents_in_declaration(Ast_Declaration* decl)
{
    if (!decl) return;  // guard [web:100]

    if (decl->initializer) {
        resolve_idents_in_expr(decl->initializer);  // resolve IDs in RHS [web:100]
    }

    // declared_type -> enqueue base unresolved
    if (decl->declared_type) {
        Ast_Type_Definition* t = decl->declared_type;
        while (t->pointed_to_type || t->element_type) {
            if (t->pointed_to_type) { t = t->pointed_to_type; continue; }  // pointer chain [web:100]
            if (t->element_type)     { t = t->element_type;     continue; } // array element [web:100]
        }
        if (t->is_unresolved && t->name) {
            CM_Unresolved_Type u{decl, t, decl->line_number, decl->character_number};
            unresolved_types.push_back(u);  // defer to queue [web:100]
        }
    }

    // return_type -> enqueue if you support it on functions
    if (decl->return_type) {
        Ast_Type_Definition* t = decl->return_type;
        while (t->pointed_to_type || t->element_type) {
            if (t->pointed_to_type) { t = t->pointed_to_type; continue; }  // pointer [web:100]
            if (t->element_type)     { t = t->element_type;     continue; } // array [web:100]
        }
        if (t->is_unresolved && t->name) {
            CM_Unresolved_Type u{decl, t, decl->line_number, decl->character_number};
            unresolved_types.push_back(u);  // defer [web:100]
        }
    }
}

Ast_Type_Definition* CodeManager::find_struct_type_in_scopes(const char* name) const {
    if (!name) return nullptr;  // guard [web:100]
    for (int si = (int)scope_stack.size() - 1; si >= 0; --si) {  // innermost to outermost [web:174]
        Ast_Block* b = scope_stack.data[si];
        if (!b) continue;  // guard [web:100]
        for (int i = 0; i < b->statements.count; ++i) {
            Ast_Statement* s = b->statements.data[i];
            if (!s) continue;  // guard [web:100]
            if (s->expression && s->expression->type == AST_STRUCT && s->type_definition) {
                Ast_Struct* st = static_cast<Ast_Struct*>(s->expression);
                if (st->name && strcmp(st->name, name) == 0) {
                    return s->type_definition;  // has struct_def set by parser [web:100]
                }
            }
        }
    }
    return nullptr;  // not found [web:100]
}

void CodeManager::resolve_unresolved_types_queue() {
    std::vector<CM_Unresolved_Type> still_unresolved;  // keep those not yet bound [web:100]

    for (const auto& u : unresolved_types) {
        Ast_Type_Definition* base = u.base_type;
        if (!base) continue;  // guard [web:100]
        if (!base->is_unresolved) continue;  // already bound [web:100]
        if (base->struct_def) { base->is_unresolved = false; continue; }  // already linked [web:100]
        if (!base->name) { still_unresolved.push_back(u); continue; }  // no name to search [web:100]

        if (Ast_Type_Definition* def = find_struct_type_in_scopes(base->name)) {
            if (def->struct_def) {
                base->struct_def = def->struct_def;  // link struct definition [web:100]
                base->is_unresolved = false;         // mark resolved [web:100]
                continue;
            }
        }
        still_unresolved.push_back(u);  // try again later or error at end [web:100]
    }

    unresolved_types.swap(still_unresolved);  // update queue [web:100]
}

void CodeManager::resolve_unresolved_member_accesses_queue() {
    std::vector<CM_Unresolved_Member_Access> still_unresolved;

    for (const auto& u : unresolved_member_accesses) {
        // Try resolving again now that more types might be inferred
        Ast_Declaration* field = resolve_member_access(u.dot_expr, u.my_scope);
        if (!field) {
            still_unresolved.push_back(u);
        }
    }

    unresolved_member_accesses.swap(still_unresolved);

    // If any still unresolved at the end, report errors
    if (!unresolved_member_accesses.empty()) {
        for (const auto& u : unresolved_member_accesses) {
            report_error(u.line_number, u.character_number,
                "Cannot resolve member access: base expression has unknown type");
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

Ast_Declaration* CodeManager::resolve_member_access(Ast_Binary* dot_expr, Ast_Block *my_scope) {
    Ast_Type_Definition* base_type = nullptr;

    // Handle nested dots recursively
    if (dot_expr->lhs->type == AST_BINARY) {
        Ast_Binary* base_dot = static_cast<Ast_Binary*>(dot_expr->lhs);
        if (base_dot->op == BINOP_DOT) {
            Ast_Declaration* nested_field = resolve_member_access(base_dot, my_scope);
            if (!nested_field) {
                CM_Unresolved_Member_Access u;
                u.dot_expr = dot_expr;
                u.line_number = dot_expr->line_number;
                u.character_number = dot_expr->character_number;
                u.my_scope = scope_stack.pop();
                unresolved_member_accesses.push_back(u);
                return nullptr;
            }
            base_type = nested_field->declared_type;

            // NEW: Check if nested field's type is resolved
            if (!base_type) {
                // Nested field exists but type not resolved yet - queue
                CM_Unresolved_Member_Access u;
                u.dot_expr = dot_expr;
                u.line_number = dot_expr->line_number;
                u.character_number = dot_expr->character_number;
                u.my_scope = scope_stack.pop();
                unresolved_member_accesses.push_back(u);
                return nullptr;
            }

            // NEW: Check if it's still marked unresolved
            Ast_Type_Definition* check_resolved = base_type;
            while (check_resolved->pointed_to_type || check_resolved->element_type) {
                if (check_resolved->pointed_to_type) { check_resolved = check_resolved->pointed_to_type; continue; }
                if (check_resolved->element_type) { check_resolved = check_resolved->element_type; continue; }
            }
            if (check_resolved->is_unresolved) {
                // Type not fully resolved yet - queue
                CM_Unresolved_Member_Access u;
                u.dot_expr = dot_expr;
                u.line_number = dot_expr->line_number;
                u.character_number = dot_expr->character_number;
                u.my_scope = scope_stack.pop();
                unresolved_member_accesses.push_back(u);
                return nullptr;
            }
        }
    }
    Ast_Declaration* base_decl = nullptr;
    // Handle identifier base
    if (!base_type && dot_expr->lhs->type == AST_IDENT) {
        Ast_Ident* base_id = static_cast<Ast_Ident*>(dot_expr->lhs);
        base_decl = lookup_symbol(base_id->name, my_scope);

        if (!base_decl) {
            report_error(base_id, "Undeclared variable '%s'", base_id->name);
            return nullptr;
        }

        base_type = base_decl->declared_type;

        // Try eager inference if no type yet
        if (!base_type && base_decl->initializer) {
            infer_types_expr(&base_decl->initializer);
            base_type = base_decl->initializer->inferred_type;
            if (base_type) {
                dot_expr->lhs->inferred_type = base_type;
            }
        }

        // Queue if still unresolved
        if (!base_type) {
            CM_Unresolved_Member_Access u;
            u.dot_expr = dot_expr;
            u.line_number = dot_expr->line_number;
            u.character_number = dot_expr->character_number;
            u.my_scope = scope_stack.pop();
            unresolved_member_accesses.push_back(u);
            return nullptr;
        }
    }

    // Handle other expressions via inferred_type
    if (!base_type) {
        if (!dot_expr->lhs->inferred_type) {
            resolve_idents_in_expr(dot_expr->lhs);
        }
        base_type = dot_expr->lhs->inferred_type;

        if (!base_type) {
            report_error(dot_expr->lhs, "Cannot determine type of base expression");
            return nullptr;
        }
    }

    // Auto-dereference all pointer levels
    while (base_type->pointed_to_type) {
        base_type = base_type->pointed_to_type;
    }

    // Validate base is a struct
    if (!base_type->struct_def) {

        if (base_type->is_unresolved) {
            CM_Unresolved_Member_Access u;
            u.dot_expr = dot_expr;
            u.line_number = dot_expr->line_number;
            u.character_number = dot_expr->character_number;
            u.my_scope = scope_stack.pop();
            unresolved_member_accesses.push_back(u);
        }
        return nullptr;
    }

    // Look up member in struct
    Ast_Ident* member_id = static_cast<Ast_Ident*>(dot_expr->rhs);
    Ast_Struct* sd = base_type->struct_def;

    for (int i = 0; i < sd->members.count; ++i) {
        Ast_Declaration* m = sd->members.data[i];
        if (m && m->identifier && m->identifier->name &&
            strcmp(m->identifier->name, member_id->name) == 0) {
            return m;
        }
    }

    report_error(member_id, "Struct '%s' has no member '%s'",
        sd->name ? sd->name : "(unknown)", member_id->name);
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
            // report_error(id, "Use of undeclared identifier '%s'", id->name);
            CM_Unresolved_Variable unresolved;
            unresolved.ident = id;
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

        if (b->op == BINOP_ASSIGN)
        {
            if (b->rhs) {
                resolve_idents_in_expr(b->rhs);
            }


            if (Ast_Binary* lhs_dot = ast_static_cast<Ast_Binary>(b->lhs, AST_BINARY)) {
                    Ast_Declaration* field = resolve_member_access(lhs_dot);
                    return;
            } else if (Ast_Ident* lhs_ident = ast_static_cast<Ast_Ident>(b->lhs, AST_IDENT)) {
                Ast_Declaration *is_decl= lookup_symbol(lhs_ident->name);
                if (!is_decl) {
                    // report_error(lhs_ident, "Assignment to undeclared variable '%s'", lhs_ident->name);
                    CM_Unresolved_Variable unresolved;
                    unresolved.ident = lhs_ident;
                    unresolved.line_number = lhs_ident->line_number;
                    unresolved.character_number = lhs_ident->character_number;
                    unresolved_vars.push_back(unresolved);

                    expr->inferred_type = _type->type_def_dummy;
                } else {
                    is_decl->initialized = true;
                }
            } else if (Ast_Unary* lhs_unary = ast_static_cast<Ast_Unary>(b->lhs, AST_UNARY)) {
                if (lhs_unary->op == UNARY_DEREFERENCE) {
                    resolve_idents_in_expr(lhs_unary->operand);
                } else {
                    report_error(lhs_unary, "Unsupported unary operation on LHS of assignment");
                }
            } else {
                report_error(b, "Left-hand side of assignment must be an identifier or dereferenced pointer");
            }
        }

        else
        {
            if (b->op == BINOP_DOT) {
                if (Ast_Binary* lhs_dot = ast_static_cast<Ast_Binary>(b->lhs, AST_BINARY)) {
                        Ast_Declaration* field = resolve_member_access(lhs_dot);
                }

            } else {
                if (b->lhs) resolve_idents_in_expr(b->lhs);
                if (b->rhs) resolve_idents_in_expr(b->rhs);
            }
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
                    unresolved.line_number = fn->line_number;
                    unresolved.character_number = fn->character_number;
                    unresolved_calls.push_back(unresolved);
                } else if (!decl->is_function) {
                    report_error(fn, "'s' is not a function", fn->name);
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
            }
        }

        if (call->arguments) {

            for (int i = 0; i < call->arguments->arguments.count; ++i) {
                Ast_Expression* a = call->arguments->arguments.data[i];
                resolve_idents_in_expr(a);
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
                while(base_type->pointed_to_type){
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
                        CM_Unresolved_Type u;
                        u.decl = decl;
                        u.base_type = base_type;
                        u.line_number = decl->line_number;
                        u.character_number = decl->character_number;
                        unresolved_types.push_back(u);
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
            report_error(unresolved.line_number, unresolved.character_number, "Use of undeclared variable '%s'", unresolved.ident->name);
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
            report_error(unresolved.line_number, unresolved.character_number, "'%s' is not a function", fn->name);
            continue;
        }

        if (call->arguments) {
            for (int i = 0; i < call->arguments->arguments.count; ++i) {
                Ast_Expression* arg = call->arguments->arguments.data[i];
                resolve_idents_in_expr(arg);
            }
        }

        int call_arg_count = call->arguments ? call->arguments->arguments.count : 0;
        int decl_arg_count = decl->parameters.count;
        if (call_arg_count != decl_arg_count) {
            report_error(unresolved.line_number, unresolved.character_number, "Function '%s' expects %d arguments, but %d were provided",
                         fn->name, decl_arg_count, call_arg_count);
            continue;
        }
        // we infer args later on....
    }

    unresolved_calls = still_unresolved;

    if (scope_stack.size() == 1 && !unresolved_calls.empty()) {
        for (const auto& unresolved : unresolved_calls) {
            auto *fn = static_cast<Ast_Ident*>(unresolved.call->function);
            report_error(unresolved.line_number, unresolved.character_number, "Call to undeclared function '%s'", fn->name);
        }
        unresolved_calls.clear();
    }
}

char* CodeManager::type_to_string(Ast_Type_Definition* type) {

    if (!type) {
        return pool_strdup(ast_pool, "unknown");
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

    return pool_strdup(ast_pool, type_str.c_str());
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
    Ast_Expression *expr = *expr_ptr;
    if (!expr_ptr) return;
    if(expr->inferred_type) return;  // this saves us unnessary checks

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
                report_error(id, "Use of undeclared identifier '%s'", id->name);  // COME BACK TO THIS SHIT
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

            Ast_Type_Definition* resultType = AST_NEW(ast_pool, Ast_Type_Definition);
            switch (u->op) {
            case UNARY_DEREFERENCE: {
                if (!operandType->pointed_to_type || operandType == _type->type_def_dummy) {
                    report_error(u, "Cannot dereference non-pointer type");
                    expr->inferred_type = _type->type_def_dummy;
                    break;
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

                    if(lt && lt == _type->type_def_dummy || rt == _type->type_def_dummy){
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
                        }
                        // pointer arithmetic
                        else if (lt && lt->pointed_to_type && (rt == _type->type_def_int || rt == _type->type_def_s64)) {
                            expr->inferred_type = lt; // Result is the pointer type (e.g., ^int)
                        } else {
                            report_error(b, "Type error in binary arithmetic: operand types incompatible");
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

                    // Auto-dereference all pointer levels
                    while (base_type->pointed_to_type) {
                        base_type = base_type->pointed_to_type;
                    }

                    if (!base_type->struct_def) {
                        report_error(b->lhs, "Member access requires a struct, got '%s'",
                            type_to_string(b->lhs->inferred_type));
                        expr->inferred_type = _type->type_def_dummy;
                        break;
                    }

                    // Parser guarantees RHS is an identifier
                    Ast_Ident* member = static_cast<Ast_Ident*>(b->rhs);
                    Ast_Struct* sd = base_type->struct_def;

                    // Look up member and set type
                    for (int i = 0; i < sd->members.count; ++i) {
                        Ast_Declaration* m = sd->members.data[i];
                        if (m && m->identifier && m->identifier->name &&
                            strcmp(m->identifier->name, member->name) == 0) {
                            expr->inferred_type = m->declared_type ? m->declared_type : _type->type_def_dummy;
                            break;  // Early exit after finding member
                        }
                    }

                    // If we didn't find it (inferred_type not set), report error
                    if (!expr->inferred_type) {
                        report_error(member, "Struct '%s' has no member '%s'",
                            sd->name ? sd->name : "(unknown)", member->name);
                        expr->inferred_type = _type->type_def_dummy;
                    }
                    break;
                }


                case BINOP_ASSIGN: {
                    infer_types_expr(&b->rhs);
                    Ast_Type_Definition *rhsType = b->rhs->inferred_type;
                    if (rhsType == _type->type_def_dummy) {
                        report_error(b,
                                     "Right-hand side of assignment has unknown type");
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
                                report_error(b,
                                    "Type mismatch in member assignment. Expected '%s' Got '%s'",
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
                            report_error(lhs_ident,
                                "Type mismatch in assignment to '%s'. Expected '%s' Got '%s'",
                                lhs_ident->name, type_to_string(is_decl->declared_type), type_to_string(rhsType));
                        }
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
                                bool is_var_param = is_function_parameter(pointer_ident->name);
                                if (pointer_decl && !pointer_decl->initialized && !is_var_param) {
                                    // report_error(lhs_unary,
                                        // "Cannot dereference uninitialized pointer '%s'",
                                        // pointer_ident->name);
                                    expr->inferred_type = _type->type_def_dummy;
                                    return;
                                }
                            }

                            // Type-check assignment
                            if (!check_that_types_match(lhsType, rhsType)) {
                                report_error(lhs_unary,
                                    "Type mismatch: cannot assign '%s' to dereferenced pointer of type '%s'",
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

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression *call = static_cast<Ast_Procedure_Call_Expression *>(expr);

            Ast_Type_Definition *return_type = nullptr;

            if (call->function) {
                Ast_Ident *fn = static_cast<Ast_Ident *>(call->function);
                if (fn->name && strcmp(fn->name, "printf") != 0) {
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
                                if (!check_that_types_match(param_type, arg_type)) {
                                    report_error(fn, "Type mismatch for argument %d in call to '%s'. Expected '%s', Got '%s'"
                                                        ,i + 1, fn->name,type_to_string(param_type), type_to_string(arg_type));
                                }
                            }
                        }

                    } else {
                        return_type = _type->type_def_void;
                    }
                } else {
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
                    // TEMPORARY WE CAN implement the flag during compilation to only throw this warning if the programmer wants it thrown or not, (just like c compilers)
                    printf("Warning: Constant overflow in '%s'. %lld wrapped to %lld for type '%s'\n",
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
                        decl->declared_type->pointed_to_type) {

                        if(!check_that_types_match(decl->declared_type->pointed_to_type, init_type))
                        {
                            report_error(decl,
                                        "Type mismatch in initializer for '%s', Expected '%s' Got '%s'",
                                        decl->identifier ? decl->identifier->name : "null",
                                        type_to_string(decl->declared_type),
                                        type_to_string(init_type));
                        }
                        // ^int assigned to ^^int
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
                infer_types_decl(decl);

                Ast_Declaration* is_decl = lookup_symbol_current_scope(decl->identifier ? decl->identifier->name : "");
                if (!is_decl) {
                    declare_variable(decl);
                }
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

    // direct full pointer match
    if (wanted == have) return true;

    if (wanted->array_kind != ARRAY_NONE || have->array_kind != ARRAY_NONE) {
        if (wanted->array_kind != have->array_kind ||
            (wanted->array_kind == ARRAY_STATIC && wanted->static_array_size != have->static_array_size) ||
            !wanted->element_type || !have->element_type) {
            return false;
        }
        return check_that_types_match(wanted->element_type, have->element_type);
    }

    // if pointer both type
    if (wanted->pointed_to_type || have->pointed_to_type) {

        bool pointee_match = check_that_types_match(wanted->pointed_to_type, have->pointed_to_type, true);
        if (is_pointer && !pointee_match) return false;  // Strict for pointer types
        return pointee_match;

    } else if ((wanted->struct_def && have->struct_def) && (wanted->struct_def == have->struct_def)) {
        return true; // checking structs
    }
    // No promotions if is_pointer
    if (is_pointer) return false;  // Already checked equality above

    //
    // Critical!!! not tested heavily yett
    //

    // integer promotions: smaller signed/unsigned to larger
    if ((wanted == _type->type_def_float || wanted == _type->type_def_float32) && have == _type->type_def_s64) return true;
    if (wanted == _type->type_def_int && have == _type->type_def_s64) return true;
    if ((wanted == _type->type_def_float || wanted == _type->type_def_float32) && have == _type->type_def_int) return true;
    if (wanted == _type->type_def_s16 && have == _type->type_def_s8) return true;
    if (wanted == _type->type_def_s32 && (have == _type->type_def_s8 || have == _type->type_def_s16)) return true;
    if (wanted == _type->type_def_s64 && (have == _type->type_def_s8 || have == _type->type_def_s16 || have == _type->type_def_s32)) return true;
    if (wanted == _type->type_def_u16 && have == _type->type_def_u8) return true;
    if (wanted == _type->type_def_u32 && (have == _type->type_def_u8 || have == _type->type_def_u16)) return true;
    if (wanted == _type->type_def_u64 && (have == _type->type_def_u8 || have == _type->type_def_u16 || have == _type->type_def_u32)) return true;

    // Signed/unsigned
    if (wanted == _type->type_def_s32 && have == _type->type_def_u32) return true;
    if (wanted == _type->type_def_u32 && have == _type->type_def_s32) return true;
    if (wanted == _type->type_def_s64 && have == _type->type_def_u64) return true;
    if (wanted == _type->type_def_u64 && have == _type->type_def_s64) return true;

    // float promotions
    if (wanted == _type->type_def_float64 && have == _type->type_def_float32) return true;
    if (wanted == _type->type_def_float64 && have == _type->type_def_s32) return true;
    if (wanted == _type->type_def_float64 && have == _type->type_def_s64) return true;
    // Add float32 -> s32?

    if (wanted == _type->type_def_s32 && have == _type->type_def_bool) return true;  // bool as int

    return false;
}