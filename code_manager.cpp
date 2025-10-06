#include "code_manager.h"

#include <cstdarg> // for variadic function

#define AST_NEW(pool, type) ([&]() -> type* {                   \
    assert(pool != nullptr && "Pool must not be null");         \
    void* mem = pool_alloc(pool, sizeof(type));                \
    type* node = new (mem) type(pool);                         \
    return node;                                               \
}())

CodeManager::CodeManager(Pool* pool, Def_Type *type) : ast_pool(pool) {
    scopes.clear();
    scopes.emplace_back(); // global scope
    _type = type;
}
char *CodeManager::pool_strdup(Pool* pool, const char* str) {
    size_t len = strlen(str) + 1;
    char* p = (char*)pool_alloc(pool, len);
    memcpy(p, str, len);
    //printf("pool_strdup %d\"%.*s\"\n", len, len, p);
    return p;
}


int CodeManager::get_count_errors(){
    return count_errors;
}

void CodeManager::report_error(int line, int col, const char* fmt, ...)  // Temporary we inherit Ast so dont need to pass line and col implicitly change it
{
    constexpr size_t BUFFER_SIZE = 512;
    char buffer[BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, args);
    va_end(args);

    count_errors += 1;
    if (line >= 0 && col >= 0) {
        fprintf(stderr, "Semantic Error[%d:%d]: %s\n", line, col, buffer);
    } else {
        fprintf(stderr, "Semantic Error: %s\n", buffer);
    }
}

void CodeManager::push_scope()
{
    scopes.emplace_back();
}

void CodeManager::pop_scope()
{
    if (!scopes.empty()) scopes.pop_back();
}

bool CodeManager::is_function_parameter(const char* name) {
    if (scopes.empty()) return false;

    // start with innermost
    for (int i = (int)scopes.size() - 1; i >= 0; --i) {
        CM_Scope& scope = scopes[i];

        // look for func decl in this scope
        for (auto& sym : scope) {
            if (sym.is_function && sym.decl && sym.is_function_body) {

                // go through function's parameter lists
                Ast_Declaration* func_decl = sym.decl;
                for (int i = 0; i < func_decl->parameters.count; ++i) {
                    Ast_Declaration* param = func_decl->parameters.data[i];
                    if (param->identifier && strcmp(param->identifier->name, name) == 0) {
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

bool CodeManager::declare_variable(Ast_Declaration* decl)
{
    CM_Scope &scope = scopes.back();

    for (auto &sym : scope) {
        if (strcmp(sym.name, decl->identifier->name) == 0) {
            //if (!decl->initializer)
            report_error(decl->line_number, decl->character_number, "Variable '%s' already declared", decl->identifier->name);
            return false;
        }
    }

    CM_Symbol sym;
    sym.name = pool_strdup(ast_pool, decl->identifier->name);
    sym.decl = decl;
    sym.type = decl->declared_type;
    sym.initialized = (decl->initializer != nullptr);

    scope.push_back(sym);
    return true;
}
bool CodeManager::declare_function(Ast_Declaration* decl) {
    if (!decl || !decl->is_function || !decl->identifier) return false;

    if (decl->is_function_header) return false;
    CM_Scope &scope = scopes.back();

    // checks if function are duplicate declaration stricly by the function name, so in the future it will change this is just here temporarily. (obviously we want to check for same types as well as the function name because we will allow functions with same names but different types but not same name and same types. Just like C.)
    for (auto &sym : scope) {
        if (strcmp(sym.name, decl->identifier->name) == 0) {
            if(decl->is_function_body) {
                report_error(decl->line_number, decl->character_number, "Function '%s' already declared in this scope", decl->identifier->name);
                return false;
            }
        }
    }

    CM_Symbol sym;
    sym.name = pool_strdup(ast_pool, decl->identifier->name);
    sym.decl = decl;
    // sym.type = decl->return_type ? decl->return_type : make_builtin_type(TYPE_VOID);
    sym.type = decl->return_type ? decl->return_type : _type->type_def_void;

    sym.is_function = true;
    sym.is_function_body = decl->is_function_body;
    sym.parameters = decl->parameters;
    sym.initialized = decl->is_function_body; // idk if its a good idea to tag function bodies as initialized

    scope.push_back(sym);
    return true;
}

CM_Symbol* CodeManager::lookup_symbol(const char *name)
{
    for (int i = (int)scopes.size() - 1; i >= 0; --i) {
        for (auto &sym : scopes[i]) {
            if (strcmp(sym.name, name) == 0) return &sym;
        }
    }
    return nullptr;
}

CM_Symbol* CodeManager::lookup_symbol_current_scope(const char *name)
{
    if (scopes.empty()) return nullptr;
    CM_Scope& current_scope = scopes.back();
    for (auto &sym : current_scope) {
        if (strcmp(sym.name, name) == 0) return &sym;
    }
    return nullptr;
}

void CodeManager::mark_initialized(const char *name)
{
    CM_Symbol* s = lookup_symbol(name);
    if (s) s->initialized = true;
}

// bool CodeManager::has_return_statement(Ast_Block* block) {
//     if (!block) return false;

//     for (int i = 0; i < block->statements.count; ++i) {
//         Ast_Statement* stmt = block->statements.data[i];
//         if (!stmt) continue;

//         if (stmt->is_return) {
//             return true;
//         } else if (stmt->type == AST_IF) {
//             Ast_If* ifn = static_cast<Ast_If*>(stmt);
//             if (ifn->then_block && has_return_statement(ifn->then_block)) {
//                 return true;
//             }
//             if (ifn->else_block && has_return_statement(ifn->else_block)) {
//                 return true;
//             }
//         } else if (stmt->block && stmt->block->is_scoped_block) {
//             if (has_return_statement(stmt->block)) {
//                 return true;
//             }
//         }
//     }
//     return false;
// }
ReturnCheckResult CodeManager::checkReturnPaths(Ast_Block* block) {
    ReturnCheckResult result = {false, false};  // Default: no returns, some path falls through
    if (!block) return result;

    bool fallthrough = true;  // Tracks if any path reaches the end without returning

    for (size_t i = 0; i < block->statements.count; ++i) {
        Ast_Statement* stmt = block->statements.data[i];
        if (!stmt) continue;

        if (!fallthrough) {
            // Unreachable code after a guaranteed return; skip
            continue;
        }

        if (stmt->is_return) {
            result.has_return = true;
            fallthrough = false;
            break;  // All paths from this return are covered
        } else if (stmt->type == AST_IF) {
            Ast_If* ifn = static_cast<Ast_If*>(stmt);
            ReturnCheckResult badResult = {false, false};
            ReturnCheckResult then_result = ifn->then_block ? checkReturnPaths(ifn->then_block) : badResult;
            ReturnCheckResult else_result = ifn->else_block ? checkReturnPaths(ifn->else_block) : badResult;

            // Update has_return: true if either branch has a return
            result.has_return |= then_result.has_return || else_result.has_return;

            // All paths return only if both branches return (if else exists)
            if (then_result.all_paths_return && else_result.all_paths_return) {
                fallthrough = false;
                break;
            }
        } else if (stmt->block && stmt->block->is_scoped_block) {
            // Recurse into scoped blocks (e.g., while, for, compound)
            ReturnCheckResult block_result = checkReturnPaths(stmt->block);
            result.has_return |= block_result.has_return;
            if (block_result.all_paths_return) {
                fallthrough = false;
                break;
            }
        }
        // Other statements (e.g., assignments) allow fallthrough
    }

    result.all_paths_return = !fallthrough;
    return result;
}

// Semantic analysis check for function return statements
void CodeManager::checkFunctionReturns(Ast_Declaration* decl) {
    if (decl->return_type == _type->type_def_void /*->builtin_type == TYPE_VOID*/) {
        return;
    }

    ReturnCheckResult result = checkReturnPaths(decl->my_scope);

    // Check for complete absence of return statements
    if (!result.has_return) {
        report_error(decl->line_number, decl->character_number,
                     "Non-void function '%s' must have a return statement",
                     decl->identifier->name);
        return;  // No need to check all-paths if no returns exist
    }

    // Check if all paths return a value
    if (!result.all_paths_return) {
        report_error(decl->line_number, decl->character_number,
                     "Not all paths return a value in non-void function '%s'",
                     decl->identifier->name);
    }
}

void CodeManager::resolve_idents(Ast_Block* block) {
    if (!block) return;

    if (scopes.size() == 1) {
        for (int i = 0; i < block->statements.count; i++) {
            Ast_Statement* stmt = block->statements.data[i];
            if (!stmt) continue;

            if (stmt->type != AST_DECLARATION) {
                if(stmt->block && stmt->block->is_entry_point == false && !stmt->block->is_scoped_block) {
                    report_error(stmt->line_number, stmt->character_number,
                             "Non-declaration statements are not allowed in global scope");

                }
                continue;
            }

            Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
            if (decl->is_function) {
                declare_function(decl);

                if (decl->is_function_body) {
                    if (!decl->return_type) {
                        report_error(decl->line_number, decl->character_number,
                                     "Function '%s' must specify a return type", decl->identifier->name);
                    } else if (decl->return_type != _type->type_def_void /*->builtin_type != TYPE_VOID*/ && decl->my_scope) {
                        // if (!has_return_statement(decl->my_scope)) {
                        //     report_error(decl->line_number, decl->character_number,
                        //                  "Non-void function '%s' must contain a return statement",
                        //                  decl->identifier->name);
                        // }
                        checkFunctionReturns(decl);
                    }
                }
            } else {
                declare_variable(decl);
            }
        }

        // Second pass: Resolve initializers, function bodies, and expressions
        for (int i = 0; i < block->statements.count; i++) {
            Ast_Statement* stmt = block->statements.data[i];
            if (!stmt) continue;

            if (stmt->is_return) {
                resolve_idents_in_expr(stmt->expression);
                continue;
            }

            if (stmt->type == AST_DECLARATION) {
                Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
                resolve_idents_in_declaration(decl);
                if (decl->is_function && decl->my_scope && decl->is_function_body) {
                    push_scope();
                    for (int j = 0; j < decl->parameters.count; ++j) {
                        declare_variable(decl->parameters.data[j]);
                    }
                    resolve_idents(decl->my_scope);
                    pop_scope();
                }
            } else if (stmt->type == AST_IF) {
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
                if (stmt->block->is_scoped_block)
                    push_scope();
                resolve_idents(stmt->block);
                if (stmt->block->is_scoped_block)
                    pop_scope();
            }
        }
    } else {
        // Non-global scope: Single pass
        for (int i = 0; i < block->statements.count; i++) {
            Ast_Statement* stmt = block->statements.data[i];
            if (!stmt) continue;

            if (stmt->is_return) {
                // Handle return statements
                // if (stmt->expression) {
                    resolve_idents_in_expr(stmt->expression);
                // }
                continue;
            }

            if (stmt->type == AST_DECLARATION) {
                Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
                if (decl->is_function) {
                    // resolve_idents_in_declaration(decl);
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
                    resolve_idents_in_declaration(decl);
                    declare_variable(decl);
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
                if (stmt->block->is_scoped_block)
                    push_scope();
                resolve_idents(stmt->block);
                if (stmt->block->is_scoped_block)
                    pop_scope();
            }
        }
    }
}

void CodeManager::resolve_idents_in_declaration(Ast_Declaration* decl)
{
    if (!decl) return;
    if (decl->initializer) {
        resolve_idents_in_expr(decl->initializer);
    }
}

void CodeManager::resolve_idents_in_expr(Ast_Expression* expr)
{
    if (!expr) return;

    switch (expr->type) {
        case AST_IDENT: {
            Ast_Ident* id = static_cast<Ast_Ident*>(expr);
            CM_Symbol* s = lookup_symbol(id->name);
            if (!s) {
                report_error(id->line_number, id->character_number,
                    "Use of undeclared identifier '%s'", id->name);
            }
            break;
        }

        case AST_LITERAL:
            break;

        case AST_UNARY: {
            Ast_Unary* u = static_cast<Ast_Unary*>(expr);
            if (!u->operand) break;

            resolve_idents_in_expr(u->operand);

            //  check if operand is a pointer
            if (u->op == UNARY_DEREFERENCE)
            {
                Ast_Type_Definition* opType = u->operand->inferred_type;
                if (opType && !opType->pointed_to_type) {
                    report_error(u->line_number, u->character_number, "Cannot dereference non-pointer type");
                }
            }

            break;
        }


        case AST_BINARY: {
            Ast_Binary* b = static_cast<Ast_Binary*>(expr);

            if (b->op == BINOP_ASSIGN)
            {
                // should resolve rhs first
                if (b->rhs) {
                    resolve_idents_in_expr(b->rhs);
                }
                infer_types_expr(&b->rhs);
                Ast_Type_Definition* rhsType = b->rhs->inferred_type;
                Ast_Type_Definition* lhsType = nullptr;

                if (Ast_Ident* lhs_ident = ast_static_cast<Ast_Ident>(b->lhs, AST_IDENT))
                {
                    CM_Symbol* sym = lookup_symbol(lhs_ident->name);
                    if (!sym) {
                        report_error(lhs_ident->line_number, lhs_ident->character_number,
                            "Assignment to undeclared variable '%s'", lhs_ident->name);
                    } else {
                        sym->initialized = true;

                        if (!sym->type) {
                            sym->type = rhsType;
                        } else if (!check_that_types_match(sym->type, rhsType)) {
                            report_error(
                                lhs_ident->line_number,
                                lhs_ident->character_number,
                                "Type mismatch in assignment to '%s'",
                                lhs_ident->name);
                        }

                        lhsType = sym->type;
                    }
                } else if (Ast_Unary* lhs_unary = ast_static_cast<Ast_Unary>(b->lhs, AST_UNARY)) {
                    if (lhs_unary->op == UNARY_DEREFERENCE) {

                        // must be a pointer expression
                        resolve_idents_in_expr(lhs_unary->operand);
                        infer_types_expr(&lhs_unary->operand);
                        Ast_Type_Definition* pointerType = lhs_unary->operand->inferred_type;
                        if (!pointerType || !pointerType->pointed_to_type)
                        {
                            report_error( lhs_unary->line_number, lhs_unary->character_number,
                                "Invalid dereference: LHS is not a pointer");
                        } else {
                            lhsType = pointerType->pointed_to_type;

                            if (!check_that_types_match(lhsType, rhsType)) {
                                report_error(
                                    lhs_unary->line_number,
                                    lhs_unary->character_number,
                                    "Type mismatch: cannot assign value to dereferenced pointer"
                                );
                            }
                        }
                    } else {
                        report_error(
                            lhs_unary->line_number,
                            lhs_unary->character_number,
                            "Unsupported unary operation on LHS of assignment"
                        );
                    }
                } else {
                    report_error(
                        b->line_number,
                        b->character_number,
                        "Left-hand side of assignment must be an identifier or dereferenced pointer"
                    );
                }
            }
            else {
                // rest binops just resolve normally
                if (b->lhs) resolve_idents_in_expr(b->lhs);
                if (b->rhs) resolve_idents_in_expr(b->rhs);
            }
            break;
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression* call =
                static_cast<Ast_Procedure_Call_Expression*>(expr);

            if (call->function)
            {
                Ast_Ident* fn = static_cast<Ast_Ident*>(call->function);

                if (fn->name && strcmp(fn->name, "printf") != 0) // allow builtins
                {

                    CM_Symbol* sym = lookup_symbol(fn->name);
                    if (!sym) {
                        // report_error(fn->line_number, fn->character_number,
                        //              "Call to undeclared function '%s'", fn->name);

                        CM_Unresolved_Call unresolved;
                        unresolved.call = call;
                        unresolved.line_number = fn->line_number;
                        unresolved.character_number = fn->character_number;
                        unresolved_calls.push_back(unresolved);
                    } else if (!sym->is_function) {
                        report_error(fn->line_number, fn->character_number,
                                     "'%s' is not a function", fn->name);
                    } else {
                        // Check parameter count
                        int call_arg_count = call->arguments ? call->arguments->arguments.count : 0;
                        int decl_arg_count = sym->parameters.count;
                        if (call_arg_count != decl_arg_count) {
                            report_error(fn->line_number, fn->character_number,
                                         "Function '%s' expects %d arguments, but %d were provided",
                                         fn->name, decl_arg_count, call_arg_count);
                        } else if (call->arguments) {

                            for (int i = 0; i < call_arg_count; ++i) {
                                Ast_Expression* arg = call->arguments->arguments.data[i];
                                resolve_idents_in_expr(arg);
                                infer_types_expr(&arg);
                                Ast_Type_Definition* arg_type = arg->inferred_type;
                                Ast_Type_Definition* param_type = sym->parameters.data[i]->declared_type;
                                if (!check_that_types_match(param_type, arg_type)) {
                                    report_error(fn->line_number, fn->character_number,
                                                 "Type mismatch for argument %d in call to '%s'. Expected '%s', Got '%s'",
                                                 i + 1, fn->name,type_to_string(param_type), type_to_string(arg_type));
                                }
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
            Ast_Comma_Separated_Args* args = static_cast<Ast_Comma_Separated_Args*>(expr);

            for (int i = 0; i < args->arguments.count; ++i) {
                Ast_Expression *a = args->arguments.data[i];
                resolve_idents_in_expr(a);
            }
            break;
        }

        default: break;
    }
}

void CodeManager::resolve_unresolved_calls() {
    std::vector<CM_Unresolved_Call> still_unresolved;

    for (const auto& unresolved : unresolved_calls) {
        Ast_Procedure_Call_Expression* call = unresolved.call;
        Ast_Ident* fn = static_cast<Ast_Ident*>(call->function);
        CM_Symbol* sym = lookup_symbol(fn->name);

        if (!sym) {
            still_unresolved.push_back(unresolved);
            continue;
        }

        if (!sym->is_function) {
            report_error(unresolved.line_number, unresolved.character_number,
                         "'%s' is not a function", fn->name);
            continue;
        }

        if (call->arguments) {
            for (int i = 0; i < call->arguments->arguments.count; ++i) {
                Ast_Expression* arg = call->arguments->arguments.data[i];
                resolve_idents_in_expr(arg);
            }
        }

        int call_arg_count = call->arguments ? call->arguments->arguments.count : 0;
        int decl_arg_count = sym->parameters.count;
        if (call_arg_count != decl_arg_count) {
            report_error(unresolved.line_number, unresolved.character_number,
                         "Function '%s' expects %d arguments, but %d were provided",
                         fn->name, decl_arg_count, call_arg_count);
            continue;
        }

        if (call->arguments) {
            for (int i = 0; i < call_arg_count; ++i) {
                Ast_Expression* arg = call->arguments->arguments.data[i];
                infer_types_expr(&arg);
                Ast_Type_Definition* arg_type = arg->inferred_type;
                Ast_Type_Definition* param_type = sym->parameters.data[i]->declared_type;
                if (!check_that_types_match(param_type, arg_type)) {
                    report_error(unresolved.line_number, unresolved.character_number,
                                 "Type mismatch for argument %d in call to '%s'",
                                 i + 1, fn->name);
                }
            }
        }
    }

    unresolved_calls = still_unresolved;

    if (scopes.size() == 1 && !unresolved_calls.empty()) {
        for (const auto& unresolved : unresolved_calls) {
            Ast_Ident* fn = static_cast<Ast_Ident*>(unresolved.call->function);
            report_error(unresolved.line_number, unresolved.character_number,
                         "Call to undeclared function '%s'", fn->name);
        }
        unresolved_calls.clear();
    }
}

char* CodeManager::type_to_string(Ast_Type_Definition* type) {

// part of code pasted from c_converter

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
        type_str = "^";
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


// Ast_Type_Definition  *CodeManager::make_builtin_type(Ast_Builtin_Type t) {
//     Ast_Type_Definition *out = AST_NEW(ast_pool, Ast_Type_Definition);
//     out->builtin_type = t;
//     return out;
// }


// Helper function to infer types for return statements and check against function return type
void CodeManager::infer_types_return(Ast_Statement* ret, Ast_Declaration* func_decl) {
    if (!ret || !func_decl) return;

    // Ast_Type_Definition* func_return_type = func_decl->return_type ? func_decl->return_type : make_builtin_type(TYPE_VOID);
    Ast_Type_Definition* func_return_type = func_decl->return_type ? func_decl->return_type : _type->type_def_void;

    if (!ret->expression && func_return_type != _type->type_def_void) {
        report_error(ret->line_number, ret->character_number,
                     "Return statement in function '%s' must return a value of type %s",
                     func_decl->identifier->name,
                     func_return_type == _type->type_def_int ? "int" :
                     func_return_type == _type->type_def_float ? "float" :
                     func_return_type == _type->type_def_bool ? "bool" : "unknown");
        return;
    }

    if (ret->expression && func_return_type == _type->type_def_void) {
        report_error(ret->line_number, ret->character_number,
                     "Void function '%s' cannot return a value",
                     func_decl->identifier->name);
        return;
    }

    if (ret->expression) {
        infer_types_expr(&ret->expression);
        Ast_Type_Definition* return_expr_type = ret->expression->inferred_type;
        if (!return_expr_type) {
            report_error(ret->line_number, ret->character_number,
                         "Could not infer type of return expression in function '%s'",
                         func_decl->identifier->name);
            return;
        }

        if (!check_that_types_match(func_return_type, return_expr_type)) {
            report_error(ret->line_number, ret->character_number,
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
    if (!expr_ptr || !*expr_ptr) return;
    Ast_Expression *expr = *expr_ptr;

    switch (expr->type) {
        case AST_LITERAL: {
            Ast_Literal *lit = static_cast<Ast_Literal *>(expr);
            switch(lit->value_type){
                case LITERAL_NUMBER: {
                    expr->inferred_type = _type->type_def_s64;
                    break;
                    // return make_builtin_type(TYPE_INT);
                }
                case LITERAL_FLOAT: {
                    expr->inferred_type = _type->type_def_float;
                    break;
                    // return make_builtin_type(TYPE_FLOAT);
                }
                case LITERAL_STRING: {
                    expr->inferred_type = _type->type_def_string;
                    break;
                    // return make_builtin_type(TYPE_STRING);
                }
                case LITERAL_TRUE:
                case LITERAL_FALSE:{
                    expr->inferred_type = _type->type_def_bool;
                    break;
                    // return make_builtin_type(TYPE_BOOL);
                }

                default: {
                    expr->inferred_type = _type->type_def_dummy;
                    break;
                    // return nullptr;
                }
            }
            break;
        }

        case AST_IDENT: {
            Ast_Ident *id = static_cast<Ast_Ident *>(expr);
            CM_Symbol *s = lookup_symbol(id->name); /* = lookup_symbol_current_scope(id->name);*/
            // if(!s) s = lookup_symbol(id->name);

            if (s && s->decl)
            {
                if (s->is_function) {
                    // return s->type;
                    expr->inferred_type = s->type;
                }else if (s->decl->declared_type) {
                    // return s->decl->declared_type;
                    expr->inferred_type = s->decl->declared_type;
                } else if (s->initialized && !s->inferred) {
                    // return infer_types_expr(&s->decl->initializer);
                    infer_types_expr(&s->decl->initializer);
                    expr->inferred_type = s->decl->initializer->inferred_type;
                } else {
                    expr->inferred_type = _type->type_def_dummy;

                }
            } else {
                report_error(id->line_number, id->character_number, "Use of undeclared identifier '%s'", id->name);
                expr->inferred_type = _type->type_def_dummy;
            }
            break;
            // return nullptr;
        }

        case AST_UNARY: {
            Ast_Unary *u = static_cast<Ast_Unary *>(expr);
            if (!u->operand) {
                expr->inferred_type = _type->type_def_dummy;
                return;
                // return nullptr;
            }

            infer_types_expr(&u->operand);
            Ast_Type_Definition *operandType = u->operand->inferred_type;
            if (!operandType) {
                report_error(u->line_number, u->character_number,
                    "Could not determine type of operand for unary expression");
                expr->inferred_type = _type->type_def_dummy;

                // return nullptr;
            }

            Ast_Type_Definition* resultType = AST_NEW(ast_pool, Ast_Type_Definition);
            switch (u->op) {
            case UNARY_DEREFERENCE: {
                if (!operandType->pointed_to_type) {
                    report_error(u->line_number, u->character_number,
                        "Cannot dereference non-pointer type");
                    expr->inferred_type = _type->type_def_dummy;
                    return;
                    // return nullptr;
                }
                expr->inferred_type = operandType->pointed_to_type;
                // resultType = operandType->pointed_to_type;
                // expr->inferred_type = resultType;
                // return resultType;
                break;
            }
            case UNARY_ADDRESS_OF:
                resultType = AST_NEW(ast_pool, Ast_Type_Definition);
                resultType->pointed_to_type = operandType;
                expr->inferred_type = resultType;
                // resultType->builtin_type = TYPE_UNKNOWN;
                break;
            case UNARY_NEGATE:
            case UNARY_NOT:
                // type same as operand
                // resultType = operandType;
                expr->inferred_type = operandType;
                break;

            default:
                report_error(u->line_number, u->character_number, "Unknown unary operator");
                expr->inferred_type = _type->type_def_dummy;

                // return nullptr;
            }

            // expr->inferred_type = resultType;
            // return resultType;
            break;
        }

        case AST_BINARY: {

            Ast_Binary *b = static_cast<Ast_Binary *>(expr);
            infer_types_expr(&b->lhs);
            infer_types_expr(&b->rhs);

            Ast_Type_Definition *lt = b->lhs->inferred_type;
            Ast_Type_Definition *rt = b->rhs->inferred_type;

            if(lt && lt == _type->type_def_dummy || rt == _type->type_def_dummy){
                expr->inferred_type = _type->type_def_dummy;
                break;
            }


            switch (b->op) {
                case BINOP_ADD:
                case BINOP_SUB:
                case BINOP_MUL:
                case BINOP_DIV: {

                    // prefer float
                    // if ((lt && lt->builtin_type == TYPE_FLOAT) || (rt && rt->builtin_type == TYPE_FLOAT)) {
                    //     return make_builtin_type(TYPE_FLOAT);
                    // }

                    // if (lt && rt && lt->builtin_type == TYPE_INT && rt->builtin_type == TYPE_INT) {
                    //     return make_builtin_type(TYPE_INT);
                    // }

                    if (lt == _type->type_def_float || rt == _type->type_def_float) {
                        expr->inferred_type = _type->type_def_float;
                    } else if (lt == _type->type_def_int && rt == _type->type_def_int) {
                        expr->inferred_type = _type->type_def_int;
                    } else if (lt == _type->type_def_s64 && rt == _type->type_def_s64) {
                        expr->inferred_type = _type->type_def_s64;
                    } else if (lt == _type->type_def_int && rt == _type->type_def_s64) {
                        expr->inferred_type = _type->type_def_s64;
                    }else {
                        // type mismatch
                        report_error(b->line_number, b->character_number, "Type error in binary arithmetic: operand types incompatible");
                        expr->inferred_type = _type->type_def_dummy;
                    }

                    break;
                    // type mismatch
                    // report_error(b->line_number, b->character_number, "Type error in binary arithmetic: operand types incompatible");
                    // return nullptr;
                }

                case BINOP_EQ:
                case BINOP_NEQ: {
                    // return make_builtin_type(TYPE_BOOL)
                    expr->inferred_type = _type->type_def_bool;
                    break;
                }
                case BINOP_ASSIGN: {
                    infer_types_expr(&b->rhs);
                    Ast_Type_Definition *rhsType = b->rhs->inferred_type;
                    if (rhsType == _type->type_def_dummy) {
                        report_error(b->line_number, b->character_number,
                                     "Right-hand side of assignment has unknown type");
                        expr->inferred_type = _type->type_def_dummy;
                        return;
                        // return nullptr;

                    }

                    Ast_Type_Definition *lhsType = nullptr;

                    if (Ast_Ident *lhs_ident = ast_static_cast<Ast_Ident>(b->lhs, AST_IDENT))
                    {
                        CM_Symbol *sym = lookup_symbol(lhs_ident->name);
                        if (!sym)
                        {
                            report_error(lhs_ident->line_number, lhs_ident->character_number,
                                "Assignment to undeclared variable '%s'",
                                lhs_ident->name);
                            expr->inferred_type = _type->type_def_dummy;
                            return;
                            // return nullptr;
                        }

                        if (!sym->type) {
                            sym->type = rhsType;
                        }
                        else if (!check_that_types_match(sym->type, rhsType))
                        {
                            report_error(lhs_ident->line_number, lhs_ident->character_number,
                                "Type mismatch in assignment to '%s'. Expected '%s' Got '%s'",
                                lhs_ident->name, type_to_string(sym->type), type_to_string(rhsType));
                        }

                        sym->initialized = true;
                        lhsType = sym->type;
                    }

                    // lhs is a dereference
                    else if (Ast_Unary *lhs_unary = ast_static_cast<Ast_Unary>(b->lhs, AST_UNARY))
                    {
                        if (lhs_unary->op == UNARY_DEREFERENCE)
                        {
                            infer_types_expr(&lhs_unary->operand);
                            Ast_Type_Definition *pointerType = lhs_unary->operand->inferred_type;

                            if (!pointerType || !pointerType->pointed_to_type)
                            {
                                report_error(lhs_unary->line_number, lhs_unary->character_number,
                                             "Invalid dereference: LHS is not a pointer");

                                expr->inferred_type = _type->type_def_dummy;
                                return;
                                // return nullptr;
                            }

                            if (Ast_Ident *pointer_ident = ast_static_cast<Ast_Ident>(lhs_unary->operand, AST_IDENT))
                            {
                                CM_Symbol *pointer_sym = lookup_symbol(pointer_ident->name);

                                bool is_var_param = is_function_parameter(pointer_ident->name); // HACK think of a better way
                                if (pointer_sym && !pointer_sym->initialized && !is_var_param) {
                                    report_error(lhs_unary->line_number, lhs_unary->character_number,
                                                 "Cannot dereference uninitialized pointer '%s'",
                                                 pointer_ident->name ? pointer_ident->name : "");

                                    expr->inferred_type = _type->type_def_dummy;
                                    return;
                                    // return nullptr;
                                }
                            }

                            lhsType = pointerType->pointed_to_type;

                            if (!check_that_types_match(lhsType, rhsType)) {
                                report_error(lhs_unary->line_number, lhs_unary->character_number,
                                             "Type mismatch: cannot assign value to dereferenced pointer");
                                return;
                            }

                        } else {
                            report_error(lhs_unary->line_number, lhs_unary->character_number,
                                         "Unsupported unary operation on LHS of assignment");


                            expr->inferred_type = _type->type_def_dummy;
                            return;
                            // return nullptr;
                        }
                    } else {
                        report_error(b->line_number, b->character_number,
                                     "Left-hand side of assignment must be an identifier or a dereferenced pointer");


                        expr->inferred_type = _type->type_def_dummy;
                        return;
                        // return nullptr;
                    }

                    expr->inferred_type = lhsType;
                    break;
                    // return lhsType;
                }

                default:
                    report_error(b->line_number, b->character_number, "Unknown binary operator in type inference");

                    expr->inferred_type = _type->type_def_dummy;
                    break;
                    // return nullptr;
            }
            break;
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression *call = static_cast<Ast_Procedure_Call_Expression *>(expr);

            Ast_Type_Definition *return_type = nullptr;

            if (call->function) {
                Ast_Ident *fn = static_cast<Ast_Ident *>(call->function);
                if (fn->name && strcmp(fn->name, "printf") != 0) { // Allow builtins
                    CM_Symbol *sym = lookup_symbol(fn->name);
                    if (sym && sym->is_function) {
                        return_type = sym->type; // Function's return type
                        // Ensure return type matches declared type
                        if (sym->decl && sym->decl->return_type && !check_that_types_match(sym->type, sym->decl->return_type)) {
                            report_error(fn->line_number, fn->character_number,
                                         "Function '%s' return type mismatch", fn->name);
                        }
                    } else {
                        return_type = _type->type_def_void;
                        // return_type = make_builtin_type(TYPE_VOID); // Default for undefined functions
                    }
                } else {
                    return_type = _type->type_def_void;
                    // return_type = make_builtin_type(TYPE_VOID); // Builtins like printf
                }
            }

            // Infer types for arguments
            if (call->arguments)
            {
                for (int i = 0; i < call->arguments->arguments.count; ++i)
                {
                    Ast_Expression* p = call->arguments->arguments.data[i];
                    infer_types_expr(&p);
                }
            }
            // // builtins like printf
            // return make_builtin_type(TYPE_VOID);

            // If this call is part of an assignment, check the LHS type
            expr->inferred_type = return_type;
            if (expr->inferred_type == _type->type_def_dummy) {
                expr->inferred_type = return_type;
            } else if (!check_that_types_match(expr->inferred_type, return_type)) {
                report_error(expr->line_number, expr->character_number,
                             "Type mismatch: function call return type does not match expected type. Expected '%s' Got '%s'",
                                type_to_string(expr->inferred_type), type_to_string(return_type));
            }

            break;
            // return return_type;
        }

        case AST_COMMA_SEPARATED_ARGS:
            expr->inferred_type = _type->type_def_dummy;
            break;
            // return nullptr;

        default:
            expr->inferred_type = _type->type_def_dummy;
            break;
            // return nullptr;
    }
}
// Ast_Type_Definition *CodeManager::infer_types_expr(Ast_Expression** expr_ptr)
// {
//     if (!expr_ptr || !*expr_ptr) return nullptr;
//     Ast_Expression *expr = *expr_ptr;

//     switch (expr->type) {
//         case AST_LITERAL: {
//             Ast_Literal *lit = static_cast<Ast_Literal *>(expr);
//             switch(lit->value_type){
//                 case LITERAL_NUMBER: {
//                     return make_builtin_type(TYPE_INT);
//                 }
//                 case LITERAL_FLOAT: return make_builtin_type(TYPE_FLOAT);
//                 case LITERAL_STRING: return make_builtin_type(TYPE_STRING);
//                 case LITERAL_TRUE:
//                 case LITERAL_FALSE: return make_builtin_type(TYPE_BOOL);
//                 default: return nullptr;
//             }
//             break;
//         }

//         case AST_IDENT: {
//             Ast_Ident *id = static_cast<Ast_Ident *>(expr);
//             CM_Symbol *s = lookup_symbol(id->name); /* = lookup_symbol_current_scope(id->name);*/
//             // if(!s) s = lookup_symbol(id->name);

//             if (s && s->decl)
//             {
//                 if (s->is_function) {
//                     return s->type;
//                 }else if (s->decl->declared_type) {
//                     return s->decl->declared_type;
//                 } else if (s->initialized && !s->inferred) {
//                     return infer_types_expr(&s->decl->initializer);
//                 }
//             }
//             return nullptr;
//         }

//         case AST_UNARY: {
//             Ast_Unary *u = static_cast<Ast_Unary *>(expr);
//             if (!u->operand) return nullptr;

//             Ast_Type_Definition *operandType = infer_types_expr(&u->operand);
//             if (!operandType) {
//                 report_error(u->line_number, u->character_number,
//                     "Could not determine type of operand for unary expression");
//                 return nullptr;
//             }

//             Ast_Type_Definition* resultType = AST_NEW(ast_pool, Ast_Type_Definition);
//             switch (u->op) {
//             case UNARY_DEREFERENCE: {
//                 if (!operandType->pointed_to_type) {
//                     report_error(u->line_number, u->character_number,
//                         "Cannot dereference non-pointer type");
//                     return nullptr;
//                 }
//                 resultType = operandType->pointed_to_type;
//                 // expr->inferred_type = resultType;
//                 // return resultType;
//                 break;
//             }
//             case UNARY_ADDRESS_OF:
//                 resultType->builtin_type = TYPE_UNKNOWN;
//                 resultType->pointed_to_type = operandType;
//                 break;
//             case UNARY_NEGATE:
//             case UNARY_NOT:
//                 // type same as operand
//                 resultType = operandType;
//                 break;

//             default:
//                 report_error(u->line_number, u->character_number, "Unknown unary operator");
//                 return nullptr;
//             }

//             expr->inferred_type = resultType;
//             return resultType;
//         }

//         case AST_BINARY: {

//             Ast_Binary *b = static_cast<Ast_Binary *>(expr);
//             Ast_Type_Definition *lt = infer_types_expr(&b->lhs);
//             Ast_Type_Definition *rt = infer_types_expr(&b->rhs);

//             switch (b->op) {
//                 case BINOP_ADD:
//                 case BINOP_SUB:
//                 case BINOP_MUL:
//                 case BINOP_DIV: {

//                     // prefer float
//                     if ((lt && lt->builtin_type == TYPE_FLOAT) || (rt && rt->builtin_type == TYPE_FLOAT)) {
//                         return make_builtin_type(TYPE_FLOAT);
//                     }

//                     if (lt && rt && lt->builtin_type == TYPE_INT && rt->builtin_type == TYPE_INT) {
//                         return make_builtin_type(TYPE_INT);
//                     }
//                     // type mismatch
//                     report_error(b->line_number, b->character_number, "Type error in binary arithmetic: operand types incompatible");
//                     return nullptr;
//                 }

//                 case BINOP_EQ:
//                 case BINOP_NEQ: {
//                     return make_builtin_type(TYPE_BOOL);
//                 }
//                 case BINOP_ASSIGN: {
//                     Ast_Type_Definition *rhsType = infer_types_expr(&b->rhs);
//                     if (!rhsType) {
//                         report_error(b->line_number, b->character_number,
//                                      "Right-hand side of assignment has unknown type");
//                         return nullptr;
//                     }

//                     Ast_Type_Definition *lhsType = nullptr;

//                     if (Ast_Ident *lhs_ident = ast_static_cast<Ast_Ident>(b->lhs, AST_IDENT))
//                     {
//                         CM_Symbol *sym = lookup_symbol(lhs_ident->name);
//                         if (!sym)
//                         {
//                             report_error(lhs_ident->line_number, lhs_ident->character_number,
//                                 "Assignment to undeclared variable '%s'",
//                                 lhs_ident->name);
//                             return nullptr;
//                         }

//                         if (!sym->type) {
//                             sym->type = rhsType;
//                         }
//                         else if (!check_that_types_match(sym->type, rhsType))
//                         {
//                             report_error(lhs_ident->line_number, lhs_ident->character_number,
//                                 "Type mismatch in assignment to '%s'. Expected '%s' Got '%s'",
//                                 lhs_ident->name, type_to_string(sym->type), type_to_string(rhsType));
//                         }

//                         sym->initialized = true;
//                         lhsType = sym->type;
//                     }

//                     // lhs is a dereference
//                     else if (Ast_Unary *lhs_unary = ast_static_cast<Ast_Unary>(b->lhs, AST_UNARY))
//                     {
//                         if (lhs_unary->op == UNARY_DEREFERENCE)
//                         {
//                             Ast_Type_Definition *pointerType = infer_types_expr(&lhs_unary->operand);

//                             if (!pointerType || !pointerType->pointed_to_type)
//                             {
//                                 report_error(lhs_unary->line_number, lhs_unary->character_number,
//                                              "Invalid dereference: LHS is not a pointer");
//                                 return nullptr;
//                             }

//                             if (Ast_Ident *pointer_ident = ast_static_cast<Ast_Ident>(lhs_unary->operand, AST_IDENT))
//                             {
//                                 CM_Symbol *pointer_sym = lookup_symbol(pointer_ident->name);

//                                 bool is_var_param = is_function_parameter(pointer_ident->name); // HACK think of a better way
//                                 if (pointer_sym && !pointer_sym->initialized && !is_var_param) {
//                                     report_error(lhs_unary->line_number, lhs_unary->character_number,
//                                                  "Cannot dereference uninitialized pointer '%s'",
//                                                  pointer_ident->name ? pointer_ident->name : "");

//                                     return nullptr;
//                                 }
//                             }

//                             lhsType = pointerType->pointed_to_type;

//                             if (!check_that_types_match(lhsType, rhsType)) {
//                                 report_error(lhs_unary->line_number, lhs_unary->character_number,
//                                              "Type mismatch: cannot assign value to dereferenced pointer");
//                             }

//                         } else {
//                             report_error(lhs_unary->line_number, lhs_unary->character_number,
//                                          "Unsupported unary operation on LHS of assignment");
//                             return nullptr;
//                         }
//                     } else {
//                         report_error(b->line_number, b->character_number,
//                                      "Left-hand side of assignment must be an identifier or a dereferenced pointer");
//                         return nullptr;
//                     }

//                     return lhsType;
//                 }

//                 default:
//                     report_error(b->line_number, b->character_number, "Unknown binary operator in type inference");
//                     return nullptr;
//             }
//         }

//         case AST_PROCEDURE_CALL_EXPRESSION: {
//             Ast_Procedure_Call_Expression *call = static_cast<Ast_Procedure_Call_Expression *>(expr);

//             Ast_Type_Definition *return_type = nullptr;

//             if (call->function) {
//                 Ast_Ident *fn = static_cast<Ast_Ident *>(call->function);
//                 if (fn->name && strcmp(fn->name, "printf") != 0) { // Allow builtins
//                     CM_Symbol *sym = lookup_symbol(fn->name);
//                     if (sym && sym->is_function) {
//                         return_type = sym->type; // Function's return type
//                         // Ensure return type matches declared type
//                         if (sym->decl && sym->decl->return_type && !check_that_types_match(sym->type, sym->decl->return_type)) {
//                             report_error(fn->line_number, fn->character_number,
//                                          "Function '%s' return type mismatch", fn->name);
//                         }
//                     } else {
//                         return_type = make_builtin_type(TYPE_VOID); // Default for undefined functions
//                     }
//                 } else {
//                     return_type = make_builtin_type(TYPE_VOID); // Builtins like printf
//                 }
//             }

//             // Infer types for arguments
//             if (call->arguments)
//             {
//                 for (int i = 0; i < call->arguments->arguments.count; ++i)
//                 {
//                     Ast_Expression* p = call->arguments->arguments.data[i];
//                     infer_types_expr(&p);
//                 }
//             }
//             // // builtins like printf
//             // return make_builtin_type(TYPE_VOID);

//             // If this call is part of an assignment, check the LHS type
//             if (expr->inferred_type == nullptr) {
//                 expr->inferred_type = return_type;
//             } else if (!check_that_types_match(expr->inferred_type, return_type)) {
//                 report_error(expr->line_number, expr->character_number,
//                              "Type mismatch: function call return type does not match expected type");
//             }

//             return return_type;
//         }

//         case AST_COMMA_SEPARATED_ARGS:
//             return nullptr;

//         default:
//             return nullptr;
//     }
// }


void CodeManager::infer_types_decl(Ast_Declaration* decl) {
    if (!decl) return;

    if (decl->initializer) {
        Ast_Expression *init_expr = decl->initializer;
        infer_types_expr(&init_expr);
        Ast_Type_Definition *init_type = init_expr->inferred_type;
        if(!init_type) {
            report_error(decl->line_number, decl->character_number, "Could not infer type for variable '%s' from intitializer.",decl->identifier->name);
            return;
        }

        init_expr->inferred_type = init_type;

        if (decl->declared_type) {

                // explicitly typed
                if (!check_that_types_match(decl->declared_type, init_type)) {
                report_error(
                    decl->line_number,
                    decl->character_number,
                    "Type mismatch in initializer for '%s', Expected '%s' Got '%s'",
                    decl->identifier->name, type_to_string(decl->declared_type), type_to_string(init_type)
                );
            }

        } else {
            // not declared with type so infer it through initializer's type instead
            decl->declared_type = init_type;
            CM_Symbol *sym = lookup_symbol(decl->identifier->name);
            if(sym) sym->type = init_type;
        }
    }
}


void CodeManager::infer_types_block(Ast_Block* block, Ast_Declaration *my_func)
{
    // if (!block) return;
    assert(block);

    for (int i = 0; i < block->statements.count; i++) {

        Ast_Statement *stmt = block->statements.data[i];

        if (!stmt) continue;

        if (stmt->is_return) {
                assert(my_func);
                if (my_func) {
                    infer_types_return(stmt, my_func);
                } else {
                    report_error(stmt->line_number, stmt->character_number,
                                 "Return statement outside of function body");
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

                    CM_Symbol* s = lookup_symbol_current_scope(decl->identifier ? decl->identifier->name : "");
                    if (!s) {
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


// bool CodeManager::check_that_types_match(Ast_Type_Definition *wanted, Ast_Type_Definition* have, bool is_pointer)
// {
//     if (!wanted || !have) return false;

//     if (wanted->array_kind != ARRAY_NONE || have->array_kind != ARRAY_NONE)
//     {

//         if (wanted->array_kind != have->array_kind)
//             return false;

//         if (wanted->array_kind == ARRAY_STATIC &&
//             wanted->static_array_size != have->static_array_size)
//             return false;

//         if (!wanted->element_type || !have->element_type)
//             return false;

//         return check_that_types_match(wanted->element_type, have->element_type);
//     }

//     if (wanted->pointed_to_type || have->pointed_to_type) {

//         if (!wanted->pointed_to_type || !have->pointed_to_type)
//             return false;

//         return check_that_types_match(wanted->pointed_to_type, have->pointed_to_type, true);
//     }

//     if (wanted->builtin_type != TYPE_UNKNOWN && have->builtin_type != TYPE_UNKNOWN) {
//         if (wanted->builtin_type == have->builtin_type)
//             return true;

//         // since wanted != have, we can't allow a pointer type to have different builtin type
//         if(is_pointer == true) return false;

//         // we allow implicit int -> float, but not if its a pointer type
//         if (wanted->builtin_type == TYPE_FLOAT && have->builtin_type == TYPE_INT)
//         {
//             return true;

//         }
//         return false;
//     }

//     return false;
// }

bool CodeManager::check_that_types_match(Ast_Type_Definition *wanted, Ast_Type_Definition* have, bool is_pointer) {
    // Early null/unknown check
    if (!wanted || !have || wanted == _type->type_def_dummy || have == _type->type_def_dummy) return false;

    // Exact match: pointer equality (handles base builtins, pointers, arrays if children match)
    if (wanted == have) return true;

    // Handle arrays recursively
    if (wanted->array_kind != ARRAY_NONE || have->array_kind != ARRAY_NONE) {
        if (wanted->array_kind != have->array_kind ||
            (wanted->array_kind == ARRAY_STATIC && wanted->static_array_size != have->static_array_size) ||
            !wanted->element_type || !have->element_type) {
            return false;
        }
        return check_that_types_match(wanted->element_type, have->element_type);
    }

    // Handle pointers recursively (strict for pointers—no promotions on pointee)
    if (wanted->pointed_to_type || have->pointed_to_type) {
        if (!wanted->pointed_to_type || !have->pointed_to_type) return false;
        bool pointee_match = check_that_types_match(wanted->pointed_to_type, have->pointed_to_type, true);
        if (is_pointer && !pointee_match) return false;  // Strict for pointer types
        return pointee_match;
    }

    // Base case: No array/pointer—check promotions via explicit pointer matches
    // (No promotions if is_pointer; otherwise, allow implicit widening)
    if (is_pointer) return false;  // Already checked equality above

    if (wanted == _type->type_def_float && have == _type->type_def_s64) return true;
    if (wanted == _type->type_def_int && have == _type->type_def_s64) return true;
    if (wanted == _type->type_def_float && have == _type->type_def_int) return true;
    // Integer promotions: smaller signed/unsigned to larger
    if (wanted == _type->type_def_s16 && have == _type->type_def_s8) return true;
    if (wanted == _type->type_def_s32 && (have == _type->type_def_s8 || have == _type->type_def_s16)) return true;
    if (wanted == _type->type_def_s64 && (have == _type->type_def_s8 || have == _type->type_def_s16 || have == _type->type_def_s32)) return true;
    if (wanted == _type->type_def_u16 && have == _type->type_def_u8) return true;
    if (wanted == _type->type_def_u32 && (have == _type->type_def_u8 || have == _type->type_def_u16)) return true;
    if (wanted == _type->type_def_u64 && (have == _type->type_def_u8 || have == _type->type_def_u16 || have == _type->type_def_u32)) return true;

    // Signed/unsigned interop (e.g., s32 <- u32; warn in caller if needed)
    if (wanted == _type->type_def_s32 && have == _type->type_def_u32) return true;
    if (wanted == _type->type_def_u32 && have == _type->type_def_s32) return true;
    // Add s64/u64 if needed

    // Float promotions
    if (wanted == _type->type_def_float64 && have == _type->type_def_float32) return true;
    if (wanted == _type->type_def_float64 && have == _type->type_def_s32) return true;  // int -> float (legacy)
    if (wanted == _type->type_def_float64 && have == _type->type_def_s64) return true;  // Extend as needed
    // Add float32 -> s32? (Usually not, but if you want: if (wanted == _type->type_def_s32 && have == _type->type_def_float32) return true;)

    // Other (e.g., bool -> int; string equality only)
    if (wanted == _type->type_def_s32 && have == _type->type_def_bool) return true;  // bool as int

    // No match
    return false;
}