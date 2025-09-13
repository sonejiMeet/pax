#include "code_manager.h"

#include <cstdarg> // for variadic function

 // https://learn.microsoft.com/en-us/cpp/c-runtime-library/find-memory-leaks-using-the-crt-library?view=msvc-170#interpret-the-memory-leak-report
 #ifdef _DEBUG
     #define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )
     // Replace _NORMAL_BLOCK with _CLIENT_BLOCK if you want the
     // allocations to be of _CLIENT_BLOCK type
 #else
     #define DBG_NEW new
 #endif

#define AST_NEW(pool, type) ([&]() -> type* {                   \
    assert(pool != nullptr && "Pool must not be null");         \
    void* mem = pool_alloc(pool, sizeof(type));                \
    type* node = new (mem) type(pool);                         \
    return node;                                               \
}())


CodeManager::CodeManager(Pool* pool)
    : ast_pool(pool) {}

void CodeManager::init() {
    scopes.clear();
    scopes.emplace_back(); // global scope
}


int CodeManager::get_count_errors(){
    return count_errors;
}

void CodeManager::report_error(int line, int col, const char* fmt, ...)
{
    constexpr size_t BUFFER_SIZE = 512;
    char buffer[BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, args);
    va_end(args);

    count_errors += 1;
    if (line >= 0 && col >= 0) {
        fprintf(stderr, "Semantic error (%d:%d): %s\n", line, col, buffer);
    } else {
        fprintf(stderr, "Semantic error: %s\n", buffer);
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

bool CodeManager::declare_variable(Ast_Declaration* decl)
{
    CM_Scope &scope = scopes.back();

    for (auto &sym : scope) {
        if (sym.name == decl->identifier->name) {
            if (!decl->initializer)
                report_error(decl->line_number, decl->character_number, "Variable '%s' already declared", decl->identifier->name);
                return false;
        }
    }

    CM_Symbol sym;
    sym.name = decl->identifier->name;
    sym.decl = decl;
    sym.type = decl->declared_type;
    sym.initialized = (decl->initializer != nullptr);

    scope.push_back(sym);
    return true;
}


CM_Symbol* CodeManager::lookup_symbol(const std::string& name)
{
    for (int i = (int)scopes.size() - 1; i >= 0; --i) {
        for (auto &sym : scopes[i]) {
            if (sym.name == name) return &sym;
        }
    }
    return nullptr;
}

CM_Symbol* CodeManager::lookup_symbol_current_scope(const std::string& name)
{
    if (scopes.empty()) return nullptr;
    CM_Scope& current_scope = scopes.back();
    for (auto &sym : current_scope) {
        if (sym.name == name) return &sym;
    }
    return nullptr;
}

void CodeManager::mark_initialized(const std::string& name)
{
    CM_Symbol* s = lookup_symbol(name);
    if (s) s->initialized = true;
}

void CodeManager::resolve_idents(Ast_Block* block)
{
    if (!block) return;

    // for (auto* decl : block->members) {
    //     if (!decl) continue;

    //     resolve_idents_in_declaration(decl);
    //     declare_variable(decl);
    // }

    // for (auto* stmt : block->statements) {
    for (int i = 0; i < block->statements.count; i++) {
        Ast_Statement* stmt = block->statements.data[i];

        if (!stmt) continue;

        if (stmt->type == AST_DECLARATION) {
            Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
            // first resolve initializer expressions (they may refer to earlier variables)
            resolve_idents_in_declaration(decl);
            // then declare the variable which are in current scope
            declare_variable(decl);
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
            // continue;
        }

        // Generic statement may hold expression or nested block
        if (stmt->expression) {
            resolve_idents_in_expr(stmt->expression);
        }
        else if (stmt->block) {
            if(stmt->block->is_scoped_block)
                push_scope();

            resolve_idents(stmt->block);

            if(stmt->block->is_scoped_block)
                pop_scope();
        }
    }
    // for (auto* child_block : block->child_scopes) {
    //     // Each nested block gets its own new scope.
    //     push_scope();
    //     resolve_idents(child_block);
    //     pop_scope();
    // }
}

void CodeManager::resolve_idents_in_declaration(Ast_Declaration* decl) {
    if (!decl) return;
    if (decl->initializer) {
        resolve_idents_in_expr(decl->initializer);
    }
}

void CodeManager::resolve_idents_in_expr(Ast_Expression* expr) {
    if (!expr) return;

    switch (expr->type) {
        case AST_IDENT: {
            Ast_Ident* id = static_cast<Ast_Ident*>(expr);
            CM_Symbol* s = lookup_symbol(id->name);
            if (!s) {
                report_error(
                    id->line_number,
                    id->character_number,
                    "Use of undeclared identifier '%s'",
                    id->name
                );
            }
            break;
        }

        case AST_LITERAL:
            break;

        case AST_UNARY: {
            Ast_Unary* u = static_cast<Ast_Unary*>(expr);
            if (!u->operand) break;

            // First resolve identifiers in operand
            resolve_idents_in_expr(u->operand);

            // For dereference, we can optionally check operand is a pointer
            if (u->op == UNARY_DEREFERENCE) {
                Ast_Type_Definition* opType = u->operand->inferred_type;
                if (opType && !opType->pointed_to_type) {
                    report_error(u->line_number, u->character_number,
                        "Cannot dereference non-pointer type");
                }
            }
            // Don't set expr->inferred_type here; that's for type inference pass
            break;
        }


        case AST_BINARY: {
            Ast_Binary* b = static_cast<Ast_Binary*>(expr);

            if (b->op == BINOP_ASSIGN) {
                // --- Resolve RHS first ---
                if (b->rhs) {
                    resolve_idents_in_expr(b->rhs);
                }
                Ast_Type_Definition* rhsType = infer_types_expr(&b->rhs);

                Ast_Type_Definition* lhsType = nullptr;

                // --- CASE 1: LHS is an identifier ---
                if (Ast_Ident* lhs_ident = dynamic_cast<Ast_Ident*>(b->lhs)) {
                    CM_Symbol* sym = lookup_symbol(lhs_ident->name);
                    if (!sym) {
                        report_error(
                            lhs_ident->line_number,
                            lhs_ident->character_number,
                            "Assignment to undeclared variable '%s'",
                            //lhs_ident->name.c_str()
                            lhs_ident->name ? lhs_ident->name : ""
                        );
                    } else {
                        // Mark as initialized
                        sym->initialized = true;

                        // Infer or check type
                        if (!sym->type) {
                            sym->type = rhsType;
                        }
                        else if (!check_that_types_match(sym->type, rhsType)) {
                            report_error(
                                lhs_ident->line_number,
                                lhs_ident->character_number,
                                "Type mismatch in assignment to '%s'",
                                //lhs_ident->name.c_str()
                                lhs_ident->name ? lhs_ident->name : ""
                            );
                        }

                        lhsType = sym->type;
                    }
                }

                // --- CASE 2: LHS is a dereference (*p = value) ---
                else if (Ast_Unary* lhs_unary = dynamic_cast<Ast_Unary*>(b->lhs)) {
                    if (lhs_unary->op == UNARY_DEREFERENCE) {
                        // The operand must be a pointer expression
                        resolve_idents_in_expr(lhs_unary->operand);
                        Ast_Type_Definition* pointerType = infer_types_expr(&lhs_unary->operand);

                        if (!pointerType || !pointerType->pointed_to_type) {
                            report_error(
                                lhs_unary->line_number,
                                lhs_unary->character_number,
                                "Invalid dereference: LHS is not a pointer"
                            );
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
                }

                // --- CASE 3: LHS is invalid ---
                else {
                    report_error(
                        b->line_number,
                        b->character_number,
                        "Left-hand side of assignment must be an identifier or dereferenced pointer"
                    );
                }
            }
            else {
                // For all other binary operations, just resolve normally
                if (b->lhs) resolve_idents_in_expr(b->lhs);
                if (b->rhs) resolve_idents_in_expr(b->rhs);
            }
            break;
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression* call =
                static_cast<Ast_Procedure_Call_Expression*>(expr);

            if (call->function && call->function->type == AST_IDENT) {
                Ast_Ident* fn = static_cast<Ast_Ident*>(call->function);
                if (fn->name && strcmp(fn->name, "printf") != 0) { // allow builtins, this is temporary.
                    if (!lookup_symbol(fn->name)) {
                        report_error(
                            fn->line_number,
                            fn->character_number,
                            "Call to undeclared function '%s'",
                            fn->name
                        );
                    }
                }
            }

            if (call->arguments) {

                // for (auto* a : call->arguments->arguments) {
                for (int i = 0; i < call->arguments->arguments.count; ++i) {
                    Ast_Expression* a = call->arguments->arguments.data[i];
                    resolve_idents_in_expr(a);
                }
            }
            break;
        }

        case AST_COMMA_SEPARATED_ARGS: {
            Ast_Comma_Separated_Args* args =
                static_cast<Ast_Comma_Separated_Args*>(expr);

            // for (auto* a : args->arguments) {
            for (int i = 0; i < args->arguments.count; ++i) {
                Ast_Expression * a = args->arguments.data[i];
                resolve_idents_in_expr(a);
            }
            break;
        }

        default:
            // TODO: handle more expression kinds when added
            break;
    }
}


Ast_Type_Definition* CodeManager::make_builtin_type(Ast_Builtin_Type t) {
    // Ast_Type_Definition* out = DBG_NEW Ast_Type_Definition();
    Ast_Type_Definition* out = AST_NEW(ast_pool, Ast_Type_Definition);
    out->builtin_type = t;
    return out;
}

Ast_Type_Definition* CodeManager::infer_types_expr(Ast_Expression** expr_ptr) {
    if (!expr_ptr || !*expr_ptr) return nullptr;
    Ast_Expression* expr = *expr_ptr;

    switch (expr->type) {
        case AST_LITERAL: {
            Ast_Literal* lit = static_cast<Ast_Literal*>(expr);
            switch(lit->value_type){
                case LITERAL_NUMBER: return make_builtin_type(TYPE_INT); break;
                case LITERAL_FLOAT: return make_builtin_type(TYPE_FLOAT); break;
                case LITERAL_STRING: return make_builtin_type(TYPE_STRING); break;
                case LITERAL_TRUE:
                case LITERAL_FALSE: return make_builtin_type(TYPE_BOOL); break;
                default:
                    return nullptr;

            }
            break;
        }

        case AST_IDENT: {
            Ast_Ident* id = static_cast<Ast_Ident*>(expr);
            CM_Symbol* s = lookup_symbol(id->name);
            if (!s) {
                return nullptr; // this will be already reported by resolve_idents_in_expr
            }
            if (s->decl) {
                if (s->decl->declared_type)
                    return s->decl->declared_type;
                else if (s->initialized && !s->inferred) {
                    return infer_types_expr(&s->decl->initializer);
                }
            }
            return nullptr;
        }

        case AST_UNARY: {
            Ast_Unary* u = static_cast<Ast_Unary*>(expr);
            if (!u->operand) return nullptr;

            Ast_Type_Definition* operandType = infer_types_expr(&u->operand);
            if (!operandType) {
                report_error(u->line_number, u->character_number,
                    "Could not determine type of operand for unary expression");
                return nullptr;
            }

            Ast_Type_Definition* resultType = AST_NEW(ast_pool, Ast_Type_Definition);
            switch (u->op) {
            case UNARY_DEREFERENCE:
                if (!operandType->pointed_to_type) {
                    report_error(u->line_number, u->character_number,
                        "Cannot dereference non-pointer type");
                    return nullptr;
                }
                resultType->builtin_type = TYPE_UNKNOWN;
                resultType->pointed_to_type = operandType->pointed_to_type;
                break;

            case UNARY_ADDRESS_OF:
            case UNARY_REFERENCE:
                resultType->builtin_type = TYPE_UNKNOWN;
                resultType->pointed_to_type = operandType;
                break;

            case UNARY_NEGATE:
            case UNARY_NOT:
                // type same as operand
                resultType = operandType;
                break;

            default:
                report_error(u->line_number, u->character_number,
                    "Unknown unary operator");
                return nullptr;
            }

            expr->inferred_type = resultType;
            return resultType;
        }

        // Ast_Unary* u = static_cast<Ast_Unary*>(expr);

        //     // First infer type of the operand
        //     Ast_Type_Definition* operandType = infer_types_expr(&u->operand);
        //     if (!operandType) {
        //         report_error(u->line_number, u->character_number,
        //                      "Could not determine type of operand for unary operator.");
        //         return nullptr;
        //     }

        //     switch (u->op) {
        //         case UNARY_ADDRESS_OF: {
        //             // ^x → result is a POINTER to operand type
        //             static Ast_Type_Definition temp;
        //             temp = {};
        //             temp.pointed_to_type = operandType;

        //             expr->inferred_type = &temp;
        //             return &temp;
        //         }

        //         case UNARY_DEREFERENCE: {
        //             // *x → operand must be a pointer
        //             if (!operandType->pointed_to_type) {
        //                 report_error(u->line_number, u->character_number,
        //                              "Cannot dereference non-pointer type.");
        //                 return nullptr;
        //             }
        //             expr->inferred_type = operandType->pointed_to_type;
        //             return operandType->pointed_to_type;
        //         }

        //         case UNARY_REFERENCE: {
        //             // &x → result is a REFERENCE to operand type
        //             static Ast_Type_Definition temp;
        //             temp = {};
        //             temp.is_reference = true; // <-- You need to add this to your Ast_Type_Definition
        //             temp.pointed_to_type = operandType;

        //             expr->inferred_type = &temp;
        //             return &temp;
        //         }

        //         case UNARY_NEGATE:
        //         case UNARY_NOT:
        //             expr->inferred_type = operandType;
        //             return operandType;

        //         default:
        //             report_error(u->line_number, u->character_number,
        //                          "Unknown unary operator.");
        //             return nullptr;
        //     }
        // }


        case AST_BINARY: {
            Ast_Binary* b = static_cast<Ast_Binary*>(expr);
            Ast_Type_Definition* lt = infer_types_expr(&b->lhs);
            Ast_Type_Definition* rt = infer_types_expr(&b->rhs);

            // If arithmetic op -> int/float; if comparison -> bool
            switch (b->op) {
                case BINOP_ADD:
                case BINOP_SUB:
                case BINOP_MUL:
                case BINOP_DIV: {
                    // prefer float if either side is float
                    if ((lt && lt->builtin_type == TYPE_FLOAT) || (rt && rt->builtin_type == TYPE_FLOAT)) {
                        return make_builtin_type(TYPE_FLOAT);
                    }
                    // otherwise integer (if both sides are ints)
                    if (lt && rt && lt->builtin_type == TYPE_INT && rt->builtin_type == TYPE_INT) {
                        return make_builtin_type(TYPE_INT);
                    }
                    // type mismatch
                    report_error(b->line_number, b->character_number, "Type error in binary arithmetic: operand types incompatible");
                    return nullptr;
                }

                case BINOP_EQ:
                case BINOP_NEQ: {
                    // comparisons produce boolean
                    return make_builtin_type(TYPE_BOOL);
                }
                case BINOP_ASSIGN: {
                    // assignment expression has the type of the LHS
                    Ast_Type_Definition* rhsType = infer_types_expr(&b->rhs);
                    if (!rhsType) {
                        report_error(b->line_number, b->character_number,
                                     "Right-hand side of assignment has unknown type");
                        return nullptr;
                    }

                    Ast_Type_Definition* lhsType = nullptr;

                    // --- Case 1: LHS is an identifier ---
                    if (Ast_Ident* lhs_ident = dynamic_cast<Ast_Ident*>(b->lhs)) {
                        CM_Symbol* sym = lookup_symbol(lhs_ident->name);
                        if (!sym) {
                            report_error(lhs_ident->line_number, lhs_ident->character_number,
                                "Assignment to undeclared variable '%s'",
                                //lhs_ident->name.c_str());
                                lhs_ident->name ? lhs_ident->name : "");
                            return nullptr;
                        }

                        // Infer the type of the variable if it's not known yet
                        if (!sym->type) {
                            sym->type = rhsType;
                        }
                        else if (!check_that_types_match(sym->type, rhsType)) {
                            report_error(lhs_ident->line_number, lhs_ident->character_number,
                                "Type mismatch in assignment to '%s'",
                                //lhs_ident->name.c_str());
                                lhs_ident->name ? lhs_ident->name : "");
                        }

                        sym->initialized = true;
                        lhsType = sym->type;
                    }

                    // --- Case 2: LHS is a dereference (*p = value) ---
                    else if (Ast_Unary* lhs_unary = dynamic_cast<Ast_Unary*>(b->lhs)) {
                        if (lhs_unary->op == UNARY_DEREFERENCE) {
                            // Get type of the pointer expression
                            Ast_Type_Definition* pointerType = infer_types_expr(&lhs_unary->operand);

                            if (!pointerType || !pointerType->pointed_to_type) {
                                report_error(lhs_unary->line_number, lhs_unary->character_number,
                                             "Invalid dereference: LHS is not a pointer");
                                return nullptr;
                            }

                            lhsType = pointerType->pointed_to_type;

                            // Check that the inner type matches RHS
                            if (!check_that_types_match(lhsType, rhsType)) {
                                report_error(lhs_unary->line_number, lhs_unary->character_number,
                                             "Type mismatch: cannot assign value to dereferenced pointer");
                            }
                        }
                        else {
                            report_error(lhs_unary->line_number, lhs_unary->character_number,
                                         "Unsupported unary operation on LHS of assignment");
                            return nullptr;
                        }
                    }

                    // --- Case 3: Unsupported LHS ---
                    else {
                        report_error(b->line_number, b->character_number,
                                     "Left-hand side of assignment must be an identifier or a dereferenced pointer");
                        return nullptr;
                    }

                    // The result type of an assignment expression is the type of the LHS
                    return lhsType;
                }

                default:
                    report_error(b->line_number, b->character_number, "Unknown binary operator in type inference");
                    return nullptr;
            }
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression* call = static_cast<Ast_Procedure_Call_Expression*>(expr);
            // infer argument types
            if (call->arguments) {
                // for (auto* arg : call->arguments->arguments) {
                for (int i = 0; i < call->arguments->arguments.count; ++i) {
                    // Ast_Expression* p = arg;
                    Ast_Expression* p = call->arguments->arguments.data[i];
                    infer_types_expr(&p);
                }
            }
            // For builtins like printf, return void (or int for printf return)
            // We'll return TYPE_VOID (but Ast_Type_Definition make_builtin(TYPE_VOID))
            return make_builtin_type(TYPE_VOID);
        }

        case AST_COMMA_SEPARATED_ARGS:
            return nullptr;

        default:
            return nullptr;
    }
}

void CodeManager::infer_types_decl(Ast_Declaration* decl) {
    if (!decl) return;

    // check if already declared?
    CM_Symbol* existing = lookup_symbol_current_scope(decl->identifier->name);
    if (existing && existing->decl != decl) {
        report_error(decl->line_number, decl->character_number, "Variable is already declared in the current scope '%s'", decl->identifier->name);
        // return;
    }

    if (decl->initializer) {
        Ast_Expression* init_expr = decl->initializer;
        Ast_Type_Definition* init_type = infer_types_expr(&init_expr);

        if(!init_type) {
            report_error(decl->line_number, decl->character_number, "Could not infer type for variable %s from intitializer.",decl->identifier->name);
            return;
        }

        init_expr->inferred_type = init_type;

        if (decl->declared_type) {

            // explicit type, check if they match
                if (!check_that_types_match(decl->declared_type, init_type)) {
                report_error(
                    decl->line_number,
                    decl->character_number,
                    "Type mismatch in initializer for '%s'",
                    decl->identifier->name
                );
            }

        } else {
            // Not declared, get initializer's type
            decl->declared_type = init_type;
            CM_Symbol* sym = lookup_symbol(decl->identifier->name);
            if(sym) sym->type = init_type;
        }
    } else {
        if (!decl->declared_type) {
            // No type and no initializer  NOT NEEDED this is reported by parser so remove it
            report_error(
                decl->line_number,
                decl->character_number,
                "Cannot declare variable '%s' without type or initializer",
                decl->identifier->name
            );
        }
        // then there is just declared type, nothing to do in that case
    }
}



// Block-level inference (recurses into statements)
void CodeManager::infer_types_block(Ast_Block* block) {
    if (!block) return;

    // for (auto* decl : block->members) {
    //     if(!decl) continue;
    //     infer_types_decl(decl);
    // }

    // for (auto* stmt : block->statements) {
    for (int i = 0; i < block->statements.count; i++) {

        Ast_Statement* stmt = block->statements.data[i];

        if (!stmt) continue;

        if (stmt->type == AST_DECLARATION) {
            Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
            infer_types_decl(decl);
            // declare variable in current scope (already done by resolve pass, but ensure symbol exists)
            CM_Symbol* s = lookup_symbol(decl->identifier ? decl->identifier->name : std::string());
            if (!s) {
                // if declaration wasn't declared (maybe resolved incorrectly), declare it now
                declare_variable(decl);
            }
        } else if (stmt->expression) {
            Ast_Expression* expr = stmt->expression;
            infer_types_expr(&expr);
        } else if (stmt->block) {
            push_scope();
            infer_types_block(stmt->block);
            pop_scope();
        }
         if (stmt->type == AST_IF) {
            Ast_If* ifn = static_cast<Ast_If*>(stmt);
            if (ifn->condition) {
                Ast_Expression* cond = ifn->condition;
                infer_types_expr(&cond);
            }
            if (ifn->then_block) {
                push_scope();
                infer_types_block(ifn->then_block);
                pop_scope();
            }
            if (ifn->else_block) {
                push_scope();
                infer_types_block(ifn->else_block);
                pop_scope();
            }
        // } else if (stmt->expression) {
        //     Ast_Expression* expr = stmt->expression;
        //     infer_types_expr(&expr);
        // } else if (stmt->block) {
        //     if (stmt->block->is_scoped_block)
        //         push_scope();
        //     infer_types_block(stmt->block);
        //     if (stmt->block->is_scoped_block)
        //         pop_scope();
        }

    }
    // // 3. NEW: Recursively process all nested child scopes.
    // for (auto* child_block : block->child_scopes) {
    //     push_scope();
    //     infer_types_block(child_block);
    //     pop_scope();
    // }
}

bool CodeManager::check_that_types_match(Ast_Type_Definition* wanted, Ast_Type_Definition* have) {
    if (!wanted || !have) return false;

    // --- Handle Arrays ---
    if (wanted->array_kind != ARRAY_NONE || have->array_kind != ARRAY_NONE) {
        // Both must be arrays
        if (wanted->array_kind != have->array_kind)
            return false;

        // If static arrays, size must match
        if (wanted->array_kind == ARRAY_STATIC &&
            wanted->static_array_size != have->static_array_size)
            return false;

        // Element type must exist
        if (!wanted->element_type || !have->element_type)
            return false;

        // Compare element types recursively
        return check_that_types_match(wanted->element_type, have->element_type);
    }

    // --- Handle Pointers / References ---
    if (wanted->pointed_to_type || have->pointed_to_type) {
        // Both must be pointers/references
        if (!wanted->pointed_to_type || !have->pointed_to_type)
            return false;

        // Compare the inner types recursively
        return check_that_types_match(wanted->pointed_to_type, have->pointed_to_type);
    }

    // --- Handle Base Types ---
    if (wanted->builtin_type != TYPE_UNKNOWN && have->builtin_type != TYPE_UNKNOWN) {
        if (wanted->builtin_type == have->builtin_type)
            return true;

        // Allow implicit int -> float
        if (wanted->builtin_type == TYPE_FLOAT && have->builtin_type == TYPE_INT)
            return true;

        return false;
    }

    // If either type is unknown, conservatively assume mismatch
    return false;
}



bool CodeManager::is_integer_type(Ast_Type_Definition* type) {
    if (!type) return false;
    return type->builtin_type == TYPE_INT;
}
