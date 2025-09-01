#include "code_manager.h"
#include <iostream>
#include <sstream>
#include <cstdarg>
// -------------------------------------------------
// Initialization / errors
// -------------------------------------------------
void CodeManager::init() {
    scopes.clear();
    scopes.emplace_back(); // global scope
    // errors.clear();
}

// bool CodeManager::has_errors() const {
//     return !errors.empty();
// }

// void CodeManager::print_errors() const {
//     for (auto const &e : errors) {
//         std::cerr << e << std::endl;
//     }
// }

int CodeManager::get_count_errors(){
    return count_errors;
}
// -------------------------------------------------
// Utility: report error (collect only)
// -------------------------------------------------
void CodeManager::report_error(int line, int col, const char* fmt, ...) {
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

// -------------------------------------------------
// Scopes & Symbols
// -------------------------------------------------
void CodeManager::push_scope() {
    scopes.emplace_back();
}

void CodeManager::pop_scope() {
    if (!scopes.empty()) scopes.pop_back();
}

bool CodeManager::declare_variable(Ast_Declaration* decl) {
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


CM_Symbol* CodeManager::lookup_symbol(const std::string& name) {
    for (int i = (int)scopes.size() - 1; i >= 0; --i) {
        for (auto &sym : scopes[i]) {
            if (sym.name == name) return &sym;
        }
    }
    return nullptr;
}

void CodeManager::mark_initialized(const std::string& name) {
    CM_Symbol* s = lookup_symbol(name);
    if (s) s->initialized = true;
}

// -------------------------------------------------
// Identifier resolution
// Walk block -> statements -> declarations/expressions
// -------------------------------------------------
void CodeManager::resolve_idents(Ast_Block* block) {
    if (!block) return;

    for (auto* stmt : block->statements) {
        if (!stmt) continue;

        // Declaration nodes are statements of type AST_DECLARATION
        if (stmt->type == AST_DECLARATION) {
            Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
            // first resolve initializer expressions (they may refer to earlier variables)
            resolve_idents_in_declaration(decl);
            // then declare the variable (in current scope)
            declare_variable(decl);
            continue;
        }

        // If statement (statement-subclass for if)
        if (stmt->type == AST_IF) {
            Ast_If* ifn = static_cast<Ast_If*>(stmt);
            if (ifn->condition) resolve_idents_in_expr(ifn->condition);

            // then block: create inner scope
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
            continue;
        }

        // Generic statement may hold expression or nested block
        if (stmt->expression) {
            resolve_idents_in_expr(stmt->expression);
        }
        if (stmt->block) {
            push_scope();
            resolve_idents(stmt->block);
            pop_scope();
        }
    }
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
            // nothing to resolve
            break;

        case AST_BINARY: {
            Ast_Binary* b = static_cast<Ast_Binary*>(expr);

            if (b->op == BINOP_ASSIGN) { // assignment
                // LHS must be an identifier
                Ast_Ident* lhs_ident = dynamic_cast<Ast_Ident*>(b->lhs);
                if (!lhs_ident) {
                    report_error(
                        b->line_number,
                        b->character_number,
                        "Left-hand side of assignment must be an identifier"
                    );
                } else {
                    CM_Symbol* sym = lookup_symbol(lhs_ident->name);
                    if (!sym) {
                        report_error(
                            lhs_ident->line_number,
                            lhs_ident->character_number,
                            "Assignment to undeclared variable '%s'",
                            lhs_ident->name
                        );
                    } else {
                        // Mark initialized
                        sym->initialized = true;

                        // Type inference / checking
                        Ast_Type_Definition* rhsType = infer_types_expr(&b->rhs);
                        if (!sym->type) {
                            // no explicit type, adopt RHS type
                            sym->type = rhsType;
                        } else if (!check_that_types_match(sym->type, rhsType)) {
                            report_error(
                                lhs_ident->line_number,
                                lhs_ident->character_number,
                                "Type mismatch in assignment to '%s'",
                                lhs_ident->name
                            );
                        }
                    }
                }

                // Always analyze RHS expression
                if (b->rhs) resolve_idents_in_expr(b->rhs);
            } else {
                // Other binary ops: resolve both sides normally
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
                if (fn->name != "printf") { // allow builtins
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
                for (auto* a : call->arguments->arguments) {
                    resolve_idents_in_expr(a);
                }
            }
            break;
        }

        case AST_COMMA_SEPARATED_ARGS: {
            Ast_Comma_Separated_Args* args =
                static_cast<Ast_Comma_Separated_Args*>(expr);
            for (auto* a : args->arguments) {
                resolve_idents_in_expr(a);
            }
            break;
        }

        default:
            // TODO: handle more expression kinds when added
            break;
    }
}


// -------------------------------------------------
// Type inference & checking
// Returns an Ast_Type_Definition* (may allocate temporary builtin)
// -------------------------------------------------

// Helper to create a fresh builtin type (allocated); caller doesn't have to delete in short term.
// In a bigger system you'd have an internal type table / arena.
static Ast_Type_Definition* make_builtin_type(Ast_Builtin_Type t) {
    Ast_Type_Definition* out = new Ast_Type_Definition();
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
                // already reported by resolve pass; return nullptr
                return nullptr;
            }
            if (s->decl && s->decl->declared_type) {
                return s->decl->declared_type;
            }
            return nullptr;
        }

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
                    Ast_Ident* lhs_ident = dynamic_cast<Ast_Ident*>(b->lhs);
                    Ast_Type_Definition* rhsType = infer_types_expr(&b->rhs);

                    if (!lhs_ident) {
                        report_error(b->line_number, b->character_number,
                                     "Left-hand side of assignment must be an identifier");
                        return nullptr;
                    }

                    CM_Symbol* sym = lookup_symbol(lhs_ident->name);
                    if (!sym) {
                        report_error(lhs_ident->line_number, lhs_ident->character_number,
                                     "Assignment to undeclared variable '%s'",
                                     lhs_ident->name.c_str());
                        return nullptr;
                    }

                    if (!sym->type) {
                        // Infer type from RHS
                        sym->type = rhsType;
                    } else if (!check_that_types_match(sym->type, rhsType)) {
                        report_error(lhs_ident->line_number, lhs_ident->character_number,
                                     "Type mismatch in assignment to '%s'",
                                     lhs_ident->name.c_str());
                    }

                    sym->initialized = true;

                    // The result type of an assignment expression is the type of the variable
                    return sym->type;
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
                for (auto* arg : call->arguments->arguments) {
                    Ast_Expression* p = arg;
                    infer_types_expr(&p);
                }
            }
            // For builtins like printf, return void (or int for printf return)
            // We'll return TYPE_VOID (but Ast_Type_Definition make_builtin(TYPE_VOID))
            return make_builtin_type(TYPE_VOID);
        }

        case AST_COMMA_SEPARATED_ARGS:
            // not a value-producing expression per se; return nullptr
            return nullptr;

        default:
            // other expressions not handled yet
            return nullptr;
    }
}

// Declaration-level type inference and checking
void CodeManager::infer_types_decl(Ast_Declaration* decl) {
    if (!decl) return;

    if (decl->initializer) {
        Ast_Expression* init_expr = decl->initializer;
        Ast_Type_Definition* init_type = infer_types_expr(&init_expr);

        init_expr->inferred_type = init_type;

        if (decl->declared_type) {
            // Explicit type → check compatibility
            if (!check_that_types_match(decl->declared_type, init_type)) {
                report_error(
                    decl->line_number,
                    decl->character_number,
                    "Type mismatch in initializer for '%s'",
                    decl->identifier->name
                );
            }
        } else {
            // No declared type → adopt initializer type
            decl->declared_type = init_type;
        }
    } else {
        if (!decl->declared_type) {
            // No type and no initializer → invalid
            report_error(
                decl->line_number,
                decl->character_number,
                "Cannot declare variable '%s' without type or initializer",
                decl->identifier->name
            );
        }
        // Case: x : int; → valid, just no initializer
    }
}



// Block-level inference (recurses into statements)
void CodeManager::infer_types_block(Ast_Block* block) {
    if (!block) return;

    for (auto* stmt : block->statements) {
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
        } else if (stmt->type == AST_IF) {
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
        }
    }
}

// -------------------------------------------------
// Type helpers
// -------------------------------------------------
bool CodeManager::check_that_types_match(Ast_Type_Definition* wanted, Ast_Type_Definition* have) {
    if (!wanted || !have) return false;
    // if both are builtin, compare builtin_type
    if (wanted->builtin_type != TYPE_UNKNOWN && have->builtin_type != TYPE_UNKNOWN) {
        if(wanted->builtin_type == have->builtin_type) return true;
        if(wanted->builtin_type == TYPE_FLOAT && have->builtin_type == TYPE_INT) return true;
        // return wanted->builtin_type == have->builtin_type;
    }
    // if wanted is named type, compare name
    if (!wanted->name.empty() && !have->name.empty()) {
        return wanted->name == have->name;
    }
    // fallback: false
    return false;
}

bool CodeManager::is_integer_type(Ast_Type_Definition* type) {
    if (!type) return false;
    return type->builtin_type == TYPE_INT;
}
