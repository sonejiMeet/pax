#include "code_manager.h"
#include <iostream>
#include <sstream>

// -------------------------------------------------
// Initialization / errors
// -------------------------------------------------
void CodeManager::init() {
    scopes.clear();
    scopes.emplace_back(); // global scope
    errors.clear();
}

bool CodeManager::has_errors() const {
    return !errors.empty();
}

void CodeManager::print_errors() const {
    for (auto const &e : errors) {
        std::cerr << e << std::endl;
    }
}

// -------------------------------------------------
// Utility: report error (collect only)
// -------------------------------------------------
void CodeManager::report_error(const std::string& msg, int line, int col) {
    std::ostringstream ss;
    if (line >= 0 && col >= 0) {
        ss << "Semantic error (" << line << ":" << col << "): " << msg;
    } else {
        ss << "Semantic error: " << msg;
    }
    errors.push_back(ss.str());
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
    if (!decl || !decl->identifier) return false;
    const std::string name = decl->identifier->name;
    CM_Scope &cur = scopes.back();

    // check redeclare in current scope
    for (auto &sym : cur) {
        if (sym.name == name) {
            report_error("Redeclaration of variable '" + name + "'", decl->line_number, decl->character_number);
            return false;
        }
    }

    CM_Symbol s;
    s.name = name;
    s.decl = decl;
    s.initialized = (decl->initializer != nullptr);
    cur.push_back(s);
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
                // if it's a function name (like printf) we might not require declaration -> but
                // for identifiers used as variables, require declaration.
                report_error("Use of undeclared identifier '" + id->name + "'", id->line_number, id->character_number);
            }
            break;
        }

        case AST_LITERAL:
            // nothing to resolve
            break;

        case AST_BINARY: {
            Ast_Binary* b = static_cast<Ast_Binary*>(expr);
            if (b->lhs) resolve_idents_in_expr(b->lhs);
            if (b->rhs) resolve_idents_in_expr(b->rhs);
            break;
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression* call = static_cast<Ast_Procedure_Call_Expression*>(expr);
            // function identifier: if it's an identifier node, we may or may not require it to be declared.
            // For now do not treat builtins as errors; only check if function ident is not a variable use.
            if (call->function) {
                // If function is an identifier expression (Ast_Ident), we try to resolve but do not error if builtin
                if (call->function->type == AST_IDENT) {
                    Ast_Ident* fn = call->function;
                    // allow builtin printf without declaration: special-case
                    if (fn->name != "printf") {
                        if (!lookup_symbol(fn->name)) {
                            report_error("Call to undeclared function '" + fn->name + "'", fn->line_number, fn->character_number);
                        }
                    }
                }
            }
            if (call->arguments) {
                for (auto* a : call->arguments->arguments) resolve_idents_in_expr(a);
            }
            break;
        }

        case AST_COMMA_SEPARATED_ARGS:
            // if an expression node of this type appears standalone (rare), resolve its children
            {
                Ast_Comma_Separated_Args* args = static_cast<Ast_Comma_Separated_Args*>(expr);
                for (auto* a : args->arguments) resolve_idents_in_expr(a);
            }
            break;

        default:
            // If there are additional expression kinds, handle them here
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
            if (lit->value_type == LITERAL_NUMBER) {
                return make_builtin_type(TYPE_INT);
            } else if (lit->value_type == LITERAL_FLOAT) {
                return make_builtin_type(TYPE_FLOAT);
            } else if (lit->value_type == LITERAL_STRING) {
                return make_builtin_type(TYPE_STRING);
            } else {
                return nullptr;
            }
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
                    report_error("Type error in binary arithmetic: operand types incompatible", b->line_number, b->character_number);
                    return nullptr;
                }

                case BINOP_EQ:
                case BINOP_NEQ: {
                    // comparisons produce boolean
                    return make_builtin_type(TYPE_BOOL);
                }

                default:
                    report_error("Unknown binary operator in type inference", b->line_number, b->character_number);
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

// Declaration-level inference
void CodeManager::infer_types_decl(Ast_Declaration* decl) {
    if (!decl) return;
    // if declared_type is present, check initializer type matches
    if (decl->initializer) {
        Ast_Expression* init_expr = decl->initializer;
        Ast_Type_Definition* init_type = infer_types_expr(&init_expr);
        if (decl->declared_type) {
            if (!check_that_types_match(decl->declared_type, init_type)) {
                report_error("Type mismatch in initializer for '" + decl->identifier->name + "'", decl->line_number, decl->character_number);
            }
        } else {
            // if no declared type, we could set declared_type = init_type (type inference for var)
            // (here we leave as-is)
        }
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
        return wanted->builtin_type == have->builtin_type;
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
