#include "code_manager.h"

#include <cstdarg> // for variadic function

#define AST_NEW(pool, type) ([&]() -> type* {                   \
    assert(pool != nullptr && "Pool must not be null");         \
    void* mem = pool_alloc(pool, sizeof(type));                \
    type* node = new (mem) type(pool);                         \
    return node;                                               \
}())

CodeManager::CodeManager(Pool* pool)
    : ast_pool(pool) {}

char *CodeManager::pool_strdup(Pool* pool, const char* str) {
    size_t len = strlen(str) + 1;
    char* p = (char*)pool_alloc(pool, len);
    memcpy(p, str, len);
    //printf("pool_strdup %d\"%.*s\"\n", len, len, p);
    return p;
}
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

void CodeManager::resolve_idents(Ast_Block* block)
{
    if (!block) return;

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
        } else if (stmt->block) {

            if(stmt->block->is_scoped_block)
                push_scope();

            resolve_idents(stmt->block);

            if(stmt->block->is_scoped_block)
                pop_scope();
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
            // Don't set expr->inferred_type here; that's for type inference pass
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
                Ast_Type_Definition* rhsType = infer_types_expr(&b->rhs);

                Ast_Type_Definition* lhsType = nullptr;

                if (Ast_Ident* lhs_ident = dynamic_cast<Ast_Ident*>(b->lhs))
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
                } else if (Ast_Unary* lhs_unary = dynamic_cast<Ast_Unary*>(b->lhs)) {
                    if (lhs_unary->op == UNARY_DEREFERENCE) {

                        // must be a pointer expression
                        resolve_idents_in_expr(lhs_unary->operand);
                        Ast_Type_Definition* pointerType = infer_types_expr(&lhs_unary->operand);

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
                // For all other binary operations, just resolve normally
                if (b->lhs) resolve_idents_in_expr(b->lhs);
                if (b->rhs) resolve_idents_in_expr(b->rhs);
            }
            break;
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression* call =
                static_cast<Ast_Procedure_Call_Expression*>(expr);

            if (call->function && call->function->type == AST_IDENT)
            {
                Ast_Ident* fn = static_cast<Ast_Ident*>(call->function);

                if (fn->name && strcmp(fn->name, "printf") != 0) // allow builtins, this is Temporary.
                {

                    if (!lookup_symbol(fn->name)){
                        report_error(fn->line_number, fn->character_number, "Call to undeclared function '%s'", fn->name);
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


Ast_Type_Definition* CodeManager::make_builtin_type(Ast_Builtin_Type t) {
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
                default: return nullptr;
            }
            break;
        }

        case AST_IDENT: {
            Ast_Ident* id = static_cast<Ast_Ident*>(expr);
            CM_Symbol* s = lookup_symbol(id->name);

            if (s->decl)
            {
                if (s->decl->declared_type) {
                    return s->decl->declared_type;
                } else if (s->initialized && !s->inferred) {
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
            case UNARY_DEREFERENCE: {
                if (!operandType->pointed_to_type) {
                    report_error(u->line_number, u->character_number,
                        "Cannot dereference non-pointer type");
                    return nullptr;
                }
                Ast_Type_Definition* resultType = operandType->pointed_to_type;
                expr->inferred_type = resultType;
                return resultType;
            }
            case UNARY_ADDRESS_OF:
                resultType->builtin_type = TYPE_UNKNOWN;
                resultType->pointed_to_type = operandType;
                break;
            // case UNARY_NEGATE:
            // case UNARY_NOT:
            //     // type same as operand
            //     resultType = operandType;
            //     break;

            default:
                report_error(u->line_number, u->character_number, "Unknown unary operator");
                return nullptr;
            }

            expr->inferred_type = resultType;
            return resultType;
        }

        case AST_BINARY: {

            Ast_Binary* b = static_cast<Ast_Binary*>(expr);
            Ast_Type_Definition* lt = infer_types_expr(&b->lhs);
            Ast_Type_Definition* rt = infer_types_expr(&b->rhs);

            switch (b->op) {
                case BINOP_ADD:
                case BINOP_SUB:
                case BINOP_MUL:
                case BINOP_DIV: {

                    // prefer float
                    if ((lt && lt->builtin_type == TYPE_FLOAT) || (rt && rt->builtin_type == TYPE_FLOAT)) {
                        return make_builtin_type(TYPE_FLOAT);
                    }

                    if (lt && rt && lt->builtin_type == TYPE_INT && rt->builtin_type == TYPE_INT) {
                        return make_builtin_type(TYPE_INT);
                    }
                    // type mismatch
                    report_error(b->line_number, b->character_number, "Type error in binary arithmetic: operand types incompatible");
                    return nullptr;
                }

                case BINOP_EQ:
                case BINOP_NEQ: {
                    return make_builtin_type(TYPE_BOOL);
                }
                case BINOP_ASSIGN: {
                    Ast_Type_Definition* rhsType = infer_types_expr(&b->rhs);
                    if (!rhsType) {
                        report_error(b->line_number, b->character_number,
                                     "Right-hand side of assignment has unknown type");
                        return nullptr;
                    }

                    Ast_Type_Definition* lhsType = nullptr;

                    if (Ast_Ident* lhs_ident = dynamic_cast<Ast_Ident*>(b->lhs))
                    {
                        CM_Symbol* sym = lookup_symbol(lhs_ident->name);
                        if (!sym)
                        {
                            report_error(lhs_ident->line_number, lhs_ident->character_number,
                                "Assignment to undeclared variable '%s'",
                                lhs_ident->name ? lhs_ident->name : "");
                            return nullptr;
                        }

                        if (!sym->type) {
                            sym->type = rhsType;
                        }
                        else if (!check_that_types_match(sym->type, rhsType))
                        {
                            report_error(lhs_ident->line_number, lhs_ident->character_number,
                                "Type mismatch in assignment to '%s'",
                                lhs_ident->name ? lhs_ident->name : "");
                        }

                        sym->initialized = true;
                        lhsType = sym->type;
                    }

                    // lhs is a dereference
                    else if (Ast_Unary* lhs_unary = dynamic_cast<Ast_Unary*>(b->lhs))
                    {
                        if (lhs_unary->op == UNARY_DEREFERENCE)
                        {
                            Ast_Type_Definition* pointerType = infer_types_expr(&lhs_unary->operand);

                            if (!pointerType || !pointerType->pointed_to_type)
                            {
                                report_error(lhs_unary->line_number, lhs_unary->character_number,
                                             "Invalid dereference: LHS is not a pointer");
                                return nullptr;
                            }

                            if (Ast_Ident* pointer_ident = dynamic_cast<Ast_Ident*>(lhs_unary->operand))
                            {
                                CM_Symbol* pointer_sym = lookup_symbol(pointer_ident->name);
                                if (pointer_sym && !pointer_sym->initialized) {
                                    report_error(lhs_unary->line_number, lhs_unary->character_number,
                                                 "Cannot dereference uninitialized pointer '%s'",
                                                 pointer_ident->name ? pointer_ident->name : "");

                                    return nullptr;
                                }
                            }

                            lhsType = pointerType->pointed_to_type;

                            if (!check_that_types_match(lhsType, rhsType)) {
                                report_error(lhs_unary->line_number, lhs_unary->character_number,
                                             "Type mismatch: cannot assign value to dereferenced pointer");
                            }

                        } else {
                            report_error(lhs_unary->line_number, lhs_unary->character_number,
                                         "Unsupported unary operation on LHS of assignment");
                            return nullptr;
                        }
                    } else {
                        report_error(b->line_number, b->character_number,
                                     "Left-hand side of assignment must be an identifier or a dereferenced pointer");
                        return nullptr;
                    }

                    return lhsType;
                }

                default:
                    report_error(b->line_number, b->character_number, "Unknown binary operator in type inference");
                    return nullptr;
            }
        }

        case AST_PROCEDURE_CALL_EXPRESSION: {
            Ast_Procedure_Call_Expression* call = static_cast<Ast_Procedure_Call_Expression*>(expr);
            if (call->arguments)
            {
                for (int i = 0; i < call->arguments->arguments.count; ++i)
                {
                    Ast_Expression* p = call->arguments->arguments.data[i];
                    infer_types_expr(&p);
                }
            }
            // builtins like printf
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

    if (decl->initializer) {
        Ast_Expression* init_expr = decl->initializer;
        Ast_Type_Definition* init_type = infer_types_expr(&init_expr);

        if(!init_type) {
            report_error(decl->line_number, decl->character_number, "Could not infer type for variable %s from intitializer.",decl->identifier->name);
            return;
        }

        init_expr->inferred_type = init_type;

        if (decl->declared_type) {

                // explicitly typed
                if (!check_that_types_match(decl->declared_type, init_type)) {
                report_error(
                    decl->line_number,
                    decl->character_number,
                    "Type mismatch in initializer for '%s'",
                    decl->identifier->name
                );
            }

        } else {
            // not declared with type so infer it through initializer's type instead
            decl->declared_type = init_type;
            CM_Symbol* sym = lookup_symbol(decl->identifier->name);
            if(sym) sym->type = init_type;
        }
    }
}


void CodeManager::infer_types_block(Ast_Block* block) {
    if (!block) return;

    for (int i = 0; i < block->statements.count; i++) {

        Ast_Statement* stmt = block->statements.data[i];

        if (!stmt) continue;

        if (stmt->type == AST_DECLARATION) {
            Ast_Declaration* decl = static_cast<Ast_Declaration*>(stmt);
            infer_types_decl(decl);

            // declare variable in current scope (already done by resolve pass, but ensure symbol exists)
            CM_Symbol* s = lookup_symbol(decl->identifier ? decl->identifier->name : "");
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
        }

    }
}

bool CodeManager::check_that_types_match(Ast_Type_Definition* wanted, Ast_Type_Definition* have, bool is_pointer)
{
    if (!wanted || !have) return false;

    if (wanted->array_kind != ARRAY_NONE || have->array_kind != ARRAY_NONE)
    {

        if (wanted->array_kind != have->array_kind)
            return false;

        if (wanted->array_kind == ARRAY_STATIC &&
            wanted->static_array_size != have->static_array_size)
            return false;

        if (!wanted->element_type || !have->element_type)
            return false;

        return check_that_types_match(wanted->element_type, have->element_type);
    }

    if (wanted->pointed_to_type || have->pointed_to_type) {

        if (!wanted->pointed_to_type || !have->pointed_to_type)
            return false;

        return check_that_types_match(wanted->pointed_to_type, have->pointed_to_type, true);
    }

    if (wanted->builtin_type != TYPE_UNKNOWN && have->builtin_type != TYPE_UNKNOWN) {
        if (wanted->builtin_type == have->builtin_type)
            return true;

        // we allow implicit int -> float, but not if its a pointer type
        if (wanted->builtin_type == TYPE_FLOAT && have->builtin_type == TYPE_INT)
        {
            if(is_pointer == true) return false; // Temporaryy!! this wont work for future types as we add them
            return true;

        }
        return false;
    }

    return false;
}
