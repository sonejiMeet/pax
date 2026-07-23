
bool exitSuccess = true;

#ifdef _DEBUG
#define AST_NEW_LOG(type) \
   printf("-----------------------------------used AST_NEW for [%d %d] %s---%zu---\n", current->row, current->col, typeid(type).name(), sizeof(type));
#else
#define AST_NEW_LOG(type)
#endif

#ifdef AST_NEW
#undef AST_NEW
#endif

// MACRO
#define AST_NEW(type) ([&]() -> type* {                         \
    AST_NEW_LOG(type)                                           \
    assert(interp->pool != nullptr && "Pool must not be null"); \
    void *mem = pool_alloc_debug(interp->pool, sizeof(type), #type, "PARSER", current->row, current->col);         \
    type *node = new (mem) type(interp->pool);                  \
    node->line_number = current->row;                           \
    node->character_number = current->col;                      \
    node->file_name = interp->current_file;                     \
    return node;                                                \
}())



Parser::Parser(Lexer *l, Pax_Interp *_interp) : lexer(l) {

    interp = _interp;
    current = lexer->nextToken();
}

void Parser::advance() {
    previous = current;
    current = lexer->nextToken();
}

void Parser::report_parse_error(const char *fmt, ...)
{
    constexpr size_t BUFFER_SIZE = 512;
    char buffer[BUFFER_SIZE];

    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, BUFFER_SIZE, fmt, args);
    va_end(args);

    interp->buffer_error(interp->current_file, current->row, current->col, pool_strdup(interp->pool, buffer));

    exitSuccess = false;
    synchronize();
}


void Parser::expect(TokenType expectedType, const char *errorMessage)
{
    if (current->type != expectedType) {
        report_parse_error(errorMessage);
        // exitSuccess = false;
        // synchronize();
        return;
    }
    advance();
}

void Parser::Expect(TokenType expectedType, const char *errorMessage)
{
    if (current->type != expectedType) {
        interp->buffer_error(interp->current_file, previous->row, previous->col, errorMessage);
        // exitSuccess = false;
        // synchronize();
        // return;
    }
    advance();
}


void Parser::synchronize() {
    advance();
    while (current->type != TOK_END_OF_FILE) {
        if (previous && previous->type == TOK_SEMICOLON) return;
        if (current->type == TOK_IDENTIFIER) {
            current = previous;
            return;
        }
        switch (lexer->peekNextToken()->type) {
            case TOK_IF:
            case TOK_WHILE:
            case TOK_RETURN:
            case TOK_PRINT:
            case TOK_MAIN_ENTRY_POINT:
            case TOK_RCURLY_PAREN:
                return;
            // default:
            //     break;
        }
        advance();
    }
}
//
//  KEEP THIS OLD RECURSIVE DECENT BELOW HERE
//  MAYBE WANT TO COMPARE IN THESIS WHY IT'S SLOWER AND LESS EFFICIENT....s
//


// // in an expression
// Ast_Expression *Parser::parseFactor()
// {
//     // Handle unary operators first: *, &
//     if (current->type == TOK_STAR) { // dereference
//         advance();
//         Ast_Unary *node = AST_NEW(Ast_Unary);
//         node->op = UNARY_DEREFERENCE;
//         node->operand = parseFactor();
//         return node;
//     }
//     else if (current->type == TOK_AMPERSAND) { // address of
//         advance();
//         Ast_Unary *node = AST_NEW(Ast_Unary);
//         node->op = UNARY_ADDRESS_OF;
//         node->operand = parseFactor();
//         return node;
//     }
//     else if (current->type == TOK_MINUS) {
//         Token *lookahead = lexer->peekNextToken();
//         if (lookahead->type == TOK_MINUS) {
//             report_parse_error("Consecutive unary minus operators are not allowed.");
//         }
//         advance();
//         Ast_Unary *node = AST_NEW(Ast_Unary);
//         node->op = UNARY_NEGATE;
//         node->operand = parseFactor();
//         return node;
//     }
//     else if (current->type == TOK_EXCLAMATION_MARK) {
//         Token *lookahead = lexer->peekNextToken();
//         if (lookahead->type == TOK_EXCLAMATION_MARK) {
//             report_parse_error("Consecutive unary exclamation mark operators are not allowed.");
//         }
//         advance();
//         Ast_Unary *node = AST_NEW(Ast_Unary);
//         node->op = UNARY_NOT;
//         node->operand = parseFactor();
//         return node;
//     }
//     else if (current->type == TOK_NUMBER)
//     {

//         Ast_Literal *node = AST_NEW(Ast_Literal);
//         node->value_type = LITERAL_NUMBER;
//         node->integer_value = current->int_value;
//         advance();
//         return node;
//     }
//     else if (current->type == TOK_FLOAT)
//     {

//         Ast_Literal *node = AST_NEW(Ast_Literal);
//         node->value_type = LITERAL_FLOAT;
//         node->float_value = current->float64_value;
//         advance();
//         return node;
//     }
//     else if (current->type == TOK_STRING )
//     {
//         Ast_Literal *node = AST_NEW(Ast_Literal);
//         node->value_type = LITERAL_STRING;
//         node->string_value = reinterpret_cast<const char*>(current->string_value.data);

//         advance();
//         return node;
//     }
//     else if (current->type == TOK_KEYWORD_TRUE || current->type == TOK_KEYWORD_FALSE)
//     {
//         Ast_Literal *node = AST_NEW(Ast_Literal);
//         if(current->type == TOK_KEYWORD_TRUE){
//             node->value_type = LITERAL_TRUE;
//         } else node->value_type = LITERAL_FALSE;

//         advance();
//         return node;
//     }
//     else if (current->type == TOK_IDENTIFIER)
//     {
//         Token *lookahead = lexer->peekNextToken();
//         if(lookahead->type == TOK_LPAREN){
//             return parseCall();
//         } else {

//             Ast_Ident *node = AST_NEW(Ast_Ident);
//             node->name = current->value;

//             advance();
//             return node;
//         }
//     }
//     else if (current->type == TOK_LPAREN)
//     {
//         advance();
//         Ast_Expression *expr = parseExpression();

//         Expect(TOK_RPAREN, "Expected ')' after expression in parentheses.");

//         return expr;
//     }

//     report_parse_error("Expected a literal, identifier, or parenthesised expression factor.");

//     return nullptr;
// }

// Ast_Expression *Parser::parseTerm()
// {
//     Ast_Expression *left = parseFactor();

//     while (current->type == TOK_STAR || current->type == TOK_SLASH)
//     {
//         Ast_Binary *node = AST_NEW(Ast_Binary);
//         node->lhs = left;

//         if(current->type == TOK_STAR) node->op = BINOP_MUL;
//         else if (current->type == TOK_SLASH) node->op = BINOP_DIV;

//         advance();
//         node->rhs = parseFactor();

//         left = node;
//     }
//     return left;
// }

// Ast_Expression *Parser::parseAdditive()
// {
//     Ast_Expression *left = parseTerm();

//     while (current->type == TOK_PLUS || current->type == TOK_MINUS) {
//         Ast_Binary *node = AST_NEW(Ast_Binary);
//         node->lhs = left;

//         switch (current->type) {
//             case TOK_PLUS: node->op = BINOP_ADD; break;
//             case TOK_MINUS: node->op = BINOP_SUB; break;
//             default: break;
//         }

//         advance();
//         node->rhs = parseTerm();
//         left = node;
//     }
//     return left;
// }

// Ast_Expression *Parser::parseExpression()
// {
//     Ast_Expression *left = parseAdditive();

//     while (current->type == TOK_EQUAL || current->type == TOK_NOT_EQUAL ||
//            current->type == TOK_LESS  ||  current->type == TOK_GREATER  ||
//            current->type == TOK_LESS_EQUAL || current->type == TOK_GREATER_EQUAL || current->type == TOK_DOT) {

//         Ast_Binary *node = AST_NEW(Ast_Binary);
//         node->lhs = left;

//         switch (current->type) {
//             case TOK_EQUAL: node->op = BINOP_EQ; break;
//             case TOK_NOT_EQUAL: node->op = BINOP_NEQ; break;
//             case TOK_LESS: node->op = BINOP_LESS; break;
//             case TOK_GREATER: node->op = BINOP_GREATER; break;
//             case TOK_LESS_EQUAL: node->op = BINOP_LESS_EQUAL; break;
//             case TOK_GREATER_EQUAL: node->op = BINOP_GREATER_EQUAL; break;
//             case TOK_DOT: node->op = BINOP_DOT; break;
//             default: break;
//         }

//         advance();
//         node->rhs = parseAdditive(); // Parse right-hand side with higher precedence
//         left = node;
//     }
//     return left;
// }

Ast_Expression *Parser::parseExpression(int minPrecedence)
{
    Ast_Expression *left = nullptr;

    // Handle unary operators and primary expressions
    if (current->type == TOK_STAR) { // dereference
        advance();
        Ast_Unary *node = AST_NEW(Ast_Unary);
        node->op = UNARY_DEREFERENCE;
        node->operand = parseExpression(100); // High precedence for unary
        left = node;
    }
    else if (current->type == TOK_AMPERSAND) { // address of
        advance();
        Ast_Unary *node = AST_NEW(Ast_Unary);
        node->op = UNARY_ADDRESS_OF;
        node->operand = parseExpression(100);
        left = node;
    }
    else if (current->type == TOK_MINUS) {
        if (lexer->peekNextToken()->type == TOK_MINUS) {
            report_parse_error("Consecutive unary minus operators are not allowed.");
        }
        advance();
        Ast_Unary *node = AST_NEW(Ast_Unary);
        node->op = UNARY_NEGATE;
        node->operand = parseExpression(100);
        left = node;
    }
    else if (current->type == TOK_EXCLAMATION_MARK) {
        if (lexer->peekNextToken()->type == TOK_EXCLAMATION_MARK) {
            report_parse_error("Consecutive unary exclamation mark operators are not allowed.");
        }
        advance();
        Ast_Unary *node = AST_NEW(Ast_Unary);
        node->op = UNARY_NOT;
        node->operand = parseExpression(100);
        left = node;
    }
    else if (current->type == TOK_NUMBER) {
        Ast_Literal *node = AST_NEW(Ast_Literal);
        node->value_type = LITERAL_NUMBER;
        node->integer_value = current->int_value;
        advance();
        left = node;
    }
    else if (current->type == TOK_FLOAT) {
        Ast_Literal *node = AST_NEW(Ast_Literal);
        node->value_type = LITERAL_FLOAT;
        node->float_value = current->float64_value;
        advance();
        left = node;
    }
    else if (current->type == TOK_STRING) {
        Ast_Literal *node = AST_NEW(Ast_Literal);
        node->value_type = LITERAL_STRING;
        node->string_value = reinterpret_cast<const char*>(current->string_value.data);
        // node->string_count = current->string_value.count;
        advance();
        left = node;
    }
    else if (current->type == TOK_KEYWORD_TRUE || current->type == TOK_KEYWORD_FALSE) {
        Ast_Literal *node = AST_NEW(Ast_Literal);
        node->value_type = (current->type == TOK_KEYWORD_TRUE) ? LITERAL_TRUE : LITERAL_FALSE;
        advance();
        left = node;
    }
    else if (current->type == TOK_IDENTIFIER) {
        if (lexer->peekNextToken()->type == TOK_LPAREN) {
            left = parseCall();
        } else {
            Ast_Ident *node = AST_NEW(Ast_Ident);
            node->name = current->value;
            advance();
            left = node;
        }
    }
    else if (current->type == TOK_UNDERSCORE) {
        Ast_Ident *node = AST_NEW(Ast_Ident);
        node->name = "_";
        advance();
        left = node;
    }
    else if (current->type == TOK_NULL){
        Ast_Literal *node = AST_NEW(Ast_Literal);
        node->value_type = LITERAL_NULL;
        advance();
        left = node;
    }
    else if (current->type == TOK_LPAREN) {
        advance();
        left = parseExpression();
        Expect(TOK_RPAREN, "Expected ')' after expression in parentheses.");
    }
    else if (current->type == TOK_CAST) {
        advance();
        expect(TOK_LPAREN, "Expected '(' after cast");
        Ast_Type_Definition *target_type = parseTypeSpecifier();
        expect(TOK_RPAREN, "Expected ')' after cast type");

        Ast_Expression *operand = parseExpression(100);

        Ast_Cast *cast = AST_NEW(Ast_Cast);
        cast->cast_expression = target_type;
        cast->expression = operand;
        left = cast;
    }
    else {
        report_parse_error("Expected a literal, identifier, or an expression.");
        return nullptr;
    }

    // Handle binary operators based on precedence
    while (true) {
        int precedence = getPrecedence(current->type);
        if (precedence < minPrecedence) return left;

        TokenType op = current->type;

        if (op == TOK_LBRACKET){
            advance();
            Ast_Binary *node = AST_NEW(Ast_Binary);
            node->op = BINOP_ARRAY_SUBSCRIPT;
            node->lhs = left;
            node->rhs = parseExpression(); // index

            expect(TOK_RBRACKET, "Expected ']' after array index");
            left = node;
            continue;
        }

        advance();

        Ast_Binary *node = AST_NEW(Ast_Binary);
        node->lhs = left;
        node->op = getBinaryOperator(op);
        node->rhs = parseExpression(precedence + 1);
        left = node;
    }

    return left;
}

int Parser::getPrecedence(TokenType type) {
    switch (type) {
        case TOK_DOT:
        case TOK_LBRACKET:
            return 100;
        case TOK_STAR:
        case TOK_SLASH:
            return 90;
        case TOK_PLUS:
        case TOK_MINUS:
            return 80;
        case TOK_EQUAL:
        case TOK_NOT_EQUAL:
        case TOK_LESS:
        case TOK_GREATER:
        case TOK_LESS_EQUAL:
        case TOK_GREATER_EQUAL:
            return 70;
        case TOK_LOGICAL_AND:
            return 60;
        case TOK_LOGICAL_OR:
            return 50;
        default:
            return -1;
    }
}

Binary_Op Parser::getBinaryOperator(TokenType type) {
    switch (type) {
        case TOK_DOT: return BINOP_DOT;
        case TOK_STAR: return BINOP_MUL;
        case TOK_SLASH: return BINOP_DIV;
        case TOK_PLUS: return BINOP_ADD;
        case TOK_MINUS: return BINOP_SUB;
        case TOK_EQUAL: return BINOP_EQ;
        case TOK_NOT_EQUAL: return BINOP_NEQ;
        case TOK_LESS: return BINOP_LESS;
        case TOK_GREATER: return BINOP_GREATER;
        case TOK_LESS_EQUAL: return BINOP_LESS_EQUAL;
        case TOK_GREATER_EQUAL: return BINOP_GREATER_EQUAL;
        case TOK_LOGICAL_AND: return BINOP_LOGICAL_AND;
        case TOK_LOGICAL_OR: return BINOP_LOGICAL_OR;

        default: return BINOP_UNKNOWN;
    }
}
Ast_Type_Definition *Parser::parseTypeSpecifier() {

    Ast_Type_Definition *currentType = nullptr;

    while (true) {
        if (current->type == TOK_CARET) {
            advance();

            Ast_Type_Definition *pointerType = AST_NEW(Ast_Type_Definition);
            pointerType->pointed_to_type = nullptr;

            if (currentType) {
                pointerType->pointed_to_type = currentType;
            }
            currentType = pointerType;
        }
        else if (current->type == TOK_LBRACKET) {
            advance();

            Ast_Array_Type *arrayType = AST_NEW(Ast_Array_Type);
            arrayType->element_type = nullptr;

            if (current->type == TOK_DOUBLE_DOT) {
                advance();
                arrayType->is_resizable = true;
                arrayType->size_expr = nullptr;
                expect(TOK_RBRACKET, "Expected ']' after '..'");
            }
            else if (current->type == TOK_RBRACKET) { // assume abstract array are static array
                advance();
                arrayType->is_resizable = false;
                arrayType->size_expr = nullptr;
            }
            else {
                arrayType->is_resizable = false;
                arrayType->size_expr = parseExpression();
                expect(TOK_RBRACKET, "Expected ']' after array size");
            }

            if (currentType) {
                arrayType->element_type = currentType;
            }
            currentType = arrayType;
        }
        else {
            break;
        }
    }

    // Parse base type
    Ast_Type_Definition *baseType = nullptr;

    if (current->type == TOK_KEYWORD_ANY) {
        baseType = interp->type->type_def_any;
    }
    else if (strcmp(current->value, "int") == 0) baseType = interp->type->type_def_int;
    else if (strcmp(current->value, "s8") == 0) baseType = interp->type->type_def_s8;
    else if (strcmp(current->value, "s16") == 0) baseType = interp->type->type_def_s16;
    else if (strcmp(current->value, "s32") == 0) baseType = interp->type->type_def_s32;
    else if (strcmp(current->value, "s64") == 0) baseType = interp->type->type_def_s64;
    else if (strcmp(current->value, "u8") == 0) baseType = interp->type->type_def_u8;
    else if (strcmp(current->value, "u16") == 0) baseType = interp->type->type_def_u16;
    else if (strcmp(current->value, "u32") == 0) baseType = interp->type->type_def_u32;
    else if (strcmp(current->value, "u64") == 0) baseType = interp->type->type_def_u64;
    else if (strcmp(current->value, "float") == 0) baseType = interp->type->type_def_float;
    else if (strcmp(current->value, "float32") == 0) baseType = interp->type->type_def_float32;
    else if (strcmp(current->value, "float64") == 0) baseType = interp->type->type_def_float64;
    else if (strcmp(current->value, "void") == 0) baseType = interp->type->type_def_void;
    else if (strcmp(current->value, "bool") == 0) baseType = interp->type->type_def_bool;
    else if (strcmp(current->value, "string") == 0) {
        // interp->type->type_def_string = AST_NEW(Ast_Type_Definition);
        baseType = interp->type->type_def_string;
    }
    else if (current->type == TOK_IDENTIFIER) {
        Ast_Type_Definition *user_defined_type = AST_NEW(Ast_Type_Definition);
        user_defined_type->name = current->value;
        user_defined_type->is_unresolved = true;
        baseType = user_defined_type;
    }
    else {
        report_parse_error("Expected a base type");
        return nullptr;
    }
    advance();

    if (!currentType) {
        return baseType;
    }

    // Reverse the chain: attach base type to the end
    Ast_Type_Definition *result = baseType;
    Ast_Type_Definition *iter = currentType;

    while (iter) {
        if (iter->type == AST_ARRAY_TYPE) {
            Ast_Array_Type *arr = static_cast<Ast_Array_Type*>(iter);
            Ast_Type_Definition *next = arr->element_type;
            arr->element_type = result;
            result = arr;
            iter = next;
        }
        else { // pointer
            Ast_Type_Definition *next = iter->pointed_to_type;
            iter->pointed_to_type = result;
            result = iter;
            iter = next;
        }
    }

    return result;
}

// uint64_t total = 0; // @Temporary

Ast_Declaration *Parser::parseVarDeclaration()
{
    Ast_Declaration *varDecl = AST_NEW(Ast_Declaration);

    auto parseIdent = [&]() -> Ast_Ident* {
        if (current->type != TOK_IDENTIFIER && current->type != TOK_UNDERSCORE) {
            report_parse_error("Expected identifier in declaration");
            return nullptr;
        }
        Ast_Ident *ident = AST_NEW(Ast_Ident);
        if (current->type == TOK_UNDERSCORE) {
            ident->name = "_";
        } else {
            ident->name = current->value;
        }
        advance();
        return ident;
    };

    Ast_Ident *first = parseIdent();
    if (!first) return nullptr;

    // if there is no comma after first ident then its single identifier
    if(current->type != TOK_COMMA) {
        varDecl->identifier = first;
    } else { // otherwise push the first ident to identifiers' array instead
        varDecl->identifiers.push_back(first);
    }

    while (current->type == TOK_COMMA) {
        advance();
        Ast_Ident *ident = parseIdent();
        if (!ident) return nullptr;
        varDecl->identifiers.push_back(ident);
    }

    expect(TOK_COLON, "Expected ':' after identifiers in declaration");

    Ast_Type_Definition *typeDef = nullptr;
    Ast_Expression *initializer = nullptr;

    Ast_Comma_Separated_Args *initializers = nullptr;

    if(varDecl->identifiers.count != 0){
        if (current->type == TOK_ASSIGN) { // type not given
            advance();
            initializers = parseCommaSeparatedExpressions();
        } else {  // explicit type given
            typeDef = parseTypeSpecifier();

            if (current->type == TOK_ASSIGN) {
                advance();
                initializers = parseCommaSeparatedExpressions();
            }
        }
    } else {
        if (current->type == TOK_ASSIGN) {
            // its non inferred but initialized form
            advance();

            // uint64_t last_count = __rdtsc();

            initializer = parseExpression();

            // uint64_t end_count = __rdtsc();
            // uint64_t done = end_count - last_count;
            // total += done;
            // // printf("\nCycle count %lld \n", done);
            // printf("\nTotal Cycle count %lld \n", total);

        } else if(current->type != TOK_SEMICOLON) {
            // maybe its inferred form
            typeDef = parseTypeSpecifier();

            if(current->type == TOK_ASSIGN){
                // inferred and initialized form
                advance();
                initializer = parseExpression();
            }

        } else {
            report_parse_error("Expected either ':' declaration");
        }

    }

    if (initializers) {
        int identsCount = varDecl->identifiers.count;
        int initCount = initializers->arguments.count;

        if (initCount != 1 && initCount != identsCount) {
            report_parse_error("Number of initializers (%d) must match number of identifiers (%d) or be a single value",
                      initCount, identsCount);
        }
    }

    varDecl->declared_type = typeDef;
    varDecl->initializer = initializer;  // since they are initialized by default anyways we can just copy them
    varDecl->initializers = initializers;

    Expect(TOK_SEMICOLON, "Expected ';' after variable declaration");

    return varDecl;
}

Ast_Statement *Parser::parseMultipleAssignment() {
    Ast_Comma_Separated_Args *lhs = AST_NEW(Ast_Comma_Separated_Args);

    do {
        Ast_Expression *expr = parseExpression();
        lhs->arguments.push_back(expr);

        if (current->type == TOK_COMMA) {
            advance();
        } else {
            break;
        }
    } while (true);

    expect(TOK_ASSIGN, "Expected '=' in assignment");

    Ast_Comma_Separated_Args *rhs = parseCommaSeparatedExpressions();

    if (rhs->arguments.count != 1 && rhs->arguments.count != lhs->arguments.count) {
        report_parse_error("Number of expressions on right side (%d) must match left side (%d) or be a single value",
                  rhs->arguments.count, lhs->arguments.count);
    }

    Ast_Binary *assignExpr = AST_NEW(Ast_Binary);
    assignExpr->op = BINOP_ASSIGN;
    assignExpr->lhs = lhs;
    assignExpr->rhs = rhs;

    Ast_Statement *stmt = AST_NEW(Ast_Statement);
    stmt->expression = assignExpr;

    expect(TOK_SEMICOLON, "Expected ';' after assignment");
    return stmt;
}

Ast_If *Parser::parseIfStatement(){
    Ast_If *ifNode = AST_NEW(Ast_If);

    advance();
    bool should_consume_paren = false;
    if(current->type == TOK_LPAREN) {
        advance();
        should_consume_paren = true;
    }

    Ast_Expression *condition = parseExpression();

    if(should_consume_paren)
        expect(TOK_RPAREN, "Expected ')' after end of expression in if statement.");
    else if (current->type == TOK_RPAREN) {
        report_parse_error("Unexpected ')' after end of expression in if statement");
    }

    Ast_Block *thenBlock = parseBlockStatement(false, true);

    ifNode->condition = condition;
    ifNode->then_block = thenBlock;

    if(current->type == TOK_ELSE){
        advance();
        if(current->type == TOK_IF){
            ifNode->else_block = parseIfStatement();
        } else {
            Ast_Block *elseBlock = parseBlockStatement(false ,true);
            ifNode->else_block = elseBlock;
        }
    }
    return ifNode;

}

Ast_Block *Parser::parseBlockStatement(bool scoped_block, bool if_block) {

    bool only_one_stmt = false;
    bool should_close_paren = false;
    if(!if_block){
        expect(TOK_LCURLY_PAREN, "Expected '{' to start a block statement.");
        should_close_paren = true;
    } else {
        if(current->type != TOK_LCURLY_PAREN) {
            only_one_stmt = true;
        } else {
            should_close_paren = true;
            advance();
        }
    }
    Ast_Block *block = AST_NEW(Ast_Block);

    block->is_scoped_block = scoped_block;

    while (current->type != TOK_RCURLY_PAREN && current->type != TOK_END_OF_FILE) {
        Ast_Statement *stmt = parseStatement();
        if (stmt) {
            block->statements.push_back(stmt);
            if(if_block && only_one_stmt) return block; // we only allow one statement to get inside block and leave
        } else {
            report_parse_error("Failed to parse statement within block.");
            break;
        }
    }

    if(!if_block){
        expect(TOK_RCURLY_PAREN, "Expected '}' to close a block statement.");
    } else {
        if (current->type == TOK_RCURLY_PAREN && should_close_paren)
            advance();
        else
            report_parse_error("Unexpected '}' in a block statement");
    }

    desugarDefersInBlock(block);

    return block;
}

void Parser::desugarDefersInBlock(Ast_Block *block) {
    if (!block) return;
    Array<Ast_Statement*> normal_stmts(interp->pool);
    Array<Ast_Block*> deferred(interp->pool);
    for (long i = 0; i < block->statements.count; ++i) {
        Ast_Statement *s = block->statements.data[i];
        if (s && s->type == AST_DEFER) {
            Ast_Defer *d = static_cast<Ast_Defer *>(s);
            if (d->block) {
                deferred.push_back(d->block);
            }
        } else if (s) {
            normal_stmts.push_back(s);
        }
    }
    block->statements.count = 0;
    for (long i = 0; i < normal_stmts.count; ++i) {
        block->statements.push_back(normal_stmts.data[i]);
    }
    for (long i = deferred.count - 1; i >= 0; --i) {
        Ast_Block *db = deferred.data[i];
        for (long j = 0; j < db->statements.count; ++j) {
            block->statements.push_back(db->statements.data[j]);
        }
    }
}

Ast_Comma_Separated_Args *Parser::parseCommaSeparatedExpressions() {
    Ast_Comma_Separated_Args *args = AST_NEW(Ast_Comma_Separated_Args);

    args->arguments.push_back(parseExpression());

    while (current->type == TOK_COMMA) {
        advance();
        args->arguments.push_back(parseExpression());
    }

    return args;
}

Ast_Procedure_Call_Expression *Parser::parseCall()
{
    Token *identToken = current;

    Ast_Procedure_Call_Expression *callExpr = AST_NEW(Ast_Procedure_Call_Expression);
    callExpr->function = AST_NEW(Ast_Ident);
    callExpr->function->name = identToken->value;

    advance();

    expect(TOK_LPAREN, "Expected '(' after function name");


    Ast_Comma_Separated_Args *argsNode = AST_NEW(Ast_Comma_Separated_Args);

    bool saw_named_argument = false;

    while(current->type != TOK_RPAREN && current->type != TOK_END_OF_FILE)
    {
        Ast_Expression *arg = nullptr;

        if (current->type == TOK_IDENTIFIER && lexer->peekNextToken()->type == TOK_ASSIGN)
        {
            Ast_Named_Argument *named_arg = AST_NEW(Ast_Named_Argument);

            named_arg->name = AST_NEW(Ast_Ident);
            named_arg->name->name = current->value;

            advance(); // consume ident
            advance();  // consume = sign

            named_arg->value = parseExpression();

            if (!named_arg->value) {
                report_parse_error("Expected an expression after '=' for named argument '%s'.", named_arg->name->name);
                return nullptr;
            }

            arg = named_arg;
            saw_named_argument = true;
        }
        else
        {
            // Valid:   func(10, b=5)
            // Invalid: func(a=10, 5)
            if (saw_named_argument) {
                report_parse_error("Positional argument cannot appear after a named argument.");
                return nullptr;
            }

            arg = parseExpression();

            if (!arg) {
                report_parse_error("Expected function argument.");
                return nullptr;
            }
        }

        argsNode->arguments.push_back(arg);

        if(current->type == TOK_COMMA){
            advance();

            if(current->type == TOK_RPAREN)
            {
                report_parse_error("Expected argument after ',' in function call");
                break;
            }

        } else {
            break; // at this point is probably a ')'
        }
    }
    expect(TOK_RPAREN, "Expected ')' after function call arguments");

    callExpr->arguments = argsNode;
    return callExpr;

}

Ast_Statement *Parser::parseStructDefinition()
{
    Ast_Struct *struct_decs = AST_NEW(Ast_Struct);

    struct_decs->name = current->value;

    Ast_Statement *stmt = AST_NEW(Ast_Statement);
    auto *td = AST_NEW(Ast_Type_Definition);
    td->struct_def = struct_decs;
    stmt->type_definition = td;

    advance();
    expect(TOK_DOUBLECOLON, "Expected '::' after struct name");

    advance(); // consume struct keyword

    expect(TOK_LCURLY_PAREN, "Expected '{' after struct keyword.");

    while(current->type == TOK_IDENTIFIER) {
        Ast_Declaration *decl = parseVarDeclaration();
        struct_decs->members.push_back(decl);
    }

    expect(TOK_RCURLY_PAREN, "Expected '}' after struct description.");

    if(current->type == TOK_SEMICOLON) // we dont wanna force struct definition to end with semicolon!
        advance();

    stmt->expression = static_cast<Ast_Expression *> (struct_decs);
    return stmt;
}

Ast_Declaration *Parser::parseFunctionDeclaration(bool is_local) {
    if (current->type != TOK_IDENTIFIER) {
        report_parse_error("Expected function name at start of declaration");
        return nullptr;
    }

    Ast_Declaration *func_decl = AST_NEW(Ast_Declaration);
    func_decl->identifier = AST_NEW(Ast_Ident);
    func_decl->identifier->name = current->value;
    func_decl->is_function = true;
    func_decl->is_local_function = is_local;
    advance();

    expect(TOK_DOUBLECOLON, "Expected '::' after function name");

    expect(TOK_LPAREN, "Expected '(' to start parameter list");

    if (current->type != TOK_RPAREN) {
        while (true) {
            if (current->type != TOK_IDENTIFIER) {
                report_parse_error("Expected parameter name");
                return nullptr;
            }

            Ast_Declaration *param = AST_NEW(Ast_Declaration);
            param->identifier = AST_NEW(Ast_Ident);
            param->identifier->name = current->value;
            param->is_declaration_function_argument = true;
            advance(); // consume param name

            expect(TOK_COLON, "Expected ':' after parameter name");

            param->declared_type = parseTypeSpecifier();
            if (!param->declared_type) {
                report_parse_error("Invalid parameter type");
                return nullptr;
            }

            func_decl->parameters.push_back(param);

            if(current->type == TOK_ASSIGN){
                advance();

                param->initializer = parseExpression();
            }

            if (current->type != TOK_COMMA)
                break;
            advance(); // consume comma
        }
    }

    expect(TOK_RPAREN, "Expected ')' after parameter list");

    if (current->type == TOK_ARROW) {
        advance();
        // multiple return types: foo :: () -> int, bool
        // turned into struct on C side
        func_decl->return_types.push_back(parseTypeSpecifier());
        while (current->type == TOK_COMMA) {
            advance();
            func_decl->return_types.push_back(parseTypeSpecifier());
        }
        if (func_decl->return_types.count == 1) {
            func_decl->return_type = func_decl->return_types.data[0];
        } else if (func_decl->return_types.count > 1) {
            func_decl->return_type = nullptr; // multi, use return_types
        }
    } else {
        func_decl->return_type = AST_NEW(Ast_Type_Definition);
        func_decl->return_type = interp->type->type_def_void;
    }


    if (current->type == TOK_LCURLY_PAREN) {
        func_decl->is_function_body = true;
        func_decl->my_scope = parseBlockStatement(); // parse the body as a block
    } else {
        // func_decl->is_function_header = true;
        if (current->type == TOK_HASHTAG) {
            Token *next = lexer->peekNextToken();
            if (next->type == TOK_FOREIGN) {
                advance();
                advance();
                func_decl->is_foreign = true;
                func_decl->is_function_header = true;
            }
            expect(TOK_SEMICOLON, "Expected ';' after function prototype");
        }
        else if (current->type == TOK_SEMICOLON){
            report_parse_error("Function header only allowed for foreign function calls.");
        }
        else {
            Expect(TOK_LCURLY_PAREN, "Expected '{' after return value");

        }
    }

    return func_decl;
}

Token *Parser::peek_after_lhs() {
    int offset = 1;
    Token *t = lexer->peekNextToken(offset);

    while (t) {
        if (t->type == TOK_DOT) {
            Token *afterDot = lexer->peekNextToken(offset + 1);
            if (!afterDot || afterDot->type != TOK_IDENTIFIER) break;
            offset += 2;
        }
        else if (t->type == TOK_LBRACKET) {
            // skip array index expression
            offset++; // skip '['
            int depth = 1;
            while (depth > 0) {
                Token *inner = lexer->peekNextToken(offset);
                if (!inner) return nullptr;
                if (inner->type == TOK_LBRACKET) depth++;
                else if (inner->type == TOK_RBRACKET) depth--;
                offset++;
            }
        }
        else {
            break;
        }

        t = lexer->peekNextToken(offset);
    }

    return t;
}

Ast_Statement *Parser::parseStatement()
{

    switch (current->type) {
        case TOK_IDENTIFIER:
        case TOK_UNDERSCORE: {
            bool is_underscore = (current->type == TOK_UNDERSCORE);
            Token *next = lexer->peekNextToken();

            if (is_underscore && next->type != TOK_COMMA) {
                report_parse_error("Unexpected use of _");
                return nullptr;
            }

            if (next->type == TOK_DOUBLECOLON){
                if (is_underscore) {
                    report_parse_error("Unexpected use of _");
                    return nullptr;
                }
                Token *lookahead = lexer->peekNextToken(2);
                if(lookahead->type == TOK_STRUCT){
                    return parseStructDefinition();
                }
                else {
                    // parse function def
                    return parseFunctionDeclaration(/*is_local=*/false);
                }
            }
            else if (next->type == TOK_COLON){
                if (is_underscore) {
                    report_parse_error("Unexpected use of _ in declaration");
                    return nullptr;
                }
                Ast_Declaration *decl = parseVarDeclaration();
                return decl;
            }
            else if(next->type == TOK_LPAREN){
                if (is_underscore) {
                    report_parse_error("Unexpected use of _");
                    return nullptr;
                }
                Ast_Procedure_Call_Expression *expr = parseCall();

                Expect(TOK_SEMICOLON, "Expected ';' after procedure call.");

                Ast_Statement *stmt = AST_NEW(Ast_Statement);
                stmt->expression = expr;
                return stmt;
            }
            else if (next->type == TOK_COMMA) {
                int offset = 2;
                Token *t = lexer->peekNextToken(offset);

                while (t && (t->type == TOK_COMMA || t->type == TOK_IDENTIFIER || t->type == TOK_UNDERSCORE)) {
                    if (t->type == TOK_COMMA) {
                        offset++;
                        t = lexer->peekNextToken(offset);
                    } else {
                        offset++;
                        t = lexer->peekNextToken(offset);
                    }
                }

                if (t && t->type == TOK_COLON) {
                    return parseVarDeclaration();
                } else {
                    return parseMultipleAssignment();
                }
            }

            // Check if after a full first lhs expr (e.g. a[0]) there is comma, then multi lhs assign
            Token *t = peek_after_lhs();
            if (t && t->type == TOK_COMMA) {
                return parseMultipleAssignment();
            }

            if (t && t->type == TOK_ASSIGN) {
                if (is_underscore) {
                    report_parse_error("Unexpected use of _");
                    return nullptr;
                }
                Ast_Expression *lhs = parseExpression();
                Expect(TOK_ASSIGN, "Expected '=' in assignment");
                Ast_Expression *rhs = parseExpression();

                Ast_Binary *assignExpr = AST_NEW(Ast_Binary);
                assignExpr->op = BINOP_ASSIGN;
                assignExpr->lhs = lhs;
                assignExpr->rhs = rhs;

                Ast_Statement *stmt = AST_NEW(Ast_Statement);
                Expect(TOK_SEMICOLON, "Expected ';' after assignment");

                stmt->expression = assignExpr;
                return stmt;
            }

            report_parse_error("This a fucked up statement." );
            return nullptr;
        }

        case TOK_DEFER: {
            advance(); // consume 'defer'
            // support both "defer { ... }" and "defer stmt;" (like if/while bodies)
            Ast_Block *dblock = parseBlockStatement(false, true);
            Ast_Defer *d = AST_NEW(Ast_Defer);
            d->block = dblock;
            return d;
        }

        case TOK_RETURN: {
            advance(); // consume 'return'

            Ast_Statement *stmt = AST_NEW(Ast_Statement);
            stmt->is_return = true;

            if (current->type != TOK_SEMICOLON) {

                // BAD HACK, WTF EVEN IS THIS DUDE
                stmt->expression = parseCommaSeparatedExpressions();
                auto *expr = static_cast<Ast_Comma_Separated_Args *>(stmt->expression);
                if(expr->arguments.count == 1){
                    stmt->expression = expr->arguments.data[0];
                }
            }

            expect(TOK_SEMICOLON, "Expected ';' after return statement.");
            return stmt;
        }
        case TOK_STAR: // fallthrough
        case TOK_CARET:  // fallthrough
        case TOK_AMPERSAND: {
            // these can be in front of statement
            Ast_Expression *lhs = parseExpression(); // could be *p, ^x, &y

            Expect(TOK_ASSIGN, "Expected '=' in pointer assignment.");

            Ast_Expression *rhs = parseExpression();

            Ast_Binary *assignExpr = AST_NEW(Ast_Binary);
            assignExpr->op = BINOP_ASSIGN;
            assignExpr->lhs = lhs;
            assignExpr->rhs = rhs;

            Expect(TOK_SEMICOLON, "Expected ';' after assignment.");

            Ast_Statement *stmt = AST_NEW(Ast_Statement);
            stmt->expression = assignExpr;
            return stmt;
        }
        case TOK_PRINT: {
            Ast_Procedure_Call_Expression *expr = parseCall();

            Expect(TOK_SEMICOLON, "Expected ';' after printf call.");

            Ast_Statement *stmt = AST_NEW(Ast_Statement);
            stmt->expression = expr;
            return stmt;
        }
        case TOK_IF:
            return parseIfStatement();
        case TOK_ELSE:
        {
            report_parse_error("Got 'else' without an 'if' statement.");
            advance();
            // break;
            return nullptr;
        }
        case TOK_WHILE:{
            Ast_While *_while = AST_NEW(Ast_While);
            advance();
            bool should_consume_paren = false;
            if(current->type == TOK_LPAREN) {
                advance();
                should_consume_paren = true;
            }

            Ast_Expression *expr = parseExpression();

            if(should_consume_paren)
                expect(TOK_RPAREN, "Expected ')' after while condition.");

            Ast_Block *block = parseBlockStatement(false, true);
            _while->condition = expr;
            _while->block = block;
            return _while;
        }
        case TOK_FOR: {
            Ast_For *forNode = AST_NEW(Ast_For);
            advance(); // consume 'for'

            bool should_consume_paren = false;
            if(current->type == TOK_LPAREN) {
                advance();
                should_consume_paren = true;
            }

            if (current->type != TOK_IDENTIFIER) {
                report_parse_error("Expected identifier after 'for'");
                return nullptr;
            }
            Ast_Ident *var = AST_NEW(Ast_Ident);
            var->name = current->value;
            forNode->variable = var;
            advance();

            expect(TOK_COLON, "Expected ':' after for loop variable");

            Ast_Expression *it = parseExpression();
            if (current->type == TOK_DOUBLE_DOT) {
                advance();
                forNode->start = it;
                forNode->end = parseExpression();
            } else {
                forNode->array = it;
            }
            if(should_consume_paren)
                expect(TOK_RPAREN, "Expected ')' after for expression.");

            Ast_Block *block = parseBlockStatement(false, true);
            forNode->block = block;
            // append a synthetic declaration for the loop variable so normal
            // resolution/declaration processing sees it (type inferred later).
            // C_Converter will skip re-emitting it inside the for.
            if (forNode->block && forNode->variable) {
                Ast_Declaration *loopd = AST_NEW(Ast_Declaration);
                loopd->identifier = forNode->variable;
                // prepend so that lookup during block walk finds it before uses
                Array<Ast_Statement*> newstmts(forNode->block->statements.pool ? forNode->block->statements.pool : interp->pool);
                newstmts.push_back(static_cast<Ast_Statement*>(loopd));
                for (long k = 0; k < forNode->block->statements.count; k++) {
                    newstmts.push_back(forNode->block->statements.data[k]);
                }
                forNode->block->statements.data = newstmts.data;
                forNode->block->statements.count = newstmts.count;
                forNode->block->statements.capacity = newstmts.capacity;
            }
            return forNode;
        }
        case TOK_LCURLY_PAREN: {
            bool is_scoped_block = true;
            Ast_Block *scopedBlock = parseBlockStatement(is_scoped_block);
            Ast_Statement *stmt = AST_NEW(Ast_Statement);
            stmt->block = scopedBlock;
            return stmt;
        }

        case TOK_NUMBER: // fallthrough
        case TOK_STRING: // fallthrough
        case TOK_FLOAT: {
            Ast_Expression *expr = parseExpression();
            expect(TOK_SEMICOLON, "Expected ';' after expression statement.");
            Ast_Statement *stmt = AST_NEW(Ast_Statement);
            stmt->expression = expr;
            return stmt;
        }
        case TOK_BREAK: {
            Ast_Break *br = AST_NEW(Ast_Break);
            advance();
            expect(TOK_SEMICOLON, "Expected ';' after break statement.");
            return br;

        }
        // case TOK_UNDERSCORE:
            // printf("Here is underscore");
            // return NULL;
        default:
            report_parse_error("Unexpected token at start of statement: " );
            return nullptr;
    }
}


Ast_Block *Parser::parseProgram(Ast_Block *program, bool skip_main)
{

    // printf("---Inside parser--- Parsing file: %s\n", interp->current_file);

    // printf("size of Ast_Type_Definition %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Ast_Type_Definition));

    // printf("size of Ast %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Ast));
     // printf("size of Token %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Token));
    // printf("size of Ast_Ident %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Ast_Ident));
    // printf("size of Ast_Procedure_Call_Expression %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Ast_Procedure_Call_Expression));

    bool mainFound = false;

    while (current->type != TOK_END_OF_FILE)
    {
        if (current->type == TOK_MAIN_ENTRY_POINT) {
            if(skip_main == true){
                report_parse_error("'main' entry point should not be inside a module.");
            }

            if (mainFound) {
                report_parse_error("Multiple 'main' functions not allowed.");
            }
            mainFound = true;
            Ast_Statement *stmt = AST_NEW(Ast_Statement);

            advance();

            expect(TOK_DOUBLECOLON, "Expected '::' after main entry point."); // fix when not expected
            expect(TOK_LPAREN, "Expected '(' after main entry point.");
            expect(TOK_RPAREN, "Expected ')' after main entry point.");

            Ast_Block *mainBlock = parseBlockStatement();
            mainBlock->is_entry_point = true;  // simple flag

            stmt->block = mainBlock;
            program->statements.push_back(stmt);
        }
        else if (current->type == TOK_IDENTIFIER) {

            Token *next = lexer->peekNextToken();
            if (next->type == TOK_COLON) {
                Ast_Declaration *decl = parseVarDeclaration();
                program->statements.push_back(static_cast<Ast_Statement*>(decl));
            }
            else if(next->type == TOK_COMMA) {
                int offset = 2;
                Token *t = lexer->peekNextToken(offset);

                while (t && (t->type == TOK_COMMA || t->type == TOK_IDENTIFIER || t->type == TOK_UNDERSCORE)) {
                    if (t->type == TOK_COMMA) {
                        offset++;
                        t = lexer->peekNextToken(offset);
                    } else {
                        offset++;
                        t = lexer->peekNextToken(offset);
                    }
                }

                if (t && t->type == TOK_COLON) {
                    auto *pv = parseVarDeclaration();
                    program->statements.push_back(pv);
                }
            }
            else if(next->type == TOK_DOUBLECOLON) {

                Token *n = lexer->peekNextToken(2);
                if(n->type == TOK_STRUCT){
                    program->statements.push_back(parseStatement());
                }
                else {

                    // Ast_Statement *stmt = parseStatement();
                    // **function declaration or definition**
                    Ast_Declaration *funcDecl = parseFunctionDeclaration(/*is_local=*/false);
                    program->statements.push_back(static_cast<Ast_Statement*>(funcDecl));
                }
            }
            else {
                // report_parse_error("Top-level executable statements not allowed. Only declarations and main.");
                exitSuccess = false;
                break;
            }
        } else if (current->type == TOK_HASHTAG){
            Token *next = lexer->peekNextToken();
            if(next->type == TOK_IMPORT){
                advance();
                advance();
                Ast_Import *import = AST_NEW(Ast_Import);
                if(current->type == TOK_STRING){
                    import->import_path = (const char *)current->string_value.data;
                    // printf("%.*s\n",(int)current->string_value.count, current->string_value.data);
                    program->imports.push_back(import);
                    advance();

                    if(current->type == TOK_SEMICOLON){
                        advance();
                    }

                }
                else {
                    report_parse_error("Expected import string.");
                }
            }
            else report_parse_error("Only import are supported following a '#'\n");
        }
        else {
            report_parse_error("Unexpected token at top-level. Only declarations and main function allowed.");
            break;
        }
    }


    if (!mainFound && exitSuccess && !skip_main) {
        report_parse_error("No 'main' entry point was found in the program.", true);
    }

    if(!exitSuccess){
        interp->had_errors = true;

        sort_errors(interp->errors);

        if (!interp->cli->verbose) { // if no verbose then print the first error and exit
            if (interp->errors.count > 0) {
                BufferedError *first = interp->errors.data[0];
                fprintf(stderr, "%s[%d:%d]: %s\n", first->file, first->row, first->col, first->message);
                interp->print_error_source(first->file, first->row, first->col);
            }
            printf("Errors in parser. Exiting.\n");
            exit(1);
        }

        interp->flush_errors();
        printf("Errors in parser. Exiting.\n");
        exit(1);
    }
    return program;
}

