#include "parser.h"
#include "token.h"

#include <iostream>
#include <cstring>
bool exitSuccess = true;

#ifdef _DEBUG
#define AST_NEW_LOG(type) \
   printf("-----------------------------------used AST_NEW for [%d %d] %s---%zu---\n", current->row, current->col, typeid(type).name(), sizeof(type));
#else
#define AST_NEW_LOG(type)
#endif

// MACRO
#define AST_NEW(pool, type) ([&]() -> type* {              \
    AST_NEW_LOG(type)                                      \
    assert(pool != nullptr && "Pool must not be null");    \
    void *mem = pool_alloc(pool, sizeof(type));            \
    type *node = new (mem) type(pool);                     \
    node->line_number = current->row;                      \
    node->character_number = current->col;                 \
    return node;                                           \
}())


Parser::Parser(Lexer *l, Pool *p, Def_Type *type) : lexer(l), pool(p) {
    current = lexer->nextToken();
    _type = type;
}

void Parser::advance() {
    previous = current;
    current = lexer->nextToken();
}

void Parser::parseError(const char *message) {
    printf("\nParsing Error[%d:%d] %s", current->row, current->col, message);
    exitSuccess = false;
    synchronize();

}

void Parser::expect(TokenType expectedType, const char *errorMessage)
{
    if (current->type != expectedType) {
        parseError(errorMessage);
        exitSuccess = false;
        synchronize();
        return;
    }
    advance();
}

void Parser::Expect(TokenType expectedType, const char *errorMessage)
{
    if (current->type != expectedType) {
        printf("\nParsing Error[%d:%d] %s", previous->row, previous->col, errorMessage);

        exitSuccess = false;
        synchronize();
        return;
    }
    advance();
}


void Parser::synchronize()
{

    while (current->type != TOK_END_OF_FILE) {
        if (previous->type == TOK_SEMICOLON) return;
        switch (current->type) {
            case TOK_IF:
            case TOK_PRINT:
            case TOK_IDENTIFIER:
            case TOK_MAIN_ENTRY_POINT:
            case TOK_LCURLY_PAREN:
                return;
        }
        advance();
    }
}

// in an expression
Ast_Expression *Parser::parseFactor()
{
    // Handle unary operators first: *, &
    if (current->type == TOK_STAR) { // dereference
        advance();
        Ast_Unary *node = AST_NEW(pool,Ast_Unary);
        node->op = UNARY_DEREFERENCE;
        node->operand = parseFactor();
        return node;
    }
    else if (current->type == TOK_AMPERSAND) { // address of
        advance();
        Ast_Unary *node = AST_NEW(pool,Ast_Unary);
        node->op = UNARY_ADDRESS_OF;
        node->operand = parseFactor();
        return node;
    }
    else if (current->type == TOK_MINUS) {
        Token *lookahead = lexer->peekNextToken();
        if (lookahead->type == TOK_MINUS) {
            parseError("Consecutive unary minus operators are not allowed.");
        }
        advance();
        Ast_Unary *node = AST_NEW(pool, Ast_Unary);
        node->op = UNARY_NEGATE;
        node->operand = parseFactor();
        return node;
    }
    else if (current->type == TOK_EXCLAMATION_MARK) {
        Token *lookahead = lexer->peekNextToken();
        if (lookahead->type == TOK_EXCLAMATION_MARK) {
            parseError("Consecutive unary exclamation mark operators are not allowed.");
        }
        advance();
        Ast_Unary *node = AST_NEW(pool, Ast_Unary);
        node->op = UNARY_NOT;
        node->operand = parseFactor();
        return node;
    }
    else if (current->type == TOK_NUMBER)
    {

        Ast_Literal *node = AST_NEW(pool,Ast_Literal);
        node->value_type = LITERAL_NUMBER;
        node->integer_value = current->int_value;
        advance();
        return node;
    }
    else if (current->type == TOK_FLOAT)
    {

        Ast_Literal *node = AST_NEW(pool,Ast_Literal);
        node->value_type = LITERAL_FLOAT;
        node->float_value = current->float32_value;
        advance();
        return node;
    }
    else if (current->type == TOK_STRING )
    {
        Ast_Literal *node = AST_NEW(pool,Ast_Literal);
        node->value_type = LITERAL_STRING;
        node->string_value = reinterpret_cast<const char*>(current->string_value.data);

        advance();
        return node;
    }
    else if (current->type == TOK_KEYWORD_TRUE || current->type == TOK_KEYWORD_FALSE)
    {
        Ast_Literal *node = AST_NEW(pool,Ast_Literal);
        if(current->type == TOK_KEYWORD_TRUE){
            node->value_type = LITERAL_TRUE;
        } else node->value_type = LITERAL_FALSE;

        advance();
        return node;
    }
    else if (current->type == TOK_IDENTIFIER)
    {
        Token *lookahead = lexer->peekNextToken();
        if(lookahead->type == TOK_LPAREN){
            return parseCall();
        } else {

            Ast_Ident *node = AST_NEW(pool,Ast_Ident);
            node->name = current->value;

            advance();
            return node;
        }
    }
    else if (current->type == TOK_LPAREN)
    {
        advance();
        Ast_Expression *expr = parseExpression();

        Expect(TOK_RPAREN, "Expected ')' after expression in parentheses.");

        return expr;
    }

    parseError("Expected a literal, identifier, or parenthesised expression factor.");

    return nullptr;
}

Ast_Expression *Parser::parseTerm()
{
    Ast_Expression *left = parseFactor();

    while (current->type == TOK_STAR || current->type == TOK_SLASH)
    {
        Ast_Binary *node = AST_NEW(pool,Ast_Binary);
        node->lhs = left;

        if(current->type == TOK_STAR) node->op = BINOP_MUL;
        else if (current->type == TOK_SLASH) node->op = BINOP_DIV;

        advance();
        node->rhs = parseFactor();

        left = node;
    }
    return left;
}

//  add, subtract, compare operations etc
Ast_Expression *Parser::parseExpression()
{
    Ast_Expression *left = parseTerm();

    while (current->type == TOK_PLUS || current->type == TOK_MINUS ||
           current->type == TOK_EQUAL || current->type == TOK_NOT_EQUAL ||
           current->type == TOK_LESS || current->type == TOK_GREATER ||
           current->type == TOK_LESS_EQUAL || current->type == TOK_GREATER_EQUAL) {

        Ast_Binary *node = AST_NEW(pool,Ast_Binary);
        node->lhs = left;

        switch(current->type){
            case TOK_PLUS: node->op = BINOP_ADD; break;
            case TOK_MINUS: node->op = BINOP_SUB; break;
            case TOK_EQUAL: node->op = BINOP_EQ; break;
            case TOK_NOT_EQUAL: node->op = BINOP_NEQ; break;
            // rest still not done
            default: break;
        }

        advance();
        node->rhs = parseTerm();
        left = node;
    }
    return left;
}


Ast_Type_Definition *Parser::parseTypeSpecifier() {

    Ast_Type_Definition *currentType = nullptr;

    while (true) {

        if (current->type == TOK_CARET) {

            Ast_Type_Definition *pointerType = AST_NEW(pool, Ast_Type_Definition);
            pointerType->pointed_to_type = _type->type_def_dummy;

            if (currentType) {
                pointerType->pointed_to_type = currentType;
            }

            currentType = pointerType;
            advance();
        }
        else if (current->type == TOK_LBRACKET) {
            // Array type
            advance();

            Ast_Type_Definition *arrayType = AST_NEW(pool,Ast_Type_Definition);
            arrayType->element_type = nullptr;

            if (current->type == TOK_NUMBER) {
                // Static array
                arrayType->array_kind = ARRAY_STATIC;
                arrayType->static_array_size = (int)current->int_value;
                advance(); // consume size
                expect(TOK_RBRACKET, "Expected ']' after static array size.");
            }
            else { // we dont have support for dynamic arrays yet
                parseError("Expected either ']' or a number inside array brackets.");
                return nullptr;
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

    Ast_Type_Definition *baseType = nullptr;


    // Temporary replace this with hashmap later
    if (strcmp(current->value, "int") == 0) baseType = _type->type_def_int;
    else if (strcmp(current->value, "s8") == 0) baseType = _type->type_def_s8;
    else if (strcmp(current->value, "s16") == 0) baseType = _type->type_def_s16;
    else if (strcmp(current->value, "s32") == 0) baseType = _type->type_def_s32;
    else if (strcmp(current->value, "s64") == 0) baseType = _type->type_def_s64;
    else if (strcmp(current->value, "u8") == 0) baseType = _type->type_def_u8;
    else if (strcmp(current->value, "u16") == 0) baseType = _type->type_def_u16;
    else if (strcmp(current->value, "u32") == 0) baseType = _type->type_def_u32;
    else if (strcmp(current->value, "u64") == 0) baseType = _type->type_def_u64;
    else if (strcmp(current->value, "float") == 0) baseType = _type->type_def_float;
    else if (strcmp(current->value, "float32") == 0) baseType = _type->type_def_float32;
    else if (strcmp(current->value, "float64") == 0) baseType = _type->type_def_float64;
    else if (strcmp(current->value, "void") == 0) baseType = _type->type_def_void;
    else if (strcmp(current->value, "bool") == 0) baseType = _type->type_def_bool;
    else if (strcmp(current->value, "string") == 0) baseType = _type->type_def_string;
    else {
        parseError("Expected a base type (e.g 'int', 'float', 'string', 'bool')");
        synchronize();
    return nullptr;
    }

    advance();

    if (!currentType) // it was a pure type
        return baseType;


    Ast_Type_Definition *iter = currentType;
    while (true) {
        if (iter->array_kind == ARRAY_NONE && iter->pointed_to_type == _type->type_def_dummy /*nullptr*/) {
            iter->pointed_to_type = baseType;
            break;
        }
        else if (iter->array_kind != ARRAY_NONE && iter->element_type == nullptr) {
            iter->element_type = baseType;
            break;
        }

        if (iter->pointed_to_type)
            iter = iter->pointed_to_type;
        else if (iter->element_type)
            iter = iter->element_type;
        else
            break;
    }

    return currentType;
}


// statement
Ast_Declaration *Parser::parseVarDeclaration()
{
    Ast_Declaration *varDecl = AST_NEW(pool,Ast_Declaration);

    const char *varName = current->value;
    advance();

    Ast_Type_Definition *typeDef = nullptr;

    Ast_Expression *initializer = nullptr;

    if(current->type == TOK_COLON){
        advance();

        if (current->type == TOK_ASSIGN) {
            // its non inferred but initialized form
            advance();
            initializer = parseExpression();
        } else if(current->type != TOK_SEMICOLON) {
            // maybe its inferred form
            typeDef = parseTypeSpecifier();

            if(current->type == TOK_ASSIGN){
                // inferred and initialized form
                advance();
                initializer = parseExpression();
            }

        } else {
            parseError("Expected either ':' declaration");
        }

    } else {
        parseError("Must be a ':' after identifier in a declaration statement");
    }

    Expect(TOK_SEMICOLON, "Expected ';' after variable declaration.");

    varDecl->declared_type = typeDef;
    varDecl->identifier = AST_NEW(pool,Ast_Ident);
    varDecl->identifier->name = varName;
    varDecl->initializer = initializer;

    return varDecl;
}


Ast_If *Parser::parseIfStatement(){

    advance();
    Expect(TOK_LPAREN, "Expected '(' before start of expression in if statement .");
    Ast_Expression *condition = parseExpression();
    Expect(TOK_RPAREN, "Expected ')' after end of expression in if statement.");

    Ast_Block *thenBlock = parseBlockStatement();

    Ast_If *ifNode = AST_NEW(pool,Ast_If);

    ifNode->condition = condition;
    ifNode->then_block = thenBlock;

    if(current->type == TOK_ELSE){
        advance();
        Ast_Block *elseBlock = parseBlockStatement();
        ifNode->else_block = elseBlock;
    }

    return ifNode;

}

Ast_Block *Parser::parseBlockStatement(bool scoped_block) {
    expect(TOK_LCURLY_PAREN, "Expected '{' to start a block statement.");

    Ast_Block *block = AST_NEW(pool,Ast_Block);

    block->is_scoped_block = scoped_block;

    while (current->type != TOK_RCURLY_PAREN && current->type != TOK_END_OF_FILE) {
        Ast_Statement *stmt = parseStatement();
        if (stmt) block->statements.push_back(stmt);
        else {
            parseError("Failed to parse statement within block.");
            exitSuccess = false;
            synchronize();
            break;
        }
    }
    expect(TOK_RCURLY_PAREN, "Expected '}' to close a block statement.");

    return block;
}


Ast_Procedure_Call_Expression *Parser::parseCall()
{
    Token *identToken = current;

    Ast_Procedure_Call_Expression *callExpr = AST_NEW(pool,Ast_Procedure_Call_Expression);
    callExpr->function = AST_NEW(pool,Ast_Ident);
    callExpr->function->name = identToken->value;

    advance();

    expect(TOK_LPAREN, "Expected '(' after function name");


    Ast_Comma_Separated_Args *argsNode = AST_NEW(pool,Ast_Comma_Separated_Args);

    if(current->type != TOK_RPAREN)
    {
        while(true)
        {
            Ast_Expression *arg = parseExpression();
            argsNode->arguments.push_back(arg);

            if(current->type == TOK_COMMA){
                advance();

                if(current->type == TOK_RPAREN)
                {
                    parseError("Expected argument after ',' in function call");
                    exitSuccess = false;
                    synchronize();
                    break;
                }

            } else {
                break; // at this point is probably a ')'
            }
        }
    }
    expect(TOK_RPAREN, "Expected ')' after function call arguments");

    callExpr->arguments = argsNode;
    return callExpr;

}

// Ast_Struct_Description *Parser::parseStructDefinition()
// {
//     char *structName = current->value;
//     advance();
//     advance();
//     advance();
// }

Ast_Declaration* Parser::parseFunctionDeclaration(bool is_local) {
    if (current->type != TOK_IDENTIFIER) {
        parseError("Expected function name at start of declaration");
        return nullptr;
    }

    Ast_Declaration* func_decl = AST_NEW(pool, Ast_Declaration);
    func_decl->identifier = AST_NEW(pool, Ast_Ident);
    func_decl->identifier->name = current->value;
    func_decl->is_function = true;
    func_decl->is_local_function = is_local;
    advance();

    expect(TOK_DOUBLECOLON, "Expected '::' after function name");

    expect(TOK_LPAREN, "Expected '(' to start parameter list");

    if (current->type != TOK_RPAREN) {
        while (true) {
            if (current->type != TOK_IDENTIFIER) {
                parseError("Expected parameter name");
                return nullptr;
            }

            Ast_Declaration* param = AST_NEW(pool, Ast_Declaration);
            param->identifier = AST_NEW(pool, Ast_Ident);
            param->identifier->name = current->value;
            advance(); // consume param name

            expect(TOK_COLON, "Expected ':' after parameter name");

            param->declared_type = parseTypeSpecifier();
            if (!param->declared_type) {
                parseError("Invalid parameter type");
                return nullptr;
            }

            func_decl->parameters.push_back(param);

            if (current->type != TOK_COMMA)
                break;
            advance(); // consume comma
        }
    }

    expect(TOK_RPAREN, "Expected ')' after parameter list");

    if (current->type == TOK_ARROW) {
        advance();
        func_decl->return_type = parseTypeSpecifier();
    } else {
        func_decl->return_type = AST_NEW(pool, Ast_Type_Definition);

        func_decl->return_type = _type->type_def_void;

    }


    if (current->type == TOK_LCURLY_PAREN) {
        func_decl->is_function_body = true;
        func_decl->my_scope = parseBlockStatement(); // parse the body as a block
    } else {
        func_decl->is_function_header = true;
        expect(TOK_SEMICOLON, "Expected ';' after function prototype");
    }

    return func_decl;
}


Ast_Statement *Parser::parseStatement()
{

    switch (current->type) {
        case TOK_IDENTIFIER: {
            Token *next = lexer->peekNextToken();

            if (next->type == TOK_DOUBLECOLON){
                Token *lookahead = lexer->peekNextToken(2);
                if(lookahead->type == TOK_STRUCT){

                    // parse struct
                }
                else {
                    // parse function def
                    return parseFunctionDeclaration(/*is_local=*/false);
                }
            }
            else if (next->type == TOK_ASSIGN) {
                const char *varName = current->value;
                advance(); // consume ident
                advance(); // and '='
                Ast_Expression *rhs = parseExpression();

                Ast_Ident *lhs = AST_NEW(pool,Ast_Ident);
                lhs->name = varName;

                Ast_Binary *assignExpr = AST_NEW(pool,Ast_Binary);
                assignExpr->op = BINOP_ASSIGN;
                assignExpr->lhs = lhs;
                assignExpr->rhs = rhs;

                Expect(TOK_SEMICOLON, "Expected ';' after assignment");

                Ast_Statement *stmt = AST_NEW(pool,Ast_Statement);
                stmt->expression = assignExpr;
                return stmt;
            }
            else if(next->type == TOK_LPAREN){
                Ast_Procedure_Call_Expression *expr = parseCall();

                Expect(TOK_SEMICOLON, "Expected ';' after printf call.");

                Ast_Statement *stmt = AST_NEW(pool,Ast_Statement);
                stmt->expression = expr;
                return stmt;
            }
            else {
                Ast_Declaration *decl = parseVarDeclaration();
                return decl;
            }
        }
        case TOK_RETURN: {
            advance(); // consume 'return'

            Ast_Statement* stmt = AST_NEW(pool, Ast_Statement);
            stmt->is_return = true;

            // Optional return value
            if (current->type != TOK_SEMICOLON) {
                stmt->expression = parseExpression();
            }

            expect(TOK_SEMICOLON, "Expected ';' after return statement.");
            return stmt;
        }
        case TOK_STAR:
        case TOK_CARET:
        case TOK_AMPERSAND: {
            // these can be in front of statement
            Ast_Expression *lhs = parseExpression(); // could be *p, ^x, &y

            Expect(TOK_ASSIGN, "Expected '=' in pointer assignment.");

            Ast_Expression *rhs = parseExpression();

            Ast_Binary *assignExpr = AST_NEW(pool,Ast_Binary);
            assignExpr->op = BINOP_ASSIGN;
            assignExpr->lhs = lhs;
            assignExpr->rhs = rhs;

            Expect(TOK_SEMICOLON, "Expected ';' after assignment.");

            Ast_Statement *stmt = AST_NEW(pool,Ast_Statement);
            stmt->expression = assignExpr;
            return stmt;
        }
        case TOK_PRINT: {
            Ast_Procedure_Call_Expression *expr = parseCall();

            Expect(TOK_SEMICOLON, "Expected ';' after printf call.");

            Ast_Statement *stmt = AST_NEW(pool,Ast_Statement);
            stmt->expression = expr;
            return stmt;
        }
        case TOK_IF:
            return parseIfStatement();
        case TOK_ELSE:
        {
            parseError("Got 'else' without an 'if' statement.");
            exitSuccess = false;
            advance();
            // break;
            return nullptr;
        }

        case TOK_LCURLY_PAREN: {
            bool is_scoped_block = true;
            Ast_Block *scopedBlock = parseBlockStatement(is_scoped_block);
            Ast_Statement *stmt = AST_NEW(pool,Ast_Statement);
            stmt->block = scopedBlock;
            return stmt;
        }

        case TOK_NUMBER:
        case TOK_STRING:
        case TOK_FLOAT: {
            Ast_Expression *expr = parseExpression();
            expect(TOK_SEMICOLON, "Expected ';' after expression statement.");
            Ast_Statement *stmt = AST_NEW(pool,Ast_Statement);
            stmt->expression = expr;
            return stmt;
        }
        default:
            parseError("Unexpected token at start of statement: " );
            synchronize();
            return nullptr;
    }
}


Ast_Block *Parser::parseProgram()
{
    Ast_Block *program = AST_NEW(pool,Ast_Block);

     // printf("size of Token %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Token));
    // printf("size of Ast_Ident %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Ast_Ident));
    // printf("size of Ast_Procedure_Call_Expression %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Ast_Procedure_Call_Expression));

    bool mainFound = false;

    while (current->type != TOK_END_OF_FILE)
    {
        if (current->type == TOK_MAIN_ENTRY_POINT) {
            if (mainFound) {
                parseError("Multiple 'main' functions not allowed.");
                exitSuccess = false;
                advance();
            }
            mainFound = true;
            Ast_Statement *stmt = AST_NEW(pool,Ast_Statement);

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
            else if(next->type == TOK_DOUBLECOLON) {
                // Ast_Statement *stmt = parseStatement();
                // **function declaration or definition**
                Ast_Declaration *funcDecl = parseFunctionDeclaration(/*is_local=*/false);
                program->statements.push_back(static_cast<Ast_Statement*>(funcDecl));
            }
            else {
                parseError("Top-level executable statements not allowed. Only declarations and main.");
                exitSuccess = false;
                synchronize();
                break;
            }
        }
        else {
            parseError("Unexpected token at top-level. Only declarations and main function allowed.");
            exitSuccess = false;
            synchronize();
            break;
        }
    }


    if (!mainFound && exitSuccess) {
        parseError("No 'main' function found. An entry point is required.");
        exit(1);
    }
    if(!exitSuccess){
        printf("\n\nExiting... There were errors.\n\n");
        exit(1);
    }
    return program;
}

