#include "parser.h"
#include "token.h"

#include <iostream>

//#ifdef _DEBUG
//    printf("-----------------------------------used AST_NEW for [%d %d] %s---%zu---\n", current->row, current->col, typeid(type).name(), sizeof(type)); \
//#endif

// MACRO
#define AST_NEW(pool, type) ([&]() -> type* {                     \
    assert(pool != nullptr && "Pool must not be null");           \
    void* mem = pool_alloc(pool, sizeof(type));                  \
    type* node = new (mem) type(pool);                           \
    node->line_number = current->row;                              \
    node->character_number = current->col;                        \
    return node;                                                  \
}())

bool exitSuccess = true;

Parser::Parser(Lexer* l, Pool *p) : lexer(l), pool(p) {
    current = lexer->nextToken();
}

void Parser::advance() {
    previous = current;
    current = lexer->nextToken();
}

void Parser::parseError(const std::string& message) {
    std::cout << "\n" << ": Parsing Error" <<  "[" << current->row << ":" << current->col << "] "  << message
              << " at token '" << current->value << "' (Type: "
              << tokenTypeToString(current->type) << ")";

}

void Parser::reportError(const std::string& message) {
    std::cout << "\n" << ": Parsing Error" <<  "[" << current->row << ":" << current->col << "] "  << message;
}

void Parser::expect(TokenType expectedType, const std::string& errorMessage)
{
    if (current->type != expectedType) {
        parseError(errorMessage);
        exitSuccess = false;
        synchronize();
        return;
    }
    advance();
}
void Parser::Expect(TokenType expectedType, const std::string& errorMessage)
{
    if (current->type != expectedType) {

    std::cout << "\n" << __FILE__ << ": Parsing Error" <<  "[" << previous->row << ":" << previous->col << "] "  << errorMessage;

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
Ast_Expression* Parser::parseFactor()
{
    // Handle unary operators first: *, ^, &
    if (current->type == TOK_STAR) { // dereference
        advance();
        Ast_Unary* node = AST_NEW(pool,Ast_Unary);
        node->op = UNARY_DEREFERENCE;
        node->operand = parseFactor();
        return node;
    }
    else if (current->type == TOK_CARET) { // address-of
        advance();
        Ast_Unary* node = AST_NEW(pool,Ast_Unary);
        node->op = UNARY_ADDRESS_OF;
        node->operand = parseFactor();
        return node;
    }
    else if (current->type == TOK_AMPERSAND) { // reference
        advance();
        Ast_Unary* node = AST_NEW(pool,Ast_Unary);
        node->op = UNARY_REFERENCE;
        node->operand = parseFactor();
        return node;
    }

    if (current->type == TOK_NUMBER)
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
        Ast_Ident *node = AST_NEW(pool,Ast_Ident);
        node->name = current->value;

        advance();
        return node;
    }
    else if (current->type == TOK_LPAREN)
    {
        advance();
        Ast_Expression* expr = parseExpression();

        Expect(TOK_RPAREN, "Expected ')' after expression in parentheses.");

        return expr;
    }

    parseError("Expected a literal, identifier, or parenthesised expression factor.");

    return nullptr;
}

// Parses a term (multiplication and division operations)
Ast_Expression* Parser::parseTerm()
{
    Ast_Expression* left = parseFactor();

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
Ast_Expression* Parser::parseExpression()
{
    Ast_Expression* left = parseTerm();

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


Ast_Type_Definition* Parser::parseTypeSpecifier() {

    Ast_Type_Definition *currentType = nullptr;

    while (true) {
        if (current->type == TOK_CARET || current->type == TOK_STAR) {
            // Pointer
            Ast_Type_Definition *pointerType = AST_NEW(pool,Ast_Type_Definition);
            pointerType->pointed_to_type = nullptr;
            pointerType->builtin_type = TYPE_UNKNOWN; // in case its pointer symbol, they dont have a builtin type

            if (currentType) {
                pointerType->pointed_to_type = currentType;
            }

            currentType = pointerType;
            advance();
        }
        else if (current->type == TOK_AMPERSAND) {
            // Reference
            Ast_Type_Definition *refType = AST_NEW(pool,Ast_Type_Definition);
            refType->pointed_to_type = nullptr;
            refType->builtin_type = TYPE_UNKNOWN;

            if (currentType) {
                refType->pointed_to_type = currentType;
            }

            currentType = refType;
            advance();
        }
        else if (current->type == TOK_LBRACKET) {
            // Array type
            advance();

            Ast_Type_Definition *arrayType = AST_NEW(pool,Ast_Type_Definition);
            arrayType->element_type = nullptr;
            arrayType->builtin_type = TYPE_UNKNOWN;

            if (current->type == TOK_NUMBER) {
                // Static array
                arrayType->array_kind = ARRAY_STATIC;
                arrayType->static_array_size = current->int_value;
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

    // Now expect the base type
    Ast_Type_Definition *baseType = AST_NEW(pool,Ast_Type_Definition);

    if (current->type == TOK_TYPE_INT)
        baseType->builtin_type = TYPE_INT;
    else if (current->type == TOK_TYPE_FLOAT)
        baseType->builtin_type = TYPE_FLOAT;
    else if (current->type == TOK_TYPE_STRING)
        baseType->builtin_type = TYPE_STRING;
    else if (current->type == TOK_TYPE_BOOL)
        baseType->builtin_type = TYPE_BOOL;
    else {
        reportError("Expected a base type (e.g 'int', 'float', 'string', 'bool')");
        synchronize();
        return nullptr;
    }

    advance();

    if (!currentType) // it was a pure type
        return baseType;


    Ast_Type_Definition *iter = currentType;
    while (true) {
        if (iter->array_kind == ARRAY_NONE && iter->pointed_to_type == nullptr) {
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
Ast_Declaration* Parser::parseVarDeclaration()
{
    Ast_Declaration* varDecl = AST_NEW(pool,Ast_Declaration);

    const char * varName = current->value;
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


Ast_If* Parser::parseIfStatement(){

    advance();
    Expect(TOK_LPAREN, "Expected '(' before start of expression in if statement .");
    Ast_Expression *condition = parseExpression();
    Expect(TOK_RPAREN, "Expected ')' after end of expression in if statement.");

    Ast_Block *thenBlock = parseBlockStatement();

    Ast_If* ifNode = AST_NEW(pool,Ast_If);

    ifNode->condition = condition;
    ifNode->then_block = thenBlock;

    if(current->type == TOK_ELSE){
        advance();
        Ast_Block *elseBlock = parseBlockStatement();
        ifNode->else_block = elseBlock;
    }

    return ifNode;

}
Ast_Block* Parser::parseBlockStatement(bool scoped_block) {
    expect(TOK_LCURLY_PAREN, "Expected '{' to start a block statement.");

    Ast_Block* block = AST_NEW(pool,Ast_Block);

    block->is_scoped_block = scoped_block;

    while (current->type != TOK_RCURLY_PAREN && current->type != TOK_END_OF_FILE) {
        Ast_Statement* stmt = parseStatement();
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


Ast_Procedure_Call_Expression* Parser::parseCall()
{
    Token *identToken = current;

    Ast_Procedure_Call_Expression* callExpr = AST_NEW(pool,Ast_Procedure_Call_Expression);
    callExpr->function = AST_NEW(pool,Ast_Ident);
    callExpr->function->name = identToken->value;

    advance();

    expect(TOK_LPAREN, "Expected '(' after function name");


    Ast_Comma_Separated_Args* argsNode = AST_NEW(pool,Ast_Comma_Separated_Args);

    if(current->type != TOK_RPAREN)
    {
        while(true)
        {
            Ast_Expression* arg = parseExpression();
            argsNode->arguments.push_back(arg);

            if (current->type != TOK_RPAREN && current->type != TOK_COMMA){
                parseError("Expected ',' in function call arguments.");
            }
            if (current->type == TOK_COMMA)
            {
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

// Ast_Struct_Description * Parser::parseStructDefinition()
// {
//     std::string structName = current->value;
//     advance();

//     advance();
//     advance();


// }
Ast_Statement* Parser::parseStatement()
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
                }
            }
            else if (next->type == TOK_ASSIGN) {
                const char * varName = current->value;
                advance(); // consume ident
                advance(); // and '='
                Ast_Expression* rhs = parseExpression();

                Ast_Ident* lhs = AST_NEW(pool,Ast_Ident);
                lhs->name = varName;

                Ast_Binary* assignExpr = AST_NEW(pool,Ast_Binary);
                assignExpr->op = BINOP_ASSIGN;
                assignExpr->lhs = lhs;
                assignExpr->rhs = rhs;

                Expect(TOK_SEMICOLON, "Expected ';' after assignment.");

                Ast_Statement* stmt = AST_NEW(pool,Ast_Statement);
                stmt->expression = assignExpr;
                return stmt;
            }
            else {
                Ast_Declaration* decl = parseVarDeclaration();
                return decl;
            }
        }

        case TOK_STAR:
        case TOK_CARET:
        case TOK_AMPERSAND: {
            // these can be in front of statement
            Ast_Expression* lhs = parseExpression(); // could be *p, ^x, &y, etc.

            Expect(TOK_ASSIGN, "Expected '=' in pointer assignment.");

            Ast_Expression* rhs = parseExpression();

            Ast_Binary* assignExpr = AST_NEW(pool,Ast_Binary);
            assignExpr->op = BINOP_ASSIGN;
            assignExpr->lhs = lhs;
            assignExpr->rhs = rhs;

            Expect(TOK_SEMICOLON, "Expected ';' after assignment.");

            Ast_Statement* stmt = AST_NEW(pool,Ast_Statement);
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
            reportError("Got 'else' without an 'if' statement.");
            exitSuccess = false;
            advance();
            break;
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


Ast_Block* Parser::parseProgram()
{
    Ast_Block* program = AST_NEW(pool,Ast_Block);

    //printf("size of Ast_Ident %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Ast_Ident));
    //printf("size of Ast_Procedure_Call_Expression %zu----------->>>>>>>>>>>>>>>>>>>\n", sizeof(Ast_Procedure_Call_Expression));
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
            Ast_Statement* stmt = AST_NEW(pool,Ast_Statement);

            advance();

            expect(TOK_DOUBLECOLON, "Expected '::' after main entry point."); // fix when not expected
            expect(TOK_LPAREN, "Expected '(' after main entry point.");
            expect(TOK_RPAREN, "Expected ')' after main entry point.");

            Ast_Block* mainBlock = parseBlockStatement();
            mainBlock->is_entry_point = true;  // simple flag

            stmt->block = mainBlock;
            program->statements.push_back(stmt);
        }
        else if (current->type == TOK_IDENTIFIER) {
            Token *next = lexer->peekNextToken();
            if (next->type == TOK_COLON) {
                Ast_Declaration* decl = parseVarDeclaration();
                program->statements.push_back(static_cast<Ast_Statement*>(decl));
            }
            else if(next->type == TOK_DOUBLECOLON) {
                Ast_Statement *stmt = parseStatement();
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

