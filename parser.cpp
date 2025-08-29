#include "parser.h"
#include "token.h"
#include <iostream>
#include <vector>


// https://learn.microsoft.com/en-us/cpp/c-runtime-library/find-memory-leaks-using-the-crt-library?view=msvc-170#interpret-the-memory-leak-report
#ifdef _DEBUG
    #define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )
    // Replace _NORMAL_BLOCK with _CLIENT_BLOCK if you want the
    // allocations to be of _CLIENT_BLOCK type
#else
    #define DBG_NEW new
#endif

Parser::Parser(Lexer* l) : lexer(l) {
    current = lexer->nextToken();
}

void Parser::advance() {
    current = lexer->nextToken();
}

void Parser::parseError(const std::string& message) {
    std::cerr << "Parsing Error" <<  "[" << current.row << ":" << current.col << "] "  << message
              << " at token '" << current.value << "' (Type: "
              << tokenTypeToString(current.type) << ")" << std::endl;

    exit(1); // this will make memory leak but who cares during production hahaha
}

void Parser::expect(TokenType expectedType, const std::string& errorMessage)
{
    if (current.type != expectedType) {
        parseError(errorMessage);
    }
    advance();
}

// in an expression
Ast_Expression* Parser::parseFactor()
{
    if (current.type == TOK_NUMBER)
    {

        Ast_Literal *node = DBG_NEW Ast_Literal();
        node->value_type = LITERAL_NUMBER;
        node->integer_value = current.int_value;
        advance();
        return node;
    }
    else if (current.type == TOK_FLOAT)
    {

        Ast_Literal *node = DBG_NEW Ast_Literal();
        node->value_type = LITERAL_FLOAT;
        node->float_value = current.float32_value;
        advance();
        return node;
    }
    else if (current.type == TOK_STRING )
    {
        Ast_Literal *node = DBG_NEW Ast_Literal();
        node->value_type = LITERAL_STRING;
        node->string_value = std::string(
            reinterpret_cast<const char*>(current.string_value.data),
            static_cast<size_t>(current.string_value.count)
        );
        advance();
        return node;
    }
    else if (current.type == TOK_IDENTIFIER)
    {
        Ast_Ident *node = DBG_NEW Ast_Ident();
        node->name = current.value;
        advance();
        return node;

    }
    else if (current.type == TOK_LPAREN)
    {
        advance();
        Ast_Expression* expr = parseExpression();

        expect(TOK_RPAREN, "Expected ')' after expression in parentheses.");

        return expr;
    }

    parseError("Expected a literal, identifier, or parenthesised expression factor.");

    return nullptr;
}

// Parses a term (multiplication and division operations).
Ast_Expression* Parser::parseTerm()
{
    Ast_Expression* left = parseFactor();

    while (current.type == TOK_STAR || current.type == TOK_SLASH)
    {
        Ast_Binary *node = DBG_NEW Ast_Binary();
        node->lhs = left;

        if(current.type == TOK_STAR) node->op = BINOP_MUL;
        else if (current.type == TOK_SLASH) node->op = BINOP_DIV;

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

    while (current.type == TOK_PLUS || current.type == TOK_MINUS ||
           current.type == TOK_EQUAL || current.type == TOK_NOT_EQUAL ||
           current.type == TOK_LESS || current.type == TOK_GREATER ||
           current.type == TOK_LESS_EQUAL || current.type == TOK_GREATER_EQUAL) {

        Ast_Binary *node = DBG_NEW Ast_Binary();
        node->lhs = left;

        switch(current.type){
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

Ast_Type_Definition* Parser::parseTypeSpecifier()
{
    Ast_Type_Definition *typeDef = DBG_NEW Ast_Type_Definition();

    if(current.type == TOK_TYPE_INT)
        typeDef->builtin_type = TYPE_INT;
    else if(current.type == TOK_TYPE_FLOAT)
        typeDef->builtin_type = TYPE_FLOAT;
    else if(current.type == TOK_TYPE_STRING)
        typeDef->builtin_type = TYPE_STRING;
    else if(current.type == TOK_TYPE_BOOL)
        typeDef->builtin_type = TYPE_BOOL;
    else
        parseError("Expected a type keyword (e.g., 'int', 'float', 'string', 'bool').");

    advance(); // and consume
    return typeDef;
}

//  statement "identifier = expression;"
Ast_Declaration* Parser::parseVarDeclaration()
{
    if (current.type != TOK_IDENTIFIER) {
        parseError("Expected identifier for variable declaration.");
    }
    std::string varName = current.value;
    advance();

    expect(TOK_COLON, "Expected ':' after variable name for type declaration.");

    Ast_Type_Definition *typeDef = parseTypeSpecifier();


    Ast_Expression *initializer = nullptr;
    if (current.type == TOK_ASSIGN) {
        advance();
        initializer = parseExpression();

    }

    expect(TOK_SEMICOLON, "Expected ';' after variable declaration.");

    Ast_Declaration* varDecl = DBG_NEW Ast_Declaration();

    varDecl->declared_type = typeDef;
    varDecl->identifier = DBG_NEW Ast_Ident();
    varDecl->identifier->name = varName;
    varDecl->initializer = initializer;


    return varDecl;
}

Ast_If* Parser::parseIfStatement(){

    advance();
    expect(TOK_LPAREN, "Expected '(' before if statement.");
    Ast_Expression *condition = parseExpression();
    expect(TOK_RPAREN, "Expected ')' after if statement.");

    Ast_Block *thenBlock = parseBlockStatement();

    Ast_If* ifNode = DBG_NEW Ast_If();

    ifNode->condition = condition;
    ifNode->then_block = thenBlock;

    return ifNode;

}

// Parses a block of statements enclosed in curly braces: '{ StatementList }'
Ast_Block* Parser::parseBlockStatement(bool scoped_block) {
    expect(TOK_LCURLY_PAREN, "Expected '{' to start a block statement.");

    Ast_Block* block = DBG_NEW Ast_Block();

    block->is_scoped_block = scoped_block;

    // Parse statements until a closing curly brace or EOF
    while (current.type != TOK_RCURLY_PAREN && current.type != TOK_END_OF_FILE) {
        Ast_Statement* stmt = parseStatement(); // Recursively parse statements within the block
        if (stmt) block->statements.push_back(stmt);
        else parseError("Failed to parse statement within block.");
    }
    expect(TOK_RCURLY_PAREN, "Expected '}' to close a block statement.");

    return block;
}

Ast_Procedure_Call_Expression* Parser::parseCall()
{
    Token identToken = current;
    advance();

    expect(TOK_LPAREN, "Expected '(' after function name");

    Ast_Procedure_Call_Expression* callExpr = DBG_NEW Ast_Procedure_Call_Expression();
    callExpr->function = DBG_NEW Ast_Ident();
    callExpr->function->name = identToken.value;

    Ast_Comma_Separated_Args* argsNode = DBG_NEW Ast_Comma_Separated_Args();

    if(current.type != TOK_RPAREN)
    {
        while(true){
            Ast_Expression* arg = parseExpression();
            argsNode->arguments.push_back(arg);

            if (current.type == TOK_COMMA) {
                advance();
                if(current.type == TOK_RPAREN) {
                    parseError("Expected expression after ',' in function call arguments.");
                }
            } else {
                break; //  must be ')'
            }
        }
    }
    expect(TOK_RPAREN, "Expected ')' after function call arguments");

    callExpr->arguments = argsNode;
    return callExpr;

}

Ast_Statement* Parser::parseStatement()
{

    switch (current.type) {
        case TOK_IDENTIFIER: {
            Token next = lexer->peekNextToken();

            if (next.type == TOK_COLON) {
                Ast_Declaration* decl = parseVarDeclaration();
                return static_cast<Ast_Statement *> (decl);
            }
            Ast_Expression* expr = parseExpression();
            expect(TOK_SEMICOLON, "Expected ';' after expression statement.");
            Ast_Statement* stmt = DBG_NEW Ast_Statement();
            stmt->expression = expr;
            return stmt;
        }
        case TOK_PRINT: {
            Ast_Procedure_Call_Expression *expr = parseCall();
            expect(TOK_SEMICOLON, "Expected ';' after printf call.");

            Ast_Statement *stmt = DBG_NEW Ast_Statement();
            stmt->expression = expr;
            return stmt;
        }
        case TOK_IF:
            return parseIfStatement();
        case TOK_LCURLY_PAREN: {
            bool is_scoped_block = true;
            Ast_Block *scopedBlock = parseBlockStatement(is_scoped_block);
            Ast_Statement *stmt = DBG_NEW Ast_Statement();
            stmt->block = scopedBlock;
            return stmt;
        }
        case TOK_NUMBER:
        case TOK_STRING:
        case TOK_FLOAT: {
            Ast_Expression *expr = parseExpression();
            expect(TOK_SEMICOLON, "Expected ';' after expression statement.");
            Ast_Statement *stmt = DBG_NEW Ast_Statement();
            stmt->expression = expr;
            return stmt;
        }
        default:
            parseError("Unexpected token at start of statement: " );
            return nullptr;
    }
}


Ast_Block* Parser::parseProgram()
{
    Ast_Block * program = DBG_NEW Ast_Block();

    while (current.type != TOK_END_OF_FILE)
    {
        Ast_Statement *stmt  = parseStatement();
        if (stmt){
            program->statements.push_back(stmt);
        }
    }
    return program;
}


