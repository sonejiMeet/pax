#pragma once
#include "lexer.h"
#include "ast.h"

#include "pool.h"

struct Parser
{
    Lexer *lexer;

    Pool *pool;

    Token *current = nullptr;
    Token *previous = nullptr;

    Parser(Lexer *l, Pool *pool);

    void advance();

    void parseError(const char *message);

    void expect(TokenType expectedType, const char *errorMessage);
    void Expect(TokenType expectedType, const char *errorMessage);

    void synchronize();
    Ast_Block *parseProgram();

    Ast_Expression *parseFactor();
    Ast_Expression *parseTerm();
    Ast_Expression *parseExpression();
    Ast_Type_Definition *parseTypeSpecifier();
    Ast_Declaration *parseVarDeclaration();
    Ast_If *parseIfStatement();
    Ast_Block *parseBlockStatement(bool scoped_block = false);
    Ast_Procedure_Call_Expression *parseCall();

    Ast_Struct_Description *parseStructDefinition();

    Ast_Declaration* parseFunctionDeclaration(bool is_local = false);
    Ast_Statement *parseStatement();


};