#pragma once
#include "lexer.h"
#include "ast.h"

#include "pool.h"

struct Parser
{
    Lexer *lexer;

    Pool *pool;
    Def_Type *_type;
    Token *current = nullptr;
    Token *previous = nullptr;

    Parser(Lexer *l, Pool *pool, Def_Type *type);

    void advance();

    void parseError(const char *message);

    void expect(TokenType expectedType, const char *errorMessage);
    void Expect(TokenType expectedType, const char *errorMessage);

    void synchronize();
    Ast_Block *parseProgram();

    //
    //  KEEP THIS OLD RECURSIVE DECENT
    //  MAYBE WANT TO COMPARE IN OUR THESIS WHY THIS IS SLOWER AND LESS EFFICIENT
    //

    // Ast_Expression *parseFactor();
    // Ast_Expression *parseTerm();
    // Ast_Expression *parseAdditive();
    // Ast_Expression *parseExpression();

    Ast_Expression *parseExpression(int minPrecedence = 0);
    int getPrecedence(TokenType type);
    Binary_Op getBinaryOperator(TokenType type);


    Ast_Type_Definition *parseTypeSpecifier();
    Ast_Declaration *parseVarDeclaration();
    Ast_If *parseIfStatement();
    Ast_Block *parseBlockStatement(bool scoped_block = false);
    Ast_Procedure_Call_Expression *parseCall();

    // Ast_Struct *parseStructDefinition();
    Ast_Statement *parseStructDefinition();

    Ast_Declaration* parseFunctionDeclaration(bool is_local = false);

    bool is_lhs_assignment();
    Ast_Statement *parseStatement();

};