#pragma once
#include "lexer.h"
#include "ast.h"

#include "pool.h"

struct Parser {
    Lexer *lexer;

    Pool *pool;

    Token *current = nullptr;
    Token *previous = nullptr;

    Parser(Lexer *l, Pool *pool);
    ~Parser();

    void parseError(const std::string &message);
    void reportError(const std::string &message);
    void expect(TokenType expectedType, const std::string &errorMessage);
    void Expect(TokenType expectedType, const std::string &errorMessage);

    void logDebug(const std::string &message, const Token *token) const;
    void advance();

    void synchronize();
    Ast_Block *parseProgram();

private:
    Ast_Expression *parseFactor();
    Ast_Expression *parseTerm();
    Ast_Expression *parseExpression();
    Ast_Type_Definition *parseTypeSpecifier();
    Ast_Declaration *parseVarDeclaration();
    Ast_If *parseIfStatement();
    Ast_Block *parseBlockStatement(bool scoped_block = false);
    Ast_Procedure_Call_Expression *parseCall();

    Ast_Struct_Description *parseStructDefinition();
    Ast_Statement *parseStatement();


};