#pragma once
#include "lexer.h"
#include "ast.h"

struct Parser {
    Lexer *lexer;
    Token current;

    Parser(Lexer *l);

    void parseError(const std::string &message);
    void expect(TokenType expectedType, const std::string &errorMessage);
    void logDebug(const std::string &message, const Token *token) const;
    void advance();

    Ast_Block *parseProgram();

private:
    Ast_Expression *parseFactor();
    Ast_Expression *parseTerm();
    Ast_Expression *parseExpression();
    Ast_Type_Definition *parseTypeSpecifier();
    Ast_Declaration *parseVarDeclaration();
    Ast_If *parseIfStatement();
    Ast_Block *parseBlockStatement();
    Ast_Procedure_Call_Expression *parseCall();
    Ast_Statement *parseStatement();


};