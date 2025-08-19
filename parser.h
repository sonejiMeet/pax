#pragma once
#include "lexer.h"
#include "ast.h"

struct Parser {
    Lexer* lexer;
    Token current;

    Parser(Lexer* l);

    // Token peekNextToken();
    void parseError(const std::string& message);
    void expect(TokenType expectedType, const std::string& errorMessage);

    void advance();

    ASTNode* parseProgram();
private:
    ASTNode* parseFactor();
    ASTNode* parseTerm();
    ASTNode* parseExpression();
    ASTNode* parseVarDeclaration();
    ASTNode* parseStatement();


};
