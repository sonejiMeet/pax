#pragma once
#include "lexer.h"
#include "ast.h"

struct Parser {
    Lexer* lexer;
    Token current;

    // (TEMP) for debugging
    std::vector<Token> tempTokens;

    Parser(Lexer* l);

    // Token peekNextToken();
    void parseError(const std::string& message);
    void expect(TokenType expectedType, const std::string& errorMessage);
    void logDebug(const std::string& message, const Token* token) const;
    void advance();

    // ASTNode* parseExpression();
    ASTNode* parseProgram();

private:
    ASTNode* parseFactor();
    ASTNode* parseTerm();
    ASTNode* parseExpression();
    ASTNode* parseTypeSpecifier();
    ASTNode* parseVarDeclaration();
    ASTNode* parseIfStatement();
    ASTNode* parseBlockStatement();
    ASTNode* parseStatement();


};