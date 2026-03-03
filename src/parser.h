#pragma once

#include "lexer.h"
#include "ast.h"

struct Pax_Interp;

struct Parser
{
    Pax_Interp *interp;

    Lexer *lexer;

    Token *current = nullptr;
    Token *previous = nullptr;

    Parser(Lexer *l, Pax_Interp *_interp);

    void advance();

    void parseError(const char *message, bool print_token_type = false);
    void report_parse_error(const char *fmt, ...);

    void expect(TokenType expectedType, const char *errorMessage);
    void Expect(TokenType expectedType, const char *errorMessage);

    void synchronize();
    Ast_Block *parseProgram(bool skip_main = false);

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
    Ast_Statement *parseMultipleAssignment();
    Ast_If *parseIfStatement();
    Ast_Block *parseBlockStatement(bool scoped_block = false, bool if_block = false);
    Ast_Comma_Separated_Args *parseCommaSeparatedExpressions();

    Ast_Procedure_Call_Expression *parseCall();

    Ast_Statement *parseStructDefinition();

    Ast_Declaration *parseFunctionDeclaration(bool is_local = false);

    bool is_lhs_assignment();
    Ast_Statement *parseStatement();

};