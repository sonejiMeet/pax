#include "token.h"


const char* tokenTypeToString(TokenType type) {
    switch (type) {
        case TOK_PRINT: return "TOK_PRINT";
        case TOK_IF: return "TOK_IF";
        case TOK_IDENTIFIER: return "TOK_IDENTIFIER";
        case TOK_STRUCT: return "TOK_STRUCT";

        case TOK_TYPE_INT: return "TOK_TYPE_INT";
        case TOK_TYPE_FLOAT: return "TOK_TYPE_FLOAT,";
        case TOK_TYPE_STRING: return "TOK_TYPE_STRING";
        case TOK_TYPE_BOOL: return "TOK_TYPE_BOOL";

        case TOK_NUMBER: return "TOK_NUMBER";
        case TOK_STRING: return "TOK_STRING";
        case TOK_LPAREN: return "TOK_LPAREN";
        case TOK_RPAREN: return "TOK_RPAREN";
        case TOK_LCURLY_PAREN: return "TOK_LCURLY_PAREN";
        case TOK_RCURLY_PAREN: return "TOK_RCURLY_PAREN";
        case TOK_LBRACKET: return "TOK_LBRACKET";
        case TOK_RBRACKET: return "TOK_RBRACKET";
        case TOK_COLON: return "TOK_COLON";
        case TOK_SEMICOLON: return "TOK_SEMICOLON";
        case TOK_COMMA: return "TOK_COMMA";
        case TOK_BACKTICK: return "TOK_BACKTICK";
        case TOK_QUOTATION: return "TOK_QUOTATION";
        case TOK_PLUS: return "TOK_PLUS";
        case TOK_MINUS: return "TOK_MINUS";
        case TOK_STAR: return "TOK_STAR";
        case TOK_SLASH: return "TOK_SLASH";
        case TOK_ASSIGN: return "TOK_ASSIGN";
        case TOK_EQUAL: return "TOK_EQUAL";
        case TOK_NOT_EQUAL: return "TOK_NOT_EQUAL";
        case TOK_LESS: return "TOK_LESS";
        case TOK_GREATER: return "TOK_GREATER";
        case TOK_LESS_EQUAL: return "TOK_LESS_EQUAL";
        case TOK_GREATER_EQUAL: return "TOK_GREATER_EQUAL";
        case TOK_COMMENT: return "TOK_COMMENT";
        case TOK_L_MULTILINE_COMMENT: return "TOK_L_MULTILINE_COMMENT";
        case TOK_R_MULTILINE_COMMENT: return "TOK_R_MULTILINE_COMMENT";
        case TOK_END_OF_FILE: return "TOK_END_OF_FILE";
        case TOK_UNKNOWN: return "TOK_UNKNOWN";
        default: return "UNKNOWN_TOKEN";
    }
}
