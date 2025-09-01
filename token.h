#pragma once

#include <string>

enum TokenType {
    TOK_ERROR,

    TOK_PRINT,

    TOK_IF,
    TOK_ELSE,
    TOK_IDENTIFIER, // variable
    TOK_STRUCT,
    TOK_MAIN_ENTRY_POINT,

    TOK_NUMBER,
    TOK_FLOAT,
    TOK_STRING,

    TOK_TYPE_INT,
    TOK_TYPE_FLOAT,
    TOK_TYPE_STRING,
    TOK_TYPE_BOOL,

    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LCURLY_PAREN,
    TOK_RCURLY_PAREN,
    TOK_LBRACKET,
    TOK_RBRACKET,

    TOK_COLON,
    TOK_DOUBLECOLON,
    TOK_SEMICOLON,
    TOK_COMMA,
    TOK_SINGLEQOUTE,
    TOK_DOUBLEQUOTE,

    TOK_PLUS,
    TOK_MINUS,
    TOK_STAR,
    TOK_SLASH,

    TOK_ASSIGN,
    TOK_EQUAL,
    TOK_NOT_EQUAL,
    TOK_LESS,
    TOK_GREATER,
    TOK_LESS_EQUAL,
    TOK_GREATER_EQUAL,

    TOK_COMMENT,
    TOK_L_MULTILINE_COMMENT, // /*
    TOK_R_MULTILINE_COMMENT, // */

    TOK_KEYWORD_TRUE,
    TOK_KEYWORD_FALSE,

    TOK_END_OF_FILE,

    TOK_UNKNOWN  // TODO: we should just exit lexing here with error saying Unkown token or smt
};


inline const char* tokenTypeToString(TokenType type) {
    switch (type) {
        case TOK_ERROR: return "TOK_ERROR";

        case TOK_PRINT: return "TOK_PRINT";
        case TOK_IF: return "TOK_IF";
        case TOK_ELSE: return "TOK_ELSE";
        case TOK_IDENTIFIER: return "TOK_IDENTIFIER";
        case TOK_STRUCT: return "TOK_STRUCT";
        case TOK_MAIN_ENTRY_POINT: return "TOK_MAIN_ENTRY_POINT";

        case TOK_TYPE_INT: return "TOK_TYPE_INT";
        case TOK_TYPE_FLOAT: return "TOK_TYPE_FLOAT,";
        case TOK_TYPE_STRING: return "TOK_TYPE_STRING";
        case TOK_TYPE_BOOL: return "TOK_TYPE_BOOL";

        case TOK_NUMBER: return "TOK_NUMBER";
        case TOK_FLOAT: return "TOK_FLOAT";
        case TOK_STRING: return "TOK_STRING";

        case TOK_LPAREN: return "TOK_LPAREN";
        case TOK_RPAREN: return "TOK_RPAREN";
        case TOK_LCURLY_PAREN: return "TOK_LCURLY_PAREN";
        case TOK_RCURLY_PAREN: return "TOK_RCURLY_PAREN";
        case TOK_LBRACKET: return "TOK_LBRACKET";
        case TOK_RBRACKET: return "TOK_RBRACKET";
        case TOK_COLON: return "TOK_COLON";
        case TOK_DOUBLECOLON: return "TOK_DOUBLECOLON";
        case TOK_SEMICOLON: return "TOK_SEMICOLON";
        case TOK_COMMA: return "TOK_COMMA";
        case TOK_SINGLEQOUTE: return "TOK_SINGLEQOUTE";
        case TOK_DOUBLEQUOTE: return "TOK_DOUBLEQUOTE";
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
        case TOK_KEYWORD_TRUE: return "TOK_KEYWORD_TRUE";
        case TOK_KEYWORD_FALSE: return "TOK_KEYWORD_FALSE";
        case TOK_END_OF_FILE: return "TOK_END_OF_FILE";
        case TOK_UNKNOWN: return "TOK_UNKNOWN";
        default: return "UNKNOWN_TOKEN";
    }
}

struct Token {
    TokenType type = TOK_ERROR;

    union {
        const char* value;
        unsigned long long int_value;
        float float32_value;
        double float64_value;

        struct {
            unsigned long long count;
            unsigned char* data;
        } string_value;
    };

    int row;
    int col;

    bool owns_value = false;
};


void freeToken(Token& t);