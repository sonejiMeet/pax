#pragma once

#include <string>

enum TokenType {
    TOK_ERROR,

    TOK_PRINT,

    TOK_IF,
    TOK_IDENTIFIER, // variable
    TOK_STRUCT,

    TOK_NUMBER,
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
    TOK_SEMICOLON,
    TOK_COMMA,
    TOK_BACKTICK,
    TOK_QUOTATION,

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

    TOK_END_OF_FILE,

    TOK_UNKNOWN  // TODO: we should just exit lexing here with error saying Unkown token or smt
};

const char* tokenTypeToString(TokenType type);

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

    // std::string value;
    int row;
    int col;
};

