#pragma once
#include "token.h"
#include "tools.h"

#include "pool.h"

struct Lexer
{
    const char *Source; // entire file
    size_t size; // size of file
    size_t Pos; // current position in Source while we interate over it to create Tokens

    // position in file
    int Row;
    int Col;

    Token* peeked_token = nullptr;

    Pool *lex_pool;

    Lexer(const char *data, size_t len, Pool *pool)
    {
        Source = data;
        size = len;
        Pos = 0;

        Row = 1;
        Col = 1;

        peeked_token = nullptr;

        lex_pool = pool;
    }

    char* pool_strdup(Pool* pool, const char* str);

    inline void lexerError(const char *message, int row, int col);

    inline char get_and_advance() {
        char c = Source[Pos];
        ++Pos;
        if(c == '\n'){
            Row++;
            Col = 1;
        }
        else {
            Col++;
        }
        return c;
    }

    inline bool match_and_advance(char expected)
    {
        if (Pos >= size || Source[Pos] != expected) return false; // if we dont match it then no need to advance.
        get_and_advance();
        return true;
    }

    inline bool check_prev_char(char expected)
    {
        if (Pos >= size || Source[Pos--] != expected) return false;
        Source[Pos++];

        return true;
    }

    inline void skipUnwantedChar()
    {
       while (Pos < size) {
            char c = Source[Pos];

            if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
                get_and_advance();
                continue;
            }

            if (c == '/' && Pos + 1 < size && Source[Pos + 1] == '/') {
                Pos += 2; // skip '//'
                while (Pos < size && Source[Pos] != '\n') get_and_advance();
                continue;
            }

            if (c == '/' && Pos + 1 < size && Source[Pos + 1] == '*') {
                Pos += 2; // skip '/*'
                bool closed = false;
                while (Pos < size) {
                    if (Source[Pos] == '/' && Pos + 1 < size && Source[Pos + 1] == '/') {  // in case multiline comment is not commented out by a single line comment, if it is skip the line
                        Pos += 2;
                        while (Pos < size && Source[Pos] != '\n') get_and_advance();
                        continue;
                    }
                    if (Source[Pos] == '*' && Pos + 1 < size && Source[Pos + 1] == '/') {
                        Pos += 2; // skip '*/'
                        closed = true;
                        break;
                    }
                    get_and_advance();
                }
                if (!closed) {
                    lexerError("Unterminated multi-line comment", Row, Col);
                }
                continue;
            }

            break; // not whitespace or comment
        }
    }

    Token* makeToken(TokenType type, const char* value, int row, int col);
    Token* makeIntToken(TokenType type, unsigned long long val, int row, int col);
    Token* makeFloatToken(TokenType type, float val, int row, int col);


    Token* stringToken(int row, int col);
    Token* numberToken(char first, int row, int col);
    Token* identifierToken(char first, int row, int col);

    Token* nextToken();
    Token* peekNextToken(int lookahead = 1);

};
