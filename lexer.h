#include "token.h"
#include "tools.h"

struct Lexer
{
    const char *Source;
    size_t size;
    size_t Pos;

    int Row;
    int Col;

    Token peeked_token;
    bool has_peeked;

    Lexer(const char *data, size_t len)
     : Source(data), size(len), Pos(0), Row(1), Col(1), peeked_token({}), has_peeked(false)
    {

    }

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

    inline void skipUnwantedChar() {
        while (Pos < size) {
            char ch = Source[Pos];
            if (ch==' ' || ch=='\t' || ch=='\r' || ch=='\n') {
                get_and_advance();
            } else break;
        }
    }

    Token makeToken(TokenType type, const char* value, int row, int col);
    Token makeIntToken(TokenType type, unsigned long long val, int row, int col);
    Token makeFloatToken(TokenType type, float val, int row, int col);

    Token stringToken(int row, int col);
    Token numberToken(char first, int row, int col);
    Token identifierToken(char first, int row, int col);

    Token nextToken();
    Token peekNextToken();

};

struct FileBuffer {
    uint8_t *data;
    size_t size;
};

FileBuffer read_entire_file(const char *path);