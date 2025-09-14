#include "lexer.h"

#define malloc(s) _malloc_dbg(s, _NORMAL_BLOCK, __FILE__, __LINE__)
#define free(p) _free_dbg(p, _NORMAL_BLOCK)

#define MAX_NUM_STR_LEN 64


char* Lexer::pool_strdup(Pool* pool, const char* str) {
    long len = strlen(str)+1;
    char* p = (char*)pool_alloc(pool, len);
    memcpy(p, str, len);
    //printf("pool_strdup %d\"%.*s\"\n", len, len, p);
    return p;
}

Token* Lexer::makeToken(TokenType type, const char* value, int row, int col) {
    printf("First pool_alloc in makeToken\n");
    Token* t = (Token*)pool_alloc(lex_pool, sizeof(Token));
    t->type = type;
    if (type == TOK_IDENTIFIER || type == TOK_PRINT || type == TOK_IF || type == TOK_STRUCT
        || type == TOK_TYPE_INT || type == TOK_TYPE_FLOAT || type == TOK_TYPE_STRING
        || type == TOK_TYPE_BOOL || type == TOK_KEYWORD_TRUE || type == TOK_KEYWORD_FALSE) {
        printf("Second pool_alloc in makeToken\n");
        t->value = pool_strdup(lex_pool, value);
        t->owns_value = false;
    } else {
        t->value = value;
         t->owns_value = false;
    }
    t->row = row;
    t->col = col;
    return t;
}

Token* Lexer::makeIntToken(TokenType type, unsigned long long val, int row, int col)
{
    Token* t = (Token*)pool_alloc(lex_pool, sizeof(Token));
    t->type = type;
    t->int_value = val;
    t->row = row;
    t->col = col;
    t->owns_value = false;
    return t;
}

Token* Lexer::makeFloatToken(TokenType type, float val, int row, int col)
{
    Token* t = (Token*)pool_alloc(lex_pool, sizeof(Token));
    t->type = type;
    t->float32_value = val;
    t->row = row;
    t->col = col;
    t->owns_value = false;
    return t;
}


inline void Lexer::lexerError(const std::string& message, int row, int col) {
    printf("\nLexer Error [%d:%d] %s", row, col, message.c_str());
    exit(1);
}

Token* Lexer::stringToken(int row, int col)
{
    const char* startPtr = Source + Pos+1; // start after quote
    while (Pos < size && Source[Pos+1] != '"') {
        get_and_advance();
    }

    if (Pos >= size) {
        lexerError("Lexer Error: Unterminated string literal", row, col);
        return makeToken(TOK_UNKNOWN, "", row, col);
    }

    size_t len = (Source + Pos+1) - startPtr;
    get_and_advance();
    get_and_advance();

    unsigned char* str = (unsigned char*) pool_alloc(lex_pool, len+1);
    memcpy(str, startPtr, len);
    str[len] = '\0';

    Token* t = (Token*)pool_alloc(lex_pool, sizeof(Token));
    t->type = TOK_STRING;
    t->string_value.data = str;
    t->string_value.count = len;
    t->row = row;
    t->col = col;
    t->owns_value = false;

    // free(str);

    return t;
}

Token* Lexer::numberToken(char first, int row, int col)
{
    static char num_str_buffer[MAX_NUM_STR_LEN];

    int buffer_idx = 0;

    if (buffer_idx < MAX_NUM_STR_LEN - 1) {
        num_str_buffer[buffer_idx++] = first;
    } else {
        lexerError("Lexer Error: Number literal too long", row, col);
        return makeToken(TOK_UNKNOWN, "", row, col);
    }

    // Parse the integer part of the number
    while (Pos < size && isNumeric(Source[Pos])) {
        if (buffer_idx < MAX_NUM_STR_LEN - 1) {
            num_str_buffer[buffer_idx++] = get_and_advance();
        } else {
            lexerError("Lexer Error: Number literal too long", row, col);
            while (Pos < size && isNumeric(Source[Pos])) get_and_advance();
            return makeToken(TOK_UNKNOWN, "", row, col);
        }
    }

    // check for a decimal point (for float type)
    if (Pos < size && Source[Pos] == '.') {
        if (buffer_idx < MAX_NUM_STR_LEN - 1) {
            num_str_buffer[buffer_idx++] = get_and_advance(); // Consume the '.'
        } else {
            lexerError("Lexer Error: Number literal too long (decimal part)", row, col);
            return makeToken(TOK_UNKNOWN, "", row, col);
        }

        // since its a float type parse  fractional part
        bool has_fractional_digits = false;
        while (Pos < size && isNumeric(Source[Pos])) {
            if (buffer_idx < MAX_NUM_STR_LEN - 1) {
                num_str_buffer[buffer_idx++] = get_and_advance();
                has_fractional_digits = true;
            } else {
                lexerError("Lexer Error: Number literal too long (fractional part)", row, col);
                while (Pos < size && isNumeric(Source[Pos])) get_and_advance();
                return makeToken(TOK_UNKNOWN, "", row, col);
            }
        }

        if (!has_fractional_digits && num_str_buffer[buffer_idx - 1] == '.') {
            lexerError("Lexer Error: Malformed float literal (missing fractional digits)", row, col);
            // exit(1); // lets not exit like this its bad
            return makeToken(TOK_UNKNOWN, "", row, col);
        }

        num_str_buffer[buffer_idx] = '\0';

        // Convert to float using strtof
        char* end_ptr;
        float val = strtof(num_str_buffer, &end_ptr);

        if (*end_ptr != '\0') {
            lexerError("Lexer Error: Invalid float literal conversion", row, col);
            return makeToken(TOK_UNKNOWN, num_str_buffer, row, col);
        }

        return makeFloatToken(TOK_FLOAT, val, row, col);
    }

    // If no decimal point, it's an integer
    num_str_buffer[buffer_idx] = '\0';

    char* end_ptr;
    unsigned long long val = strtoull(num_str_buffer, &end_ptr, 10);

    if (*end_ptr != '\0') {
        lexerError("Lexer Error: Invalid integer literal conversion", row, col);
        return makeToken(TOK_UNKNOWN, num_str_buffer, row, col);
    }

    return makeIntToken(TOK_NUMBER, val, row, col);
}

Token* Lexer::identifierToken(char first, int row, int col)
{
    const char* startPtr = Source + Pos - 1;
    while (Pos < size && (isAlphaNumeric(Source[Pos]) || Source[Pos] == '_')) {
        get_and_advance();
    }

    size_t len = (Source + Pos) - startPtr;
    char* ident = (char*)malloc(len + 1);
    memcpy(ident, startPtr, len);
    ident[len] = '\0';

    TokenType type = TOK_IDENTIFIER;

    // printf("ident: %s\n", ident);

    if (strcmp(ident, "printf") == 0) type = TOK_PRINT;
    else if (strcmp(ident, "if") == 0) type = TOK_IF;
    else if (strcmp(ident, "else") == 0) type = TOK_ELSE;
    else if (strcmp(ident, "main") == 0) type = TOK_MAIN_ENTRY_POINT;
    else if (strcmp(ident, "int") == 0) type = TOK_TYPE_INT;
    else if (strcmp(ident, "bool") == 0) type = TOK_TYPE_BOOL;
    else if (strcmp(ident, "true") == 0) type = TOK_KEYWORD_TRUE;

    else if (strcmp(ident, "float") == 0) type = TOK_TYPE_FLOAT;
    else if (strcmp(ident, "false") == 0) type = TOK_KEYWORD_FALSE;

    else if (strcmp(ident, "string") == 0) type = TOK_TYPE_STRING;
    else if (strcmp(ident, "struct") == 0) type = TOK_STRUCT;


    Token* t = makeToken(type, ident, row, col);

    free(ident);
    return t;
}


Token* Lexer::nextToken()
{

    skipUnwantedChar();

    if (Pos >= size) {
        return makeToken(TOK_END_OF_FILE, "EOF", Row, Col);
    }
    int sRow = Row;
    int sCol = Col;

    char c = get_and_advance();

    // the String[Pos] pointer at this point is no longer on the same character as c above but the next character. its like consuming and moving forward

    switch (c) {
        case '(': return makeToken(TOK_LPAREN, "(", sRow, sCol);
        case ')': return makeToken(TOK_RPAREN, ")", sRow, sCol);
        case '{': return makeToken(TOK_LCURLY_PAREN, "{", sRow, sCol);
        case '}': return makeToken(TOK_RCURLY_PAREN, "}", sRow, sCol);
        case '[': return makeToken(TOK_LBRACKET, "[", sRow, sCol);
        case ']': return makeToken(TOK_RBRACKET, "]", sRow, sCol);
        case ':':
            if (match_and_advance(':')) return makeToken(TOK_DOUBLECOLON, "::", sRow, sCol);
            return makeToken(TOK_COLON, ":", sRow, sCol);
        case ';': return makeToken(TOK_SEMICOLON, ";", sRow, sCol);
        case ',': return makeToken(TOK_COMMA, ",", sRow, sCol);
        case '\'':return makeToken(TOK_SINGLEQOUTE, "'", sRow, sCol);
        case '+': return makeToken(TOK_PLUS, "+", sRow, sCol);
        case '-': return makeToken(TOK_MINUS, "-", sRow, sCol);
        case '*':
            return makeToken(TOK_STAR, "*", sRow, sCol);

        case '=':
            if (match_and_advance('=')) return makeToken(TOK_EQUAL, "==", sRow, sCol);
            return makeToken(TOK_ASSIGN, "=", sRow, sCol);
        case '!':
            if (match_and_advance('=')) return makeToken(TOK_NOT_EQUAL, "!=", sRow, sCol);
            break;
        case '<':
            if (match_and_advance('=')) return makeToken(TOK_LESS_EQUAL, "<=", sRow, sCol);
            return makeToken(TOK_LESS, "<", sRow, sCol);
        case '>':
            if (match_and_advance('=')) return makeToken(TOK_GREATER_EQUAL, ">=", sRow, sCol);
            return makeToken(TOK_GREATER, ">", sRow, sCol);
        case '"':
            if(check_prev_char('\'') && match_and_advance('\''))
                return makeToken(TOK_DOUBLEQUOTE, "\"", sRow, sCol);
            return stringToken(Row, Col);
        case '/':
            return makeToken(TOK_SLASH, "/", sRow, sCol);
        case '^':
            return makeToken(TOK_CARET, "^", sRow, sCol);
        case '&':
            return makeToken(TOK_AMPERSAND, "&", sRow, sCol);
            break;
    }

    if (isNumeric(c)) return numberToken(c, sRow, sCol);
    if (isAlpha(c)) return identifierToken(c, sRow, sCol);

    // nothing matches
    return makeToken(TOK_ERROR, "", sRow, sCol);

}

Token* Lexer::peekNextToken(int lookahead)
{

    size_t origPos = Pos;
    int origRow = Row;
    int origCol = Col;

    for(int i=0; i<lookahead; i++){
        peeked_token = nextToken();
    }

    Pos = origPos;
    Row = origRow;
    Col = origCol;

    return peeked_token;
}