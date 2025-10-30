#include "lexer.h"


#ifdef _WIN32
    #define malloc(s) _malloc_dbg(s, _NORMAL_BLOCK, __FILE__, __LINE__)
    #define free(p) _free_dbg(p, _NORMAL_BLOCK)
#endif

#define MAX_NUM_STR_LEN 64


char *Lexer::pool_strdup(Pool *pool, const char *str) {
    size_t len = strlen(str)+1;
    char *p = (char *)pool_alloc(pool, len);
    memcpy(p, str, len);
    //printf("pool_strdup %d\"%.*s\"\n", len, len, p);
    return p;
}

Token *Lexer::makeToken(TokenType type, const char *value) {

#ifdef _DEBUG
    printf("First pool_alloc in makeToken\n");
#endif

    Token *t = (Token*)pool_alloc(lex_pool, sizeof(Token));
    t->type = type;
    if (type == TOK_IDENTIFIER || type == TOK_PRINT || type == TOK_IF || type == TOK_STRUCT || type == TOK_KEYWORD_TRUE || type == TOK_KEYWORD_FALSE){

        t->value = pool_strdup(lex_pool, value);

#ifdef _DEBUG
        printf("Second pool_alloc in makeToken, %p\n", &t->value);
#endif

    } else {
        t->value = value;
    }
    t->row = row;
    t->col = col;
    return t;
}

Token *Lexer::makeIntToken(TokenType type, unsigned long long val)
{
    Token *t = (Token*)pool_alloc(lex_pool, sizeof(Token));
    t->type = type;
    t->int_value = val;
    t->row = row;
    t->col = col;
    return t;
}

Token *Lexer::makeFloatToken(TokenType type, double val)
{
    Token *t = (Token*)pool_alloc(lex_pool, sizeof(Token));
    t->type = type;
    t->float64_value = val;
    t->row = row;
    t->col = col;
    return t;
}

inline void Lexer::lexerError(const char *message) {
    printf("\nLexer Error [%d:%d]: %s", row, col, message);
    exit(1);
}

Token *Lexer::stringToken()
{
    const char *startPtr = Source + Pos+1; // start after quote
    while (Pos < size && Source[Pos+1] != '"') {
        get_and_advance();
    }

    if (Pos >= size) {
        lexerError("Unterminated string literal");
        return makeToken(TOK_ERROR, "");
    }

    size_t len = (Source + Pos+1) - startPtr;
    get_and_advance();
    get_and_advance();

    unsigned char *str = (unsigned char *) pool_alloc(lex_pool, len+1);
    memcpy(str, startPtr, len);
    str[len] = '\0';

    Token *t = (Token*)pool_alloc(lex_pool, sizeof(Token));
    t->type = TOK_STRING;
    t->string_value.data = str;
    t->string_value.count = len;
    t->row = row;
    t->col = col;

    return t;
}

Token *Lexer::numberToken(char first)
{
    static char num_str_buffer[MAX_NUM_STR_LEN];

    int buffer_idx = 0;

    num_str_buffer[buffer_idx++] = first;

    // Parse the integer part of the number
    while (Pos < size && isNumeric(Source[Pos])) {
        if (buffer_idx < MAX_NUM_STR_LEN - 1) {
            num_str_buffer[buffer_idx++] = get_and_advance();
        } else {
            lexerError("Number literal too long");
            while (Pos < size && isNumeric(Source[Pos])) get_and_advance();
            return makeToken(TOK_ERROR, "");
        }
    }

    bool is_float = false;
    // check for a decimal point (for float type)
    if (Pos < size && Source[Pos] == '.') {
        is_float = true;
        if (buffer_idx < MAX_NUM_STR_LEN - 1) {
            num_str_buffer[buffer_idx++] = get_and_advance(); // Consume the '.'
        } else {
            lexerError("Number literal too long (decimal part)");
            return makeToken(TOK_ERROR, "");
        }

        // since its a float type parse  fractional part
        bool has_fractional_digits = false;
        while (Pos < size && isNumeric(Source[Pos])) {
            if (buffer_idx < MAX_NUM_STR_LEN - 1) {
                num_str_buffer[buffer_idx++] = get_and_advance();
                has_fractional_digits = true;
            } else {
                lexerError("Number literal too long (fractional part)");
                while (Pos < size && isNumeric(Source[Pos])) get_and_advance();
                return makeToken(TOK_ERROR, "");
            }
        }

        if (!has_fractional_digits /* && num_str_buffer[buffer_idx - 1] == '.'*/) {
            lexerError("Malformed float literal (missing fractional digits)");
            return makeToken(TOK_ERROR, "");
        }
    }
    num_str_buffer[buffer_idx] = '\0';

    if (is_float){
        // Convert to float using strtof
        char *end_ptr;
        double val = strtod(num_str_buffer, &end_ptr);

        if (*end_ptr != '\0') {
            lexerError("Invalid float literal conversion");
            return makeToken(TOK_ERROR, num_str_buffer);
        }

        return makeFloatToken(TOK_FLOAT, val);
    } else {
        // If no decimal point, it's an integer
        char *end_ptr;

        unsigned long long val = strtoull(num_str_buffer, &end_ptr, 10);
        if (errno == ERANGE) {
            lexerError("Integer literal overflow.");
            return makeToken(TOK_ERROR, num_str_buffer);
        }

        if (*end_ptr != '\0') {
            lexerError("Invalid integer literal conversion");
            return makeToken(TOK_ERROR, num_str_buffer);
        }

        return makeIntToken(TOK_NUMBER, val);
    }
}


Token *Lexer::identifierToken(char first)
{
    const char *startPtr = Source + Pos - 1;
    while (Pos < size && (isAlphaNumeric(Source[Pos]) || Source[Pos] == '_')) {
        get_and_advance();
    }

    size_t len = (Source + Pos) - startPtr;
    char  *ident = (char*)malloc(len + 1);
    memcpy(ident, startPtr, len);
    ident[len] = '\0';

    TokenType type = TOK_IDENTIFIER;

    // printf("ident: %s\n", ident);

    // Temporary replace this with hashmap later
    if (strcmp(ident, "printf") == 0) type = TOK_PRINT;
    else if (strcmp(ident, "if") == 0) type = TOK_IF;
    else if (strcmp(ident, "else") == 0) type = TOK_ELSE;
    else if (strcmp(ident, "main") == 0) type = TOK_MAIN_ENTRY_POINT;

    else if (strcmp(ident, "true") == 0) type = TOK_KEYWORD_TRUE;

    else if (strcmp(ident, "false") == 0) type = TOK_KEYWORD_FALSE;

    else if (strcmp(ident, "struct") == 0) type = TOK_STRUCT;

    else if (strcmp(ident, "return") == 0) type = TOK_RETURN;

    Token *t = makeToken(type, ident);

    free(ident);
    return t;
}


Token *Lexer::nextToken()
{

    skipUnwantedChar();

    if (Pos >= size) {
        return makeToken(TOK_END_OF_FILE, "EOF");
    }
    row = Row;
    col = Col;

    char c = get_and_advance();

    // the String[Pos] pointer at this point is no longer on the same character as c above but the next character. its like consuming and moving forward

    switch (c) {
        case '(': return makeToken(TOK_LPAREN, "(");
        case ')': return makeToken(TOK_RPAREN, ")");
        case '{': return makeToken(TOK_LCURLY_PAREN, "{");
        case '}': return makeToken(TOK_RCURLY_PAREN, "}");
        case '[': return makeToken(TOK_LBRACKET, "[");
        case ']': return makeToken(TOK_RBRACKET, "]");
        case ':':
            if (match_and_advance(':')) return makeToken(TOK_DOUBLECOLON, "::");
            return makeToken(TOK_COLON, ":");
        case ';': return makeToken(TOK_SEMICOLON, ";");
        case ',': return makeToken(TOK_COMMA, ",");
        case '\'':return makeToken(TOK_SINGLEQOUTE, "'");
        case '.': return makeToken(TOK_DOT, ".");
        case '+': return makeToken(TOK_PLUS, "+");
        case '-':
            if (match_and_advance('>')) return makeToken(TOK_ARROW, "->");
            return makeToken(TOK_MINUS, "-");
        case '*':
            return makeToken(TOK_STAR, "*");

        case '=':
            if (match_and_advance('=')) return makeToken(TOK_EQUAL, "==");
            return makeToken(TOK_ASSIGN, "=");
        case '!':
            if (match_and_advance('=')) return makeToken(TOK_NOT_EQUAL, "!=");
            return makeToken(TOK_EXCLAMATION_MARK, "!");
            break;
        case '<':
            if (match_and_advance('=')) return makeToken(TOK_LESS_EQUAL, "<=");
            return makeToken(TOK_LESS, "<");
        case '>':
            if (match_and_advance('=')) return makeToken(TOK_GREATER_EQUAL, ">=");
            return makeToken(TOK_GREATER, ">");
        case '"':
            if(check_prev_char('\'') && match_and_advance('\''))
                return makeToken(TOK_DOUBLEQUOTE, "\"");
            return stringToken();
        case '/':
            return makeToken(TOK_SLASH, "/");
        case '^':
            return makeToken(TOK_CARET, "^");
        case '&':
            return makeToken(TOK_AMPERSAND, "&");
            break;
    }

    if (isNumeric(c)) return numberToken(c);
    if (isAlpha(c)) return identifierToken(c);

    // nothing matches
    return makeToken(TOK_ERROR, "");

}

Token *Lexer::peekNextToken(int lookahead)
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