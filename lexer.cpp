#include "lexer.h"

#define MAX_NUM_STR_LEN 64

Token Lexer::makeToken(TokenType type, const char* value, int row, int col) {
    Token t;
    t.type = type;
    if (type == TOK_IDENTIFIER || type == TOK_PRINT || type == TOK_IF || type == TOK_STRUCT
        || type == TOK_TYPE_INT || type == TOK_TYPE_FLOAT || type == TOK_TYPE_STRING
        || type == TOK_TYPE_BOOL ) {

        t.value = _strdup(value);
        t.owns_value = true;
    } else {
        t.value = value;
        t.owns_value = false;
    }
    t.row = row;
    t.col = col;
    return t;
}

Token Lexer::makeIntToken(TokenType type, unsigned long long val, int row, int col)
{
    Token t;
    t.type = type;
    t.int_value = val;
    t.row = row;
    t.col = col;
    return t;
}

Token Lexer::makeFloatToken(TokenType type, float val, int row, int col)
{
    Token t;
    t.type = type;
    t.float32_value = val;
    t.row = row;
    t.col = col;
    return t;
}

Token Lexer::stringToken(int row, int col)
{
    const char* startPtr = Source + Pos+1; // start after quote
    while (Pos < size && Source[Pos+1] != '"') {
        get_and_advance();
    }

    if (Pos >= size) {
        fprintf(stderr, "Unterminated string literal\n");
        return makeToken(TOK_UNKNOWN, "", row, col);
    }

    size_t len = (Source + Pos+1) - startPtr;
    get_and_advance();
    get_and_advance();

    unsigned char* str = (unsigned char*)malloc(len);
    memcpy(str, startPtr, len);

    Token t;
    t.type = TOK_STRING;
    t.string_value.data = str;
    t.string_value.count = len;
    t.row = row;
    t.col = col;

    return t;
}

Token Lexer::numberToken(char first, int row, int col)
{
    static char num_str_buffer[MAX_NUM_STR_LEN];

    int buffer_idx = 0;

    if (buffer_idx < MAX_NUM_STR_LEN - 1) {
        num_str_buffer[buffer_idx++] = first;
    } else {
        fprintf(stderr, "Lexer Error: Number literal too long at line %d, col %d\n", row, col);
        return makeToken(TOK_UNKNOWN, "", row, col);
    }

    // Parse the integer part of the number
    while (Pos < size && isNumeric(Source[Pos])) {
        if (buffer_idx < MAX_NUM_STR_LEN - 1) {
            num_str_buffer[buffer_idx++] = get_and_advance();
        } else {
            fprintf(stderr, "Lexer Error: Number literal too long at line %d, col %d\n", row, col);
            while (Pos < size && isNumeric(Source[Pos])) get_and_advance();
            return makeToken(TOK_UNKNOWN, "", row, col);
        }
    }

    // check for a decimal point (for float type)
    if (Pos < size && Source[Pos] == '.') {
        if (buffer_idx < MAX_NUM_STR_LEN - 1) {
            num_str_buffer[buffer_idx++] = get_and_advance(); // Consume the '.'
        } else {
            fprintf(stderr, "Lexer Error: Number literal too long (decimal part) at line %d, col %d\n", row, col);
            return makeToken(TOK_UNKNOWN, "", row, col);
        }

        // since its a float type parse  fractional part
        bool has_fractional_digits = false;
        while (Pos < size && isNumeric(Source[Pos])) {
            if (buffer_idx < MAX_NUM_STR_LEN - 1) {
                num_str_buffer[buffer_idx++] = get_and_advance();
                has_fractional_digits = true;
            } else {
                fprintf(stderr, "Lexer Error: Number literal too long (fractional part) at line %d, col %d\n", row, col);
                while (Pos < size && isNumeric(Source[Pos])) get_and_advance();
                return makeToken(TOK_UNKNOWN, "", row, col);
            }
        }

        if (!has_fractional_digits && num_str_buffer[buffer_idx - 1] == '.') {
            fprintf(stderr, "Lexer Error: Malformed float literal (missing fractional digits) at line %d, col %d\n", row, col);
            // exit(1); // lets not exit like this its bad
            return makeToken(TOK_UNKNOWN, "", row, col);
        }

        num_str_buffer[buffer_idx] = '\0';

        // Convert to float using strtof
        char* end_ptr;
        float val = strtof(num_str_buffer, &end_ptr); // Use strtof for float conversion

        if (*end_ptr != '\0') {
            fprintf(stderr, "Lexer Error: Invalid float literal conversion at line %d, col %d\n", row, col);
            return makeToken(TOK_UNKNOWN, num_str_buffer, row, col);
        }

        return makeFloatToken(TOK_FLOAT, val, row, col); // Pass float value
    }

    // If no decimal point, it's an integer
    num_str_buffer[buffer_idx] = '\0';

    char* end_ptr;
    unsigned long long val = strtoull(num_str_buffer, &end_ptr, 10);

    if (*end_ptr != '\0') {
        fprintf(stderr, "Lexer Error: Invalid integer literal conversion at line %d, col %d\n", row, col);
        return makeToken(TOK_UNKNOWN, num_str_buffer, row, col);
    }

    return makeIntToken(TOK_NUMBER, val, row, col);
}

// Token Lexer::floatToken(char first, int row, int col)
// {
// }

Token Lexer::identifierToken(char first, int row, int col)
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

    printf("ident: %s\n", ident);

    if (strcmp(ident, "printf") == 0) type = TOK_PRINT;
    else if (strcmp(ident, "if") == 0) type = TOK_IF;
    else if (strcmp(ident, "else") == 0) type = TOK_ELSE;
    else if (strcmp(ident, "struct") == 0) type = TOK_STRUCT;
    else if (strcmp(ident, "int") == 0) type = TOK_TYPE_INT;
    else if (strcmp(ident, "float") == 0) type = TOK_TYPE_FLOAT;
    else if (strcmp(ident, "string") == 0) type = TOK_TYPE_STRING;
    else if (strcmp(ident, "bool") == 0) type = TOK_TYPE_BOOL;

    Token t = makeToken(type, ident, row, col);

    free(ident);
    return t;
}


Token Lexer::nextToken()
{

    skipUnwantedChar();

    if (Pos >= size) {
        return makeToken(TOK_END_OF_FILE, "EOF", Row, Col);
    }
    int sRow = Row;
    int sCol = Col;

    char c = get_and_advance();

    // the String[Pos] pointer at this point is no longer on the same character as c above but the next character. This makes it simpler

    switch (c) {
        case '(': return makeToken(TOK_LPAREN, "(", sRow, sCol);
        case ')': return makeToken(TOK_RPAREN, ")", sRow, sCol);
        case '{': return makeToken(TOK_LCURLY_PAREN, "{", sRow, sCol);
        case '}': return makeToken(TOK_RCURLY_PAREN, "}", sRow, sCol);
        case '[': return makeToken(TOK_LBRACKET, "[", sRow, sCol);
        case ']': return makeToken(TOK_RBRACKET, "]", sRow, sCol);
        case ':': return makeToken(TOK_COLON, ":", sRow, sCol);
        case ';': return makeToken(TOK_SEMICOLON, ";", sRow, sCol);
        case ',': return makeToken(TOK_COMMA, ",", sRow, sCol);
        case '\'':return makeToken(TOK_BACKTICK, "'", sRow, sCol);
        case '+': return makeToken(TOK_PLUS, "+", sRow, sCol);
        case '-': return makeToken(TOK_MINUS, "-", sRow, sCol);
        case '*':
            if(match_and_advance('/')) return makeToken(TOK_R_MULTILINE_COMMENT, "*/", sRow, sCol);
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
                return makeToken(TOK_QUOTATION, "\"", sRow, sCol);
            return stringToken(Row, Col);
        case '/':
            if (match_and_advance('/')) return makeToken(TOK_COMMENT, "//", sRow, sCol);
            if(match_and_advance('*')) return makeToken(TOK_L_MULTILINE_COMMENT, "/*", sRow, sCol);
            return makeToken(TOK_SLASH, "/", sRow, sCol);
            break;


    }

    if (isNumeric(c)) return numberToken(c, sRow, sCol);
    if (isAlpha(c)) return identifierToken(c, sRow, sCol);

    // nothing matches
    return makeToken(TOK_UNKNOWN, "", sRow, sCol);

}

Token Lexer::peekNextToken()
{
    if(has_peeked) return peeked_token;

    size_t origPos = Pos;
    int origRow = Row;
    int origCol = Col;

    peeked_token = nextToken();
    has_peeked = true;

    Pos = origPos;
    Row = origRow;
    Col = origCol;

    return peeked_token;
}


// move this to tools.h !!!

FileBuffer read_entire_file(const char *path)
{
    FileBuffer result = {};
    FILE *f = NULL;
    fopen_s(&f, path, "rb"); // fopen_s cause fopen is depreacted vs throws error
    if (!f) {
        fprintf(stderr, "Could not open file: %s\n", path);
        return result;
    }

    struct stat st;
    if (stat(path, &st) != 0) {
        fprintf(stderr, "Could not stat file: %s\n", path);
        fclose(f);
        return result;
    }
    result.size = st.st_size;

    result.data = (uint8_t*)malloc(result.size);
    if (!result.data) {
        fprintf(stderr, "Memory allocation failed.\n");
        fclose(f);
        return result;
    }

    fread(result.data, 1, result.size, f);
    fclose(f);
    return result;
}

