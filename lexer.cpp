#include "lexer.h"

#include <vector> // temp
#include <chrono>



Token Lexer::makeToken(TokenType type, const char* value, int row, int col) {
    Token t;
    t.type = type;
    if (type == TOK_IDENTIFIER || type == TOK_PRINT || type == TOK_IF || type == TOK_STRUCT) {

        char* copy = _strdup(value);
        t.value = copy;
    } else {
        t.value = value;
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

Token Lexer::makeFloatToken(TokenType type, double val, int row, int col)
{
    Token t;
    t.type = type;
    t.float64_value = val;
    t.row = row;
    t.col = col;
    return t;
}

Token Lexer::stringToken(int row, int col)
{
    const char* startPtr = Source + Pos; // start after quote
    while (Pos < size && Source[Pos] != '"') {
        get_and_advance();
    }

    if (Pos >= size) {
        fprintf(stderr, "Unterminated string literal\n");
        return makeToken(TOK_UNKNOWN, "", row, col);
    }

    size_t len = (Source + Pos) - startPtr;
    get_and_advance();

    // make a copy of the string content
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
    unsigned long long val = first - '0'; // convert char to int
    while (Pos < size && isdigit(Source[Pos])) {
        val = val * 10 + (get_and_advance() - '0');
    }
    return makeIntToken(TOK_NUMBER, val, row, col);
}

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

    if (strcmp(ident, "printf") == 0) type = TOK_PRINT;
    else if (strcmp(ident, "if") == 0) type = TOK_IF;
    else if (strcmp(ident, "struct") == 0) type = TOK_STRUCT;

    Token t = makeToken(type, ident, row, col);

    free(ident);

    return t;
}


Token Lexer::nextToken()
{
    if(has_peeked) {
        has_peeked = false;
        return peeked_token;
    }
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

            break;


    }

    if (isNumeric(c)) return numberToken(c, sRow, sCol);
    if (isAlpha(c)) return identifierToken(c, sRow, sCol);

    // nothing matches
    return makeToken(TOK_UNKNOWN, "", sRow, sCol);

}

Token Lexer::peekNextToken(){
    if(has_peeked) return peeked_Token;

    size_t origPos = Pos;
    int origRow = Row;
    int origCol = Col;


}

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


// int main(int argc, char **args)
// {
//     if (argc < 2) {
//         fprintf(stderr, "Usage: %s <file>\n", args[0]);
//         return 1;
//     }

//     auto start = std::chrono::high_resolution_clock::now();

//     FileBuffer buf = read_entire_file(args[1]);
//     if (!buf.data) {
//         return 1;
//     }

//     Lexer lexer((const char*)buf.data, buf.size);
//     std::vector<Token> tokens;

//     while (true) {
//         Token tok = lexer.nextToken();
//         // // tokens.push_back(tok);
//         // printf("[%d:%d]\tToken: %s\t ", tok.row, tok.col, tokenTypeToString(tok.type));

//         // switch (tok.type) {
//         //     case TOK_NUMBER:
//         //         printf("Value: %llu\n", tok.int_value);
//         //         break;
//         //     case TOK_STRING:
//         //         printf("Value: \"%.*s\"\n", (int)tok.string_value.count, tok.string_value.data);
//         //         break;
//         //     case TOK_IDENTIFIER:
//         //     case TOK_PRINT:
//         //     case TOK_IF:
//         //     case TOK_STRUCT:
//         //     case TOK_COMMENT:
//         //     case TOK_L_MULTILINE_COMMENT:
//         //     case TOK_R_MULTILINE_COMMENT:
//         //     default:
//         //         // For simple tokens (operators, etc.)
//         //         if (tok.value) {
//         //             printf("Value: \"%s\"\n", tok.value);
//         //         } else {
//         //             printf("\n");
//         //         }
//         //         break;
//         // }

//         if (tok.type == TOK_END_OF_FILE) break;
//     }
//     // printf("\n\nVector size %zd SizeOf %zd\n", tokens.size(), sizeof(tokens));

//     auto end = std::chrono::high_resolution_clock::now();
//     std::chrono::duration<double> elapsed = end - start;

//     printf("Time: %.6f seconds\n", elapsed.count());

//     free(buf.data);
//     return 0;
// }


// #include <cstdio>
// #include <fstream>
// #include <cstring>
// #include <vector> // temp
// #include <chrono> //temp

// #include "token.h"
// #include "tools.h"

// struct Lexer {
//     std::string Source;
//     size_t Pos;

//     int Row;
//     int Col;

//     Lexer(const std::string &src) {
//         Source = src;
//         Pos = 0;
//         Row = 1;
//         Col = 1;
//     }

//     char advance_to_next_char() {
//         char c = Source[Pos++];
//         if(c == '\n'){
//             Row++;
//             Col = 1;
//         }
//         else {
//             Col++;
//         }
//         return c;
//     }

//     bool match(char expected)
//     {
//         if (Pos >= Source.size() || Source[Pos] != expected) return false;
//         advance_to_next_char();
//         return true;
//     }

//     Token makeToken(TokenType type, const std::string &value, int row, int col)
//     {
//         Token t;

//         t.type = type;
//         t.value = value;
//         t.row = row;
//         t.col = col;

//         return t;
//     }

//     void skipUnwantedChar()
//     {
//         while (Pos < Source.size() && (Source[Pos] == ' ' || Source[Pos] == '\t' || Source[Pos] == '\r'|| Source[Pos] == '\n')) {
//             Col++;
//             Pos++;
//         }
//     }

//     Token stringToken(int row, int col) {
//         size_t start = Pos;
//         while (Pos < Source.size() && Source[Pos] != '"') advance_to_next_char();
//         std::string value = Source.substr(start, Pos - start);
//         advance_to_next_char();

//         return makeToken(TOK_STRING, value, row, col);
//     }

//     Token numberToken(char first, int row, int col) {
//         std::string num;
//         num += first;
//         while (Pos < Source.size() && isdigit(Source[Pos])) {
//             num += advance_to_next_char();
//         }

//         return makeToken(TOK_NUMBER, num, row, col);
//     }

//     Token identifierToken(char first, int row, int col) {
//         std::string ident;
//         ident += first;
//         while (Pos < Source.size() && (isAlphaNumeric(Source[Pos]) || Source[Pos] == '_')) {
//             ident += advance_to_next_char();
//         }

//         if (ident == "printf") {
//             return makeToken(TOK_PRINT, ident, row, col);
//         }

//         if (ident == "if"){
//             if (Source[Pos] =='('){
//                 return makeToken(TOK_IF, ident, row, col);
//             }
//         }

//         // if (Source[Pos] =='('){
//         //     return makeToken(TOK_FUNCTION, ident, row, col);
//         // }
//         // else if (Source[Pos+1] == ' ') {
//         //     skipUnwantedChar();
//         //     if(Source[Pos == '('])
//         //     return makeToken(TOK_FUNCTION, ident, row, col);

//         // }

//         return makeToken(TOK_IDENTIFIER, ident, row, col);

//     }

//     Token nextToken() {
//         skipUnwantedChar();

//         if (Pos >= Source.size()) {
//             return makeToken(TOK_END_OF_FILE, "EOF", Row, Col);

//         }
//         char c = advance_to_next_char();
//         int sRow = Row;
//         int sCol = Col;


//         switch (c) {
//             case '(': return makeToken(TOK_LPAREN, "(", sRow, sCol);
//             case ')': return makeToken(TOK_RPAREN, ")", sRow, sCol);
//             case '{': return makeToken(TOK_LCURLY_PAREN, "{", sRow, sCol);
//             case '}': return makeToken(TOK_RCURLY_PAREN, "}", sRow, sCol);
//             case '[': return makeToken(TOK_LBRACKET, "[", sRow, sCol);
//             case ']': return makeToken(TOK_RBRACKET, "]", sRow, sCol);
//             case ':': return makeToken(TOK_COLON, ":", sRow, sCol);
//             case ';': return makeToken(TOK_SEMICOLON, ";", sRow, sCol);
//             case ',': return makeToken(TOK_COMMA, ";", sRow, sCol);
//             case '+': return makeToken(TOK_PLUS, "+", sRow, sCol);
//             case '-': return makeToken(TOK_MINUS, "-", sRow, sCol);
//             case '*':
//                 if(match('/')) return makeToken(TOK_R_MULTILINE_COMMENT, "*/", sRow, sCol);
//                 return makeToken(TOK_STAR, "*", sRow, sCol);

//             case '=':
//                 if (match('=')) return makeToken(TOK_EQUAL, "==", sRow, sCol);
//                 return makeToken(TOK_ASSIGN, "=", sRow, sCol);
//             case '!':
//                 if (match('=')) return makeToken(TOK_NOT_EQUAL, "!=", sRow, sCol);
//                 break;
//             case '<':
//                 if (match('=')) return makeToken(TOK_LESS_EQUAL, "<=", sRow, sCol);
//                 return makeToken(TOK_LESS, "<", sRow, sCol);
//             case '>':
//                 if (match('=')) return makeToken(TOK_GREATER_EQUAL, ">=", sRow, sCol);
//                 return makeToken(TOK_GREATER, ">", sRow, sCol);
//             case '"':
//                 return stringToken(Row, Col);
//             case '/':
//                 if (match('/')) return makeToken(TOK_COMMENT, "//", sRow, sCol);
//                 if(match('*')) return makeToken(TOK_L_MULTILINE_COMMENT, "/*", sRow, sCol);

//                 break;


//         }

//         if (isNumeric(c)) return numberToken(c, sRow, sCol);
//         if (isAlpha(c)) return identifierToken(c, sRow, sCol);

//         return makeToken(TOK_UNKNOWN, "", sRow, sCol);

//     }
// };


// int main(int argc, char **argv) {


//     // we dont want this ugly and bad file access, read entire file and and operate on the memory !!!!!!
//     std::ifstream in(argv[1]);
//     if (!in) {
//         printf("Could not open file.\n");
//         return 1;
//     }

//     auto start = std::chrono::high_resolution_clock::now();

//     std::string src((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
//     Lexer lexer(src);

//     std::vector<Token> tokens;
//     Token tok;
//     do {
//         tok = lexer.nextToken();
//         tokens.push_back(tok);
//         printf("[Row:%d Col:%d]\ttoken: %s \tValue: \"%s\"\n",tok.row, tok.col, tokenTypeToString(tok.type), tok.value.c_str());
//     // get rid of this while loop
//     } while (tok.type != TOK_END_OF_FILE);

//     auto end = std::chrono::high_resolution_clock::now();
//     std::chrono::duration<double> elapsed = end - start;
//     printf("Time: %.6f seconds\n", elapsed.count());
//     return 0;
// }