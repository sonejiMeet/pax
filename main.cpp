#include "token.h"
#include "parser.h"
#include "ast_printer.h"
#include <chrono>

#include <crtdbg.h>  // for detecting mem leaks

inline void printLex(FileBuffer buf);
inline void printParsing(FileBuffer buf);

int main(int argc, char **args) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <file>\n", args[0]);
        return 1;
    }

    auto start = std::chrono::high_resolution_clock::now();

    FileBuffer buf = read_entire_file(args[1]);
    if (!buf.data) return 1;

    printLex(buf);
    // printParsing(buf);

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;

    printf("\nTime: %.6f seconds\n", elapsed.count());
    printf("DONE. exiting..\n\n");
    free(buf.data);

    _CrtDumpMemoryLeaks();
    return 0;
}
void freeToken(Token& t) {
    switch (t.type) {
        case TOK_IDENTIFIER:
        case TOK_PRINT:
        case TOK_IF:
        case TOK_ELSE:
        case TOK_STRUCT:
        case TOK_TYPE_INT:
        case TOK_TYPE_FLOAT:
        case TOK_TYPE_STRING:
        case TOK_TYPE_BOOL:
        case TOK_UNKNOWN:
            if (t.value) {
                free((void*)t.value);
                t.value = nullptr;
            }
            break;

        case TOK_STRING:
            if (t.string_value.data) {
                free(t.string_value.data);
                t.string_value.data = nullptr;
                t.string_value.count = 0;
            }
            break;

        default:
            // numbers and operators have nothing to free
            break;
    }
}



inline void printLex(FileBuffer buf){

    Lexer lexer((const char*)buf.data, buf.size);
    std::vector<Token> tokens;

    while (true) {
        Token tok = lexer.nextToken();

        printf("[%d:%d]\tToken: %s\t ", tok.row, tok.col, tokenTypeToString(tok.type));

        switch (tok.type) {
            case TOK_NUMBER:
                printf("Value: \"%llu\"\n", tok.int_value);
                break;
            case TOK_FLOAT:
                printf("Value: \"%.5f\"\n", tok.float32_value);
                break;

            case TOK_STRING:
                printf("Value: \"%.*s\"\n", (int)tok.string_value.count, tok.string_value.data);
                break;
            case TOK_IDENTIFIER:
            default:
                // For simple tokens (operators, etc.)
                if (tok.value) {
                    printf("Value: \"%s\"\n", tok.value);
                } else {
                    printf("\n");
                }
                break;
        }

        if (tok.type == TOK_END_OF_FILE) {
            freeToken(tok);
            break;
        }
        freeToken(tok);

    }

    printf("\n");

}

inline void printParsing(FileBuffer buf){

    Lexer lexer((const char*)buf.data, buf.size);

    Parser parser(&lexer);

    ASTNode* ast = parser.parseProgram();
    printAST(ast);

}