#define _CRTDBG_MAP_ALLOC
#include <cstdlib>
#include <crtdbg.h>



#include "dbgnew.h"

#include "token.h"
#include "parser.h"
#include "ast_printer.h"
#include <chrono>

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

    // printLex(buf);
    printParsing(buf);

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;

    printf("\nTime: %.6f seconds\n", elapsed.count());
    printf("DONE. exiting..\n\n");

    free(buf.data);

    _CrtDumpMemoryLeaks();
    return 0;
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

void freeAST(ASTNode* node) {
    if (!node) return;

    // 1. Free children recursively
    for (ASTNode* child : node->children) {
        freeAST(child);
    }

    // 2. Free Token memory if allocated dynamically
    if (node->token) {
        Token* tok = node->token;

        if (tok->type == TOK_STRING && tok->string_value.data) {
            delete[] tok->string_value.data;  // free string buffer
            tok->string_value.data = nullptr;
        }

        if ((tok->type == TOK_IDENTIFIER || tok->type == TOK_PRINT ||
             tok->type == TOK_IF || tok->type == TOK_TYPE_INT ||
             tok->type == TOK_TYPE_FLOAT || tok->type == TOK_TYPE_STRING ||
             tok->type == TOK_TYPE_BOOL) &&
            tok->value) {
            delete[] tok->value;               // free identifier string
            tok->value = nullptr;
        }

        delete tok;  // delete Token object itself
    }

    // 3. Delete the ASTNode
    delete node;
}


inline void printParsing(FileBuffer buf){

    Lexer lexer((const char*)buf.data, buf.size);

    Parser parser(&lexer);

    ASTNode* ast = parser.parseProgram();
    printAST(ast);

    delete ast;
    // freeAST(ast);
}