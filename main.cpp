#define _CRTDBG_MAP_ALLOC // for mem leaks

#include <cstdlib>
#include <crtdbg.h>

#include "token.h"
#include "parser.h"
#include "ast_printer.h"

#include "code_manager.h"

#include "c_converter.h"

#include <chrono>

inline void printLex(FileBuffer buf);
inline void printParsing(FileBuffer buf);

int main(int argc, char **args) {

    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF ); // to avoid when there are

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

    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_DEBUG);
    // _CrtDumpMemoryLeaks();
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
                printf("Value: \"%f\"\n", tok.float32_value);
                break;

                // printf("Value: \"%c\"\n",(char)tok.value);
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

    Ast_Block* ast = parser.parseProgram();
    // printAst(ast);


    CodeManager cm;
    cm.init();

    // 1) resolve identifiers and populate symbol table/scopes
    cm.resolve_idents(ast);

    // 2) run type inference / checking
    cm.infer_types_block(ast);

    if (cm.get_count_errors() != 0) {
        return;
    }

    generate_cpp_code("generated.cpp", ast);
    printf("\n\nC code generated -> generated.cpp\n");

    delete ast;

}