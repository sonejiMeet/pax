#include "parser.h"
#include "ast_printer.h"
// #include <chrono>

int main(int argc, char **args) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <file>\n", args[0]);
        return 1;
    }

    FileBuffer buf = read_entire_file(args[1]);
    if (!buf.data) return 1;

    Lexer lexer((const char*)buf.data, buf.size);

    Parser parser(&lexer);

    ASTNode* ast = parser.parseExpression();
    // ASTNode* ast = parser.parseProgram();
    printAST(ast);
    // printf("Parsed program with %zu statements.\n", ast->children.size());

    free(buf.data);
    return 0;
}

// void printTokens(const Token& token){


// }

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
//         printf("[%d:%d]\tToken: %s\t ", tok.row, tok.col, tokenTypeToString(tok.type));

//         switch (tok.type) {
//             case TOK_NUMBER:
//                 printf("Value: \"%llu\"\n", tok.int_value);
//                 break;
//             case TOK_STRING:
//                 printf("Value: \"%.*s\"\n", (int)tok.string_value.count, tok.string_value.data);
//                 break;
//             case TOK_IDENTIFIER:
//             case TOK_PRINT:
//             case TOK_IF:
//             case TOK_STRUCT:
//             case TOK_COMMENT:
//             case TOK_L_MULTILINE_COMMENT:
//             case TOK_R_MULTILINE_COMMENT:
//             default:
//                 // For simple tokens (operators, etc.)
//                 if (tok.value) {
//                     printf("Value: \"%s\"\n", tok.value);
//                 } else {
//                     printf("\n");
//                 }
//                 break;
//         }

//         if (tok.type == TOK_END_OF_FILE) break;
//     }
//     // printf("\n\nVector size %zd SizeOf %zd\n", tokens.size(), sizeof(tokens));

//     auto end = std::chrono::high_resolution_clock::now();
//     std::chrono::duration<double> elapsed = end - start;

//     printf("Time: %.6f seconds\n", elapsed.count());

//     free(buf.data);
//     return 0;
// }
