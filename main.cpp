#include "token.h"
#include "parser.h"
#include "ast_printer.h"
#include "code_manager.h"
#include "c_converter.h"
#include "tools.h"

#include "pool.h"

#ifdef _WIN32
#include <windows.h>
    #ifdef _DEBUG
        #define _CRTDBG_MAP_ALLOC // for mem leaks
        #include <crtdbg.h>

        #define malloc(s) _malloc_dbg(s, _NORMAL_BLOCK, __FILE__, __LINE__)
        #define free(p) _free_dbg(p, _NORMAL_BLOCK)
    #endif

#endif


#include <cstdlib>
#include <chrono>


  //#define PRINT_LEX

#ifdef _DEBUG
int totalNbyte = 0;
#endif
int total_malloc = 0;

inline void printLex(FileBuffer buf, Pool *pool);
void runCompiler(char * command);

void* default_allocator(int mode, size_t size, size_t old_size,
                        void* old_memory, void* allocator_data, int options) {
    switch(mode) {
        case ALLOCATE: {
            void* ptr = calloc(size, 1);
            total_malloc += 1;
            assert(ptr && "Memeory allocation failed");
            return ptr;
        }
        // case RESIZE: {
        //     void* new_ptr = realloc(old_memory, size);
        //     if (!new_ptr) {
        //         // Handle allocation failure
        //         printf("Memory reallocation failed\n");
        //         exit(1); // or throw an exception
        //     }
        //     return new_ptr;
        // }
        // case FREE: return 0;
        // case FREE_ALL: return 0;
    }
    return 0;
}

int main(int argc, char **args) {

#ifdef _WIN32
#ifdef _DEBUG
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF ); // put it at start, when we want to exit(1) early. temporary!!!!!!

    //_CrtSetBreakAlloc(182);
#endif
#endif

    if (argc < 2) {
        printf("Usage: %s <file>.mylang\n", args[0]);
        return 1;
    }

    auto start = std::chrono::high_resolution_clock::now();

    Pool pool;
    pool_init(&pool);
    pool.block_allocator = default_allocator;

    FileBuffer buf = read_entire_file(args[1]);
    if (!buf.data) return 1;

#ifdef PRINT_LEX
    printLex(buf, &pool);

#else

    Lexer lexer((const char*)buf.data, buf.size, &pool);

    Parser parser(&lexer, &pool);

    Ast_Block* ast = parser.parseProgram();
    // printAst(ast);

    free(buf.data);

    {
        CodeManager cm(&pool);
        cm.init();
        cm.resolve_idents(ast); // resolve identifiers and populate symbol table/scopes
        cm.infer_types_block(ast); // run type inference / checking

        if (cm.get_count_errors() != 0) {
            printf("\nGot errors from code manager. Exiting...\n");
            exit(1); // exit unexpectedly
        }
    }

    char baseName[256]; // temporary?
    char tempName[256];

    {
        #ifdef _WIN32
            strncpy_s(tempName, args[1], sizeof(tempName));
        #elif __linux__
            strncpy(tempName, args[1], sizeof(tempName));
        #endif

        char* dot = strrchr(tempName, '.');
        if (dot != NULL) {
            *dot = '\0';
        }

        snprintf(baseName, sizeof(baseName), "%s.cpp", tempName);

    }

    auto end1 = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed1 = end1 - start;
    printf("\n\t -Time in frontend: %.6f seconds (lexer,parser,semantic checker)\n", elapsed1.count());

    {
        auto start3 = std::chrono::high_resolution_clock::now();

        generate_cpp_code(baseName, ast);
        printf("\nC code generated\n");

        auto end3 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> elapsed3 = end3 - start3;
        printf("\n\t -Time to ouput c code: %.6f seconds\n\n", elapsed3.count());
    }



    {
        auto start2 = std::chrono::high_resolution_clock::now();
#ifdef _WIN32
    char command[256];
    snprintf(command, sizeof(command), "cl.exe /Od /EHsc /nologo %s", baseName);
    printf("Running C compiler: %s\n", command);

    #ifndef _DEBUG // temporary!!!!
            runCompiler(command);
    #endif

#elif __linux__
    char command[256];
    snprintf(command, sizeof(command), "g++ -o %s %s", tempName, baseName);
    printf("Running C compiler: %s\n", command);
    system(command);
#endif

        auto end2 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> elapsed2 = end2 - start2;
        printf("\n\t -Time in c compiler: %.6f seconds\n", elapsed2.count());
    }

#endif // not defined PRINT_LEX
    pool_release(&pool);

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;

    printf("\nTotal Time: %.6f seconds\n", elapsed.count());
    printf("DONE. exiting..\n\n");

#ifdef _DEBUG
    _CrtMemState state;
    _CrtMemCheckpoint(&state); // snapshot current memory state
    _CrtMemDumpStatistics(&state); // print summary
    _CrtMemDumpAllObjectsSince(&state); // detailed list of allocations
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_DEBUG);
#endif

    printf("\nTotal mallocs called %d\n\n", total_malloc);
    return 0;
}

#ifdef _WIN32
void runCompiler(char * command){

    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    char cmdLine[256];
    snprintf(cmdLine, sizeof(cmdLine), "%s", command);

    if (!CreateProcessA( NULL, cmdLine, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi))
    {
        printf("CreateProcess failed (%lu)\n", GetLastError());
        exit(1);
    }

    WaitForSingleObject(pi.hProcess, INFINITE);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
}
#endif

inline void printLex(FileBuffer buf, Pool *pool){

    Lexer lexer((const char*)buf.data, buf.size, pool);

    while (true) {
        Token *tok = lexer.nextToken();
        printf("[%-2d:%-2d] Token: %-15s\t", tok->row, tok->col, tokenTypeToString(tok->type));

        switch (tok->type) {
            case TOK_NUMBER:
                printf("Value: \"%llu\"\n", tok->int_value);
                break;
            case TOK_FLOAT:
                printf("Value: \"%f\"\n", tok->float32_value);
                break;

                // printf("Value: \"%c\"\n",(char)tok.value);
            case TOK_STRING:
                printf("Value: \"%.*s\"\n", (int)tok->string_value.count, tok->string_value.data);
                break;
            case TOK_IDENTIFIER:
            default:
                // For simple tokens (operators, etc.)
                if (tok->value) {
                    printf("Value: \"%s\"\n", tok->value);
                } else {
                    printf("\n");
                }
                break;
        }

        if (tok->type == TOK_END_OF_FILE) {
            break;
        }
    }

    printf("\n");

}
