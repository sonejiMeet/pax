#pragma once

#include "token.h"
#include "parser.h"
#include "ast.h"
#include "code_manager.h"
#include "c_converter.h"
#include "tools.h"
#include "pool.h"

#include <chrono>

#ifdef _WIN32
#include <windows.h>
#endif

struct Pax_Interp {
    Pax_Interp();
    ~Pax_Interp();

    Pool pool;
    Def_Type type;
    FileBuffer buf;

    Lexer *lexer;
    Parser *parser;
    Ast_Block *ast;

    char input_path[256];
    char base_name[256];
    char file_name_only[256];
    void parse_filename(const char *filename);
    bool init(const char* filename);

    void printLexer(const char *filename);

    void run_frontend();
    void generate_cpp();

    void runCompiler(char * command);
    void compile_cpp();

    void release();
};
