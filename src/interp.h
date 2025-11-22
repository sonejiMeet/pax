#pragma once

#include "token.h"
#include "parser.h"
#include "ast.h"
#include "code_manager.h"
#include "c_converter.h"
#include "tools.h"
#include "pool.h"

#include <chrono>
#include <unordered_map>
#include <vector>


struct Pax_Interp {
    Pax_Interp();
    ~Pax_Interp();

    Pool *pool;
    Def_Type *type;
    Lexer *lexer;
    Parser *parser;
    CodeManager *code_manager;
    C_Converter *c_converter;

    Ast_Block *ast;

    std::unordered_map<std::string, Ast_Block *> loaded_modules;
    std::vector<std::string> module_parse_order;

    const char *current_file;

    char input_path[256];
    char base_name[256];
    char file_name_only[256];

    void parse_filename(const char *filename);
    bool init(const char *entry_file);

    Ast_Block *parse_file(const char *filename, bool skip_main_check);
    void load_imports(Ast_Block *module_ast, const char *module_path);

    Ast_Block *load_and_parse_module(const char *filename);
    Ast_Block *merge_all_modules();

    // path helpers
    char *get_absolute_path(const char *path);
    char *get_directory(const char *filepath);
    char *resolve_import_path(const char *import_path, const char *current_file);

    void printLexer(const char *filename);

    void run_frontend();
    void generate_cpp();

    void runCompiler(char * command);
    void compile_cpp();

    void release();
};
