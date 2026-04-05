#pragma once

struct Pool;
struct Def_Type;
struct Lexer;
struct Parser;
struct CodeManager;
struct C_Converter;
struct Ast_Block;

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

    Array<char *> all_unique_import_paths;

    const char *current_file;

    char input_path[256];
    char base_name[256];
    char file_name_only[256];

    void parse_filename(const char *filename);
    bool init(const char *entry_file);

    Ast_Block *parse_file(const char *filename, bool skip_main_check);
    void load_imports(Ast_Block *module_ast, const char *module_path);

    bool does_import_already_exist(char*path);
    Ast_Block *load_and_parse_module(const char *filename);
    Ast_Block *merge_all_modules();

    // path helpers
    char *get_absolute_path(const char *path);
    char *get_directory(const char *filepath);
    char *resolve_import_path(const char *import_path, const char *current_file);

    void printLexer(const char *filename);

    bool run_frontend();
    void generate_cpp();

    void runCompiler(char *command);
    void compile_cpp();

    void release();
};
