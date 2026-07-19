// #ifdef _WIN32
// #define WIN32_LEAN_AND_MEAN
// #define NOMINMAX
// #include <windows.h>
// #endif

#ifdef AST_NEW
#undef AST_NEW
#endif

#define AST_NEW(pool, type) ([&]() -> type* {           \
    assert(pool != nullptr && "Pool must not be null"); \
    void *mem =  pool_alloc_debug(pool, sizeof(type), #type, "INTERP");        \
    type *node = new (mem) type(pool);                  \
    return node;                                        \
}())


inline void __strncpy(char *input, const char *filename, size_t bytes) {
#ifdef _WIN32
    strncpy_s(input, bytes, filename, _TRUNCATE);
#elif __linux__
    strncpy(input, filename, bytes);
    input[bytes - 1] = '\0';
#endif
}

inline void __strcpy(char *base_name, const char *input_path, size_t bytes){
#ifdef _WIN32
    strcpy_s(base_name, bytes, input_path);
#elif __linux__
    strcpy(base_name, input_path);
#endif
}

inline char *get_slash(char *base_name){

#ifdef _WIN32
    char *slash = strrchr(base_name, '/');
    // char *slash = strrchr(base_name, '\\');
#elif __linux__
    char *slash = strrchr(base_name, '/');
#endif
    return slash;
}


Pax_Interp::Pax_Interp()
{
   pool = nullptr;
   type = nullptr;
   lexer = nullptr;
   parser = nullptr;
   code_manager = nullptr;
   c_converter = nullptr;
   current_file = nullptr;

}

Pax_Interp::~Pax_Interp() {
}

inline void backslash_to_frontslash(char *path){
    if (!path) return;
    for (char *p = path; *p; p++) {
        if (*p == '\\') *p = '/';
    }
}

void Pax_Interp::parse_filename(const char *filename){

    // copy the file path
    __strncpy(input_path, filename, sizeof(input_path));

    backslash_to_frontslash(input_path);

    // make base name
    __strcpy(base_name, input_path, sizeof(base_name));

    char *dot = strrchr(base_name, '.');
    if(strcmp(dot+1,"pax") && strcmp(dot+5, "\0")==0){
        printf("Incorrect extenstion.\n");
        // exit(1);
    }
    if (dot) *dot = 0;

    char *slash = get_slash(base_name);

    const char *name_only = (slash) ? slash + 1 : base_name;

    // copy just filename for naming the compiled generated code in linux side
    __strncpy(file_name_only, name_only, sizeof(file_name_only));

}

char *Pax_Interp::get_absolute_path(const char *path) {
#ifdef _WIN32
    char abs_path[_MAX_PATH];
    char *resolved = _fullpath(abs_path, path, sizeof(abs_path));
    assert(resolved && "_fullpath failed");

    backslash_to_frontslash(abs_path);

    return pool_strdup(pool, abs_path);
#else
    char *resolved = realpath(path, NULL);
    if(!resolved) return nullptr;
    // assert(resolved && "realpath() failed");

    char *result = pool_strdup(pool, resolved);
    free(resolved);
    return result;
#endif
}


char *Pax_Interp::get_directory(const char *filepath) {
    const char *pos = strrchr(filepath, '/');

    if (pos) {
        size_t len = pos - filepath;
        char *out = (char*)malloc(len + 1);
        memcpy(out, filepath, len);
        out[len] = '\0';
        return out;
    }

    return pool_strdup(pool, ".");
}

inline bool is_absolute_path(const char *path) {
    if (!path || !*path) return false;

    if(isAlpha(path[0]) && path[1] == ':' && (path[2] == '\\' || path[2] == '/')) {
        return true;
    }

    return false;
}

char *Pax_Interp::resolve_import_path(const char *import_path, const char *current_file) {
    char *current_dir = get_directory(current_file);

    char *temp = c_concat3(current_dir, "/", import_path);

    // char *abs = get_absolute_path(temp);
    char *abs = nullptr;
    if (is_absolute_path(import_path)) {
        abs = get_absolute_path(import_path);
        return abs;
    } else {
        abs = get_absolute_path(temp);
    }
    free(current_dir);
    free(temp);

    return abs;
}

Ast_Block *Pax_Interp::parse_file(const char *filename, bool skip_main_check) {
    char *abs_path = get_absolute_path(filename);

    FileBuffer buf = read_entire_file(filename);
    if (!buf.data) {
        printf("\nCould not open file: %s\n", filename);
        exit(1);
    }

    current_file = (const char *) abs_path;

    Lexer temp_lexer((const char*)buf.data, buf.size, pool);
    Parser temp_parser(&temp_lexer, this);
    Ast_Block *parsed_ast = temp_parser.parseProgram(ast, skip_main_check);
    parsed_ast->file_name = (const char *) abs_path;

    free(buf.data);

    return parsed_ast;
}

void Pax_Interp::load_imports(Ast_Block *module_ast, const char *module_path) {
    for (int i = 0; i < module_ast->imports.count; i++) {
        Ast_Import *imp = module_ast->imports.data[i];
        char *imp_path = resolve_import_path(imp->import_path, module_path);
        load_and_parse_module(imp_path);
    }
}

bool Pax_Interp::init(const char *entry_file) {

    all_unique_import_paths = pool;
    ast = AST_NEW(pool, Ast_Block);

    parse_filename(entry_file);

    Ast_Block *entry_ast = parse_file(entry_file, false); // only allow entry file to have main entry point
    if (!entry_ast) {
        return false;
    }
    ast = AST_NEW(pool, Ast_Block);
    char *abs_path = get_absolute_path(entry_file);

    all_unique_import_paths.push_back(abs_path);
    load_imports(entry_ast, abs_path);

    FOR(entry_ast->statements){
        ast->statements.push_back(it);
    }

    ast->file_name = abs_path;

    return true;
}

bool Pax_Interp::does_import_already_exist(char *path){

    FOR(all_unique_import_paths){
        if(strcmp(it, path) == 0) return true;
    }
    return false;
}

Ast_Block *Pax_Interp::load_and_parse_module(const char *filename) {
    char *abs_path = get_absolute_path(filename);

    if(does_import_already_exist(abs_path) == true){
        // printf("\n>>>>Module already imported before: %s\n\n", abs_path);
        return {};
    }

    // printf("<<<Inside interp>>> Loading module: %s\n", filename);


    Ast_Block *mod_ast = parse_file(filename, true); // kind of an hack we just tell the parser to forbid a main entry point in modules
    if (!mod_ast) {
        free(abs_path);
        return nullptr;
    }

    all_unique_import_paths.push_back(abs_path);

    load_imports(mod_ast, abs_path);

    return mod_ast;
}


void Pax_Interp::printLexer(const char *filename) {
    FileBuffer buf = read_entire_file(filename);
    if (!buf.data) {
        printf("Failed to read file: %s\n", filename);
        return;
    }

    Lexer lexer((const char*)buf.data, buf.size, pool);
    printLex(buf, pool);

    free(buf.data);

}

bool Pax_Interp::run_frontend() {
    TIME_SCOPE("\n\tFrontend finished in");

    code_manager->resolve_idents(ast);

    code_manager->resolve_unresolved_vars();
    code_manager->resolve_unresolved_calls();
    code_manager->resolve_unresolved_types();
    code_manager->resolve_unresolved_arrays();
    code_manager->resolve_unresolved_member_accesses();

    code_manager->is_everything_resolved();

    if (code_manager->count_errors != 0) {
        printf("\nErrors in code manager. Exiting.\n");
        // return false
        exit(1);
    }
    code_manager->infer_types_block(ast);

    if (code_manager->count_errors != 0) {
        printf("\nErrors in code manager. Exiting.\n");
        exit(1);
        // return false;
    }

    return true;
}

void Pax_Interp::generate_cpp() {
    TIME_SCOPE("\t -Time to output c code");

    char cpp_name[256];
    snprintf(cpp_name, sizeof(cpp_name), "%s.cpp", base_name);

    c_converter->generate_cpp_code(cpp_name, ast);
}

void Pax_Interp::runCompiler(char *command)
{
   int result = system(command);
#ifdef _WIN32
    if (result != 0) {
        printf("Compilation failed with error code: %d\n", result);
        exit(1);
    }
#else
    if (WEXITSTATUS(result) != 0) {
        printf("Compilation failed.\n");

        exit(1);
    }
#endif

}

void Pax_Interp::compile_cpp() {

    TIME_SCOPE("\t -C compilation");

    char command[256];
#ifdef _WIN32                               /* vvvvvvvvvvvvvv @Temporary */
    snprintf(command, sizeof(command), "cl.exe /Z7 /w /Od /EHsc /nologo %s.cpp /link /INCREMENTAL:NO", base_name);
    // printf("Running C compiler: %s\n", command);
    runCompiler(command);
#else
    snprintf(command, sizeof(command), "g++ -w -o %s.exe %s.cpp", file_name_only, base_name);
    printf("Running C compiler: %s\n", command);
    runCompiler(command);
#endif
}

void Pax_Interp::release() {
    if (pool) {
        pool_release(pool);
    }
}
