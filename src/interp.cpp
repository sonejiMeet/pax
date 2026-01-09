#include "interp.h"
// #include "ast_printer.h"

#ifdef _WIN32
#include <windows.h>
#endif

#define AST_NEW(pool, type) ([&]() -> type* { \
    assert(pool != nullptr && "Pool must not be null"); \
    void* mem = pool_alloc(pool, sizeof(type)); \
    type* node = new (mem) type(pool); \
    return node; \
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

void Pax_Interp::parse_filename(const char *filename){

    // copy the file path
    __strncpy(input_path, filename, sizeof(input_path));

    // make base name
    __strcpy(base_name, input_path, sizeof(base_name));

    char* dot = strrchr(base_name, '.');
    if (dot) *dot = 0;

    char *slash = get_slash(base_name);

    const char* name_only = (slash) ? slash + 1 : base_name;

    // copy just filename for naming the compiled generated code in linux side
    __strncpy(file_name_only, name_only, sizeof(file_name_only));

}

char *Pax_Interp::get_absolute_path(const char* path) {
#ifdef _WIN32
    char abs_path[_MAX_PATH];
    char *resolved = _fullpath(abs_path, path, sizeof(abs_path));
    assert(resolved && "_fullpath failed");

    return pool_strdup(pool, abs_path);   
#else
    char *resolved = realpath(path, NULL);

    assert(resolved && "realpath() failed");

    char *result = pool_strdup(pool, resolved);
    free(resolved);
    return result;
#endif
}


char *Pax_Interp::get_directory(const char* filepath) {
    const char* last = strrchr(filepath, '/');
    const char* last2 = strrchr(filepath, '\\');
    const char* pos = last ? last : last2;

    if (pos) {
        size_t len = pos - filepath;
        char* out = (char*)malloc(len + 1);
        memcpy(out, filepath, len);
        out[len] = '\0';
        return out;
    }

    return pool_strdup(pool, ".");
}

char* Pax_Interp::resolve_import_path(const char* import_path, const char* current_file) {
    char* current_dir = get_directory(current_file);

    char* temp = c_concat3(current_dir, "/", import_path);

    char* abs = get_absolute_path(temp);

    free(current_dir);
    free(temp);

    return abs;
}

Ast_Block* Pax_Interp::parse_file(const char* filename, bool skip_main_check) {
    char *abs_path = get_absolute_path(filename);

    FileBuffer buf = read_entire_file(filename);
    if (!buf.data) {
        printf("Failed to read file: %s\n", filename);
        return nullptr;
    }

    current_file = (const char *) abs_path;

    Lexer temp_lexer((const char*)buf.data, buf.size, pool);
    Parser temp_parser(&temp_lexer, this);
    Ast_Block* parsed_ast = temp_parser.parseProgram(skip_main_check);
    parsed_ast->file_name = (const char *) abs_path;

    free(buf.data);

    return parsed_ast;
}

void Pax_Interp::load_imports(Ast_Block *module_ast, const char *module_path) {
    for (int i = 0; i < module_ast->imports.count; i++) {
        Ast_Import* imp = module_ast->imports.data[i];
        char *imp_path = resolve_import_path(imp->import_path, module_path);
        load_and_parse_module(imp_path);
    }
}

bool Pax_Interp::init(const char* entry_file) {
    parse_filename(entry_file);

    Ast_Block* entry_ast = parse_file(entry_file, false); // only allow entry file to have main entry point
    if (!entry_ast) {
        return false;
    }

    char *abs_path = get_absolute_path(entry_file);
    const char* stored_path = pool_strdup(pool, abs_path);

    loaded_modules[abs_path] = entry_ast;

    load_imports(entry_ast, stored_path);

    module_parse_order.push_back(abs_path);

    ast = merge_all_modules();
    ast->file_name = stored_path;

    return true;
}


Ast_Block* Pax_Interp::load_and_parse_module(const char* filename) {
    char *abs_path = get_absolute_path(filename);

    if (loaded_modules.count(abs_path)) {
        printf("\n>>>>Module already imported before: %s\n\n", abs_path);
        return loaded_modules[abs_path];
    }

    printf("<<<Inside interp>>> Loading module: %s\n", filename);


    Ast_Block* mod_ast = parse_file(filename, true); // kind of an hack we just tell the parser to forbid a main entry point in modules
    if (!mod_ast) {
        free(abs_path);
    return nullptr;
    }

    const char* stored_path = pool_strdup(pool, abs_path);
    loaded_modules[abs_path] = mod_ast;
    module_parse_order.push_back(abs_path);

    load_imports(mod_ast, stored_path);

    return mod_ast;
}

Ast_Block* Pax_Interp::merge_all_modules() {
    Ast_Block* root = AST_NEW(pool, Ast_Block);

    for (const std::string& mod_path : module_parse_order) {
        Ast_Block* mod_ast = loaded_modules[mod_path];

        for (int i = 0; i < mod_ast->statements.count; i++) {
            root->statements.push_back(mod_ast->statements.data[i]);
        }
    }

    return root;
}

void Pax_Interp::printLexer(const char* filename) {
    FileBuffer buf = read_entire_file(filename);
    if (!buf.data) {
        printf("Failed to read file: %s\n", filename);
        return;
    }

    Lexer lexer((const char*)buf.data, buf.size, pool);
    // printLex(buf, pool);

    free(buf.data);

}

void Pax_Interp::run_frontend() {
    auto start = std::chrono::high_resolution_clock::now();

    code_manager->resolve_idents(ast);

    code_manager->resolve_unresolved_vars();
    code_manager->resolve_unresolved_calls();
    code_manager->resolve_unresolved_types();
    code_manager->resolve_unresolved_member_accesses();

    // code_manager->is_everything_resolved(); // @Comeback, breaks on demo_memory.pax
    code_manager->infer_types_block(ast);

    if (code_manager->count_errors != 0) {
        printf("\nErrors in code manager. Exiting.\n");
        exit(1);
    }


    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    // printf("\n\tFrontend finished in %.6f seconds (lexer,parser,semantic checker)\n\n", elapsed.count());
}

void Pax_Interp::generate_cpp() {
    auto start = std::chrono::high_resolution_clock::now();

    char cpp_name[256];
    snprintf(cpp_name, sizeof(cpp_name), "%s.cpp", base_name);

    c_converter->generate_cpp_code(cpp_name, ast);

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;

    // printf("\n\t -Time to output c code: %.6f seconds\n\n", elapsed.count());
}

void Pax_Interp::runCompiler(char* command)
{
   // printf("Running: %s\n", command);

   #ifdef _WIN32

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
#else
    system(command);
#endif

}

void Pax_Interp::compile_cpp() {
    auto start = std::chrono::high_resolution_clock::now();

    char command[256];
#ifdef _WIN32                               /* vvvvvvvvvvvvvv @Temporary */
    snprintf(command, sizeof(command), "cl.exe /wd4477 /wd4313 /Od /EHsc /nologo %s.cpp", base_name);
    runCompiler(command);
#else
    snprintf(command, sizeof(command), "g++ -w -o %s %s.cpp", file_name_only, base_name);
    runCompiler(command);
#endif

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    // printf("\n\t -C compilation finished in %.6f seconds\n\n", elapsed.count());
}

void Pax_Interp::release() {
    if (pool) {
        pool_release(pool);
    }
}
