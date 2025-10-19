#include "interp.h"
#include "ast_printer.h"

#define AST_NEW(pool, type) ([&]() -> type* { \
    assert(pool != nullptr && "Pool must not be null"); \
    void* mem = pool_alloc(pool, sizeof(type)); \
    type* node = new (mem) type(pool); \
    return node; \
}())

static void init_Def_Type(Def_Type* type, Pool* pool) {
    type->type_def_dummy   = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_int     = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_s8      = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_s16     = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_s32     = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_s64     = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_u8      = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_u16     = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_u32     = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_u64     = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_float   = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_float32 = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_float64 = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_void    = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_bool    = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_string  = AST_NEW(pool, Ast_Type_Definition);
    type->literal_true     = AST_NEW(pool, Ast_Literal);
    type->literal_false    = AST_NEW(pool, Ast_Literal);
}

extern const Def_Type* ttype = nullptr;
int total_malloc = 0;

static void* default_allocator(int mode, size_t size, size_t old_size,
                               void* old_memory, void* allocator_data, int options)
{
    switch(mode) {
        case ALLOCATE: {
            void* ptr = calloc(size, 1);
            total_malloc += 1;
            assert(ptr && "Memory allocation failed");
            return ptr;
        }
    }
    return 0;
}

Pax_Interp::Pax_Interp()
    : lexer(nullptr), parser(nullptr), ast(nullptr)
{
    pool_init(&pool);
    pool.block_allocator = default_allocator;
    init_Def_Type(&type, &pool);
    ttype = &type;
}

Pax_Interp::~Pax_Interp() {
    release();
}

bool Pax_Interp::init(const char* filename) {
    strncpy(input_path, filename, sizeof(input_path));

    buf = read_entire_file(filename);
    if (!buf.data) {
        printf("Failed to read file: %s\n", filename);
        return false;
    }

    lexer = new Lexer((const char*)buf.data, buf.size, &pool);
    parser = new Parser(lexer, &pool, &type);

    // make base name
    strcpy(base_name, input_path);
    char* dot = strrchr(base_name, '.');
    if (dot) *dot = 0;

#ifdef _WIN32
    char* slash = strrchr(base_name, '\\');
#else
    char* slash = strrchr(base_name, '/');
#endif
    const char* name_only = (slash) ? slash + 1 : base_name;
    strncpy(file_name_only, name_only, sizeof(file_name_only));

    return true;
}

void Pax_Interp::run_frontend() {
    auto start = std::chrono::high_resolution_clock::now();

    ast = parser->parseProgram();
    // printAst(ast);

    CodeManager cm(&pool, &type);
    cm.resolve_idents(ast);
    cm.resolve_unresolved_calls();
    cm.infer_types_block(ast);

    if (cm.count_errors != 0) {
        printf("\nErrors in code manager. Exiting.\n");
        exit(1);
    }

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    printf("\n\tFrontend finished in %.6f seconds (lexer,parser,semantic checker)\n\n", elapsed.count());
}

void Pax_Interp::generate_cpp() {
    auto start = std::chrono::high_resolution_clock::now();

    char cpp_name[256];
    snprintf(cpp_name, sizeof(cpp_name), "%s.cpp", base_name);

    C_Converter cconv(&type);
    cconv.generate_cpp_code(cpp_name, ast);
    printf("Generated: %s\n", cpp_name);

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    printf("\n\t -Time to output c code: %.6f seconds\n\n", elapsed.count());
}

void Pax_Interp::runCompiler(char * command)
{
    printf("Running: %s\n", command);

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
#ifdef _WIN32
    snprintf(command, sizeof(command), "cl.exe /w /Od /EHsc /nologo %s.cpp", base_name);
    runCompiler(command);
#else
    snprintf(command, sizeof(command), "g++ -w -o %s %s.cpp", file_name_only, base_name);
    runCompiler(command);
#endif

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    printf("\n\t -C compilation finished in %.6f seconds\n\n", elapsed.count());
}

void Pax_Interp::release() {
    if (buf.data) {
        free(buf.data);
        buf.data = nullptr;
    }
    if (parser) {
        delete parser;
        parser = nullptr;
    }
    if (lexer) {
        delete lexer;
        lexer = nullptr;
    }
    pool_release(&pool);
}
