#include "interp.h"
#include "tools.h"

#ifdef _WIN32
#include <windows.h>

    #ifdef _DEBUG
        #define _CRTDBG_MAP_ALLOC // for mem leaks
        #include <crtdbg.h>

        #define malloc(s) _malloc_dbg(s, _NORMAL_BLOCK, __FILE__, __LINE__)
        #define free(p) _free_dbg(p, _NORMAL_BLOCK)
    #endif

#endif

#ifdef _DEBUG
int totalNbyte = 0;
#endif


// #define PRINT_LEX

extern const Def_Type *ttype = nullptr;

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
    type->type_def_null    = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_bool    = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_string  = AST_NEW(pool, Ast_Type_Definition);
    type->type_def_any     = AST_NEW(pool, Ast_Type_Definition);
    type->literal_true     = AST_NEW(pool, Ast_Literal);
    type->literal_false    = AST_NEW(pool, Ast_Literal);
}

static void* default_allocator(int mode, size_t size, size_t old_size,
                               void* old_memory, void* allocator_data, int options)
{
    switch(mode) {
        case ALLOCATE: {
            void* ptr = calloc(size, 1);
            assert(ptr && "Memory allocation failed");
            return ptr;
        }
    }
    return 0;
}


int main(int argc, char** argv) {

#ifdef _WIN32

#ifdef _DEBUG
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF ); // put it at start, when we want to exit(1) early. temporary!!!!!!

    //_CrtSetBreakAlloc(182);
#endif
#endif

    printf(" %s RUNNING %s \n", "\x1B[0;33m", "\x1B[0m");

    if (argc < 2) {
        printf("Usage: %s <file>.pax\n", argv[0]);
        return 1;
    }

    auto start = std::chrono::high_resolution_clock::now();

    Pool pool;
    pool_init(&pool);
    pool.block_allocator = default_allocator;

    Def_Type type;
    init_Def_Type(&type, &pool);

    Pax_Interp interp;
    interp.pool = &pool;
    interp.type = &type;

    ttype = &type; // TEMPORARY

#ifdef PRINT_LEX
    interp.printLexer(argv[1]);

#else
    CodeManager cm(&interp);
    C_Converter cconv(&interp);

    interp.code_manager = &cm;
    interp.c_converter = &cconv;

    if (!interp.init(argv[1])) return 1;
    defer {interp.release();};

    interp.run_frontend();
    interp.generate_cpp();
    interp.compile_cpp();

#endif

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    // printf("Total time: %.6f seconds\n\n", elapsed.count());

    printf(" %s SUCCESS %s \n", "\x1B[0;32m", "\x1B[0m");

    #ifdef _WIN32
    #ifdef _DEBUG
        _CrtMemState state;
        _CrtMemCheckpoint(&state);
        _CrtMemDumpStatistics(&state);
        _CrtMemDumpAllObjectsSince(&state);
        _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_DEBUG);
    #endif
    #endif

    return 0;
}
