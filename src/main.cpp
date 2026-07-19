#include "all.h"

#ifdef _DEBUG
int totalNbyte = 0;
#endif
int total_global_malloc = 0;


extern const Def_Type *ttype = nullptr; // temporary

#ifdef AST_NEW
#undef AST_NEW
#endif

#define AST_NEW(pool, type) ([&]() -> type* {           \
    assert(pool != nullptr && "Pool must not be null"); \
    void *mem = pool_alloc_debug(pool, sizeof(type), #type, "MAIN");         \
    type *node = new (mem) type(pool);                  \
    return node;                                        \
}())

static void init_Def_Type(Def_Type *type, Pool *pool) {
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

static void *default_allocator(int mode, size_t size, size_t old_size,
                               void *old_memory, void *allocator_data, int options)
{
    switch(mode) {
        case ALLOCATE: {
            void *ptr = calloc(size, 1);
            total_global_malloc += 1;
            assert(ptr && "Memory allocation failed");
            return ptr;
        }
    }
    return 0;
}

// std::at_exit should be registered in main function so we make sure to clean up release pool even if there is unexpected exit
static Pool* global_pool = nullptr;
void cleanup_pool() {
    if (global_pool) {
        // printf("Cleaning up memory pool on exit...\n");
        pool_release(global_pool);
        global_pool = nullptr;
    }
}


int main(int argc, char **argv) {

#ifdef _WIN32

#ifdef _DEBUG
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF ); // put it at start, when we want to exit(1) early. temporary!!!!!!

    //_CrtSetBreakAlloc(182);
#endif
#endif

    // printf(" %s RUNNING %s \n", "\x1B[0;33m", "\x1B[0m");

    CLI cli;
    if(!cli_parse(&cli, argc, argv))
        return cli.help ? 0 : 1;

    tools_set_cli_time_outputter(cli.time);

    TIME_SCOPE("Total time");

    Pool pool;
    pool_init(&pool);
    pool.block_allocator = default_allocator;

   global_pool = &pool;

    // register the cleanup func
    std::atexit(cleanup_pool);

    if(cli.memory_profiler == true) {
        pool_trace_init(&pool, "pool_trace.txt");
    }

    Def_Type type;
    init_Def_Type(&type, &pool);

    Pax_Interp interp;
    interp.pool = &pool;
    interp.type = &type;

    ttype = &type; // TEMPORARY

    if(cli.print_lex) {
        interp.printLexer(argv[1]);
    } else {
        CodeManager cm(&interp);
        C_Converter cconv(&interp);

        interp.code_manager = &cm;
        interp.c_converter = &cconv;

        if (!interp.init(argv[1])) return 1;
        defer {interp.release();};

        interp.run_frontend();
        interp.generate_cpp();
        interp.compile_cpp();
    }
    // printf(" %s SUCCESS %s \n", "\x1B[0;32m", "\x1B[0m");
    printf("Total lines processed %zu\n", LINE_COUNT);

    if(cli.memory_profiler == true) {
        printf("\nOutputted trace file: pool_trace.txt\n\n");
        pool_trace_close(&pool);
    }

#ifdef _DEBUG
    printf("Total global malloc %d\n", total_global_malloc);
#ifdef _WIN32
    _CrtMemState state;
    _CrtMemCheckpoint(&state);
    _CrtMemDumpStatistics(&state);
    _CrtMemDumpAllObjectsSince(&state);
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_DEBUG);
#endif
#endif

    return 0;
}
