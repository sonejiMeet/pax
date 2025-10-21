#include "interp.h"


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

int main(int argc, char** argv) {

#ifdef _WIN32

#ifdef _DEBUG
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF ); // put it at start, when we want to exit(1) early. temporary!!!!!!

    //_CrtSetBreakAlloc(182);
#endif
#endif

    if (argc < 2) {
        printf("Usage: %s <file>.pax\n", argv[0]);
        return 1;
    }

    Pax_Interp interp;
    if (!interp.init(argv[1]))
        return 1;

    auto start = std::chrono::high_resolution_clock::now();

    interp.run_frontend();
    interp.generate_cpp();
    interp.compile_cpp();

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    printf("Total time: %.6f seconds\n\n", elapsed.count());

    // printf("\nDONE. Total mallocs: %d\n", total_malloc);
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
