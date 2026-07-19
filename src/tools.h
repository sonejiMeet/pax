#pragma once

//
// Visual studio Memory checker
//
#ifdef _WIN32
#ifdef _DEBUG
    // #define _CRTDBG_MAP_ALLOC // for mem leaks
    // #include <crtdbg.h>

    // #define malloc(s) _malloc_dbg(s, _NORMAL_BLOCK, __FILE__, __LINE__)
    // #define free(p) _free_dbg(p, _NORMAL_BLOCK)
#endif
#endif


#define FOR(type)                                             \
    for(int it_index=0; it_index < (type).count; ++it_index)  \
        for(auto *it = (type).data[it_index]; it; it=nullptr)


int isNumeric(char c);
int isAlpha(char c);
int isAlphaNumeric(char c);

struct FileBuffer {
    uint8_t *data = nullptr;
    size_t size = 0;
};

FileBuffer read_entire_file(const char *path);


#ifndef HAVE_DEFER
#define HAVE_DEFER

#define CONCAT_INTERNAL(x, y) x ## y
#define CONCAT(x,y) CONCAT_INTERNAL(x,y)

template<typename T>
struct ExitScope
{
    T lambda;
    ExitScope(T lambda):lambda(lambda){}
    ~ExitScope(){lambda();}
    ExitScope(const ExitScope&);

private:
    ExitScope& operator =(const ExitScope&);
};


class ExitScopeHelp
{
public:
    template<typename T>
    ExitScope<T> operator+(T t){return t;}
};

#define defer const auto& CONCAT(defer__, __LINE__) = ExitScopeHelp() + [&]()
#endif // HAVE_DEFER


inline
char *pool_strdup(Pool *pool, const char *str) {
    size_t len = strlen(str)+1;
    // char *p = (char *)pool_alloc(pool, len);
    char *p = (char *)pool_alloc_debug(pool, len, "char *", "TOOLS");
    memcpy(p, str, len);
    // p[len] = '\0';
    //printf("pool_strdup %d\"%.*s\"\n", len, len, p);
    return p;
}

inline
char *c_concat3(const char *a, const char *b, const char *c) {
    size_t la = strlen(a);
    size_t lb = strlen(b);
    size_t lc = strlen(c);
    char *out = (char*)malloc(la + lb + lc + 1);
    memcpy(out, a, la);
    memcpy(out + la, b, lb);
    memcpy(out + la + lb, c, lc + 1);
    return out;
}

#ifndef TINY_TIMER
#define TINY_TIMER

bool time_enabled = false;

inline void tools_set_cli_time_outputter(bool b) {
    time_enabled = b;
}

#ifdef _WIN32
extern "C" {
    __declspec(dllimport) int __stdcall QueryPerformanceCounter(long long* lpPerformanceCount);
    __declspec(dllimport) int __stdcall QueryPerformanceFrequency(long long* lpFrequency);
}

struct ScopedTimer {
    const char* name;
    long long start;
    bool enabled;

    ScopedTimer(const char* n) : enabled(time_enabled), name(n) {
        if(enabled) QueryPerformanceCounter(&start);
    }

    ~ScopedTimer() {
        if(!enabled) return;
        long long end, freq;
        QueryPerformanceCounter(&end);
        QueryPerformanceFrequency(&freq);

        // (Difference in ticks) / (Ticks per second)
        printf("%s: %fs\n\n", name, (double)(end - start) / freq);
    }
};

#else

#include <time.h>

class ScopedTimer {
private:
    struct timespec start;
    const char* name;
    bool enabled;
public:
    ScopedTimer(const char* timer_name) : enabled(time_enabled), name(timer_name) {
        if(enabled) clock_gettime(CLOCK_MONOTONIC, &start);
    }

    ~ScopedTimer() {
        if(!enabled) return;
        struct timespec end;
        clock_gettime(CLOCK_MONOTONIC, &end);
        double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
        printf("%s: %fs\n\n", name, elapsed);
    }
};

#endif



#define TIME_SCOPE(name) ScopedTimer timer_##__LINE__(name)

#endif // TINY_TIMER
