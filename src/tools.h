#pragma once

#include <cstdint>   // uint8_t, uint64_t, etc.
#include <cstddef>

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
static char * pool_strdup(Pool *pool, const char *str) {
    size_t len = strlen(str)+1;
    char *p = (char *)pool_alloc(pool, len);
    memcpy(p, str, len);
    // p[len] = '\0';
    //printf("pool_strdup %d\"%.*s\"\n", len, len, p);
    return p;
}

inline
static char* c_concat3(const char* a, const char* b, const char* c) {
    size_t la = strlen(a);
    size_t lb = strlen(b);
    size_t lc = strlen(c);
    char* out = (char*)malloc(la + lb + lc + 1);
    memcpy(out, a, la);
    memcpy(out + la, b, lb);
    memcpy(out + la + lb, c, lc + 1);
    return out;
}
