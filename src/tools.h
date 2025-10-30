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