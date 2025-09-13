#pragma once

#include <cstdint>   // uint8_t, uint64_t, etc.

int isNumeric(char c);
int isAlpha(char c);
int isAlphaNumeric(char c);

struct FileBuffer {
    uint8_t *data = nullptr;
    size_t size = 0;
};

FileBuffer read_entire_file(const char *path);