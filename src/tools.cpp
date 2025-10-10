#include "tools.h"
#include <cstdio>
#include <cstdlib>

int isNumeric(char c) {
    return c >= '0' && c <= '9';
}

int isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int isAlphaNumeric(char c){
    return (isNumeric(c) || isAlpha(c));
}


FileBuffer read_entire_file(const char *path) {
    FileBuffer result;

    FILE *f = NULL;
#ifdef _WIN32
    fopen_s(&f, path, "rb");
#elif __linux
    f = fopen64(path, "rb");
#endif
    if (!f) {
        fprintf(stderr, "Could not open file: %s\n", path);
        return result;
    }

    // Get file size
#ifdef _WIN32
    if (_fseeki64(f, 0, SEEK_END) != 0) {
        fprintf(stderr, "Could not seek end of file: %s\n", path);
        fclose(f);
        return result;
    }

    long long file_size = _ftelli64(f);
#else
    if (fseek(f, 0, SEEK_END) != 0) {
        fprintf(stderr, "Could not seek end of file: %s\n", path);
        fclose(f);
        return result;
    }

    long file_size = ftell(f);
#endif

    if (file_size < 0) {
        fprintf(stderr, "Could not get file size: %s\n", path);
        fclose(f);
        return result;
    }

    result.size = static_cast<size_t>(file_size);

    if (fseek(f, 0, SEEK_SET) != 0) {
        fprintf(stderr, "Could not rewind file: %s\n", path);
        fclose(f);
        return result;
    }

    result.data = (uint8_t*)malloc(result.size);
    if (!result.data) {
        fprintf(stderr, "Memory allocation failed for file: %s\n", path);
        fclose(f);
        return result;
    }

    size_t read_bytes = fread(result.data, 1, result.size, f);
    if (read_bytes != result.size) {
        fprintf(stderr, "Failed to read file: %s\n", path);
        free(result.data);
        result.data = nullptr;
        result.size = 0;
        fclose(f);
        return result;
    }

    fclose(f);
    return result;
}
