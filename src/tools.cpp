
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
    FileBuffer result = {nullptr, 0};

    FILE *f = fopen(path, "rb");
    defer { fclose(f); };

    if (!f) {
        printf("Could not open file: %s\n", path);
        return result;
    }

    fseek(f, 0, SEEK_END);
    size_t len = (size_t)ftell(f);
    fseek(f, 0, SEEK_SET);

    if (len == 0) return result;


    result.data = (unsigned char *)malloc(len);
    if (!result.data) return result;

    size_t read_bytes = fread(result.data, 1, len, f);

    if (read_bytes == 0) {
        printf("Failed to read file: %s\n", path);
        free(result.data);
        result.data = nullptr;
        return result;
    }

    result.size = read_bytes;

    return result;
}