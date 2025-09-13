#include "token.h"

void freeToken(Token& t) {

    /*if (t.type == TOK_STRING) {
        if (t.string_value.data) {
            free(t.string_value.data);
            t.string_value.data = nullptr;
            t.string_value.count = 0;
            t.owns_value = false;
        }
    }
    else*/ if (t.owns_value && t.value) {
        free((void*)t.value);
        t.value = nullptr;
        t.owns_value = false;
    }
}
