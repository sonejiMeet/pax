#include "token.h"

void freeToken(Token& t) {
    switch (t.type) {
        case TOK_IDENTIFIER:
        case TOK_PRINT:
        case TOK_IF:
        case TOK_ELSE:
        case TOK_STRUCT:
        case TOK_TYPE_INT:
        case TOK_TYPE_FLOAT:
        case TOK_TYPE_STRING:
        case TOK_TYPE_BOOL:
        case TOK_UNKNOWN:
            if (t.value) {
                free((void*)t.value);
                t.value = nullptr;
            }
            break;

        case TOK_STRING:
            if (t.string_value.data) {
                free(t.string_value.data);
                t.string_value.data = nullptr;
                t.string_value.count = 0;
            }
            break;

        default:
            // numbers and operators have nothing to free
            break;
    }
}

