#include "tools.h"

int isNumeric(char c) {
    return c >= '0' && c <= '9';
}

int isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int isAlphaNumeric(char c){
    return (isNumeric(c) || isAlpha(c));
}