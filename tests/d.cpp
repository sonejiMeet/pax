/* GENERATED FILE */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
typedef unsigned long long u64;
typedef unsigned int       u32;
typedef unsigned short     u16;
typedef unsigned char      u8;
typedef long long  s64;
typedef int        s32;
typedef short      s16;
typedef char       s8;
typedef float      float32;
typedef double     float64;

/*STRUCT FORWARD DECLARATIONS*/
struct Node;
struct Node;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
Node * make_list(s64 val);

/*STRUCTS DEFINITIONS*/
struct Node {
    int value;
    Node * next;
};
struct Node {
    int data;
    Node * next;
    Node * prev;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Node(Node* self){
    self->value = -24;
    self->next = nullptr;
}

inline void _init_Node(Node* self){
    self->data = -24;
}

/*FUNCTION BODIES*/
Node * make_list (s64 val) {
    Node * base = (Node *)malloc(sizeof(Node));
    (((*base).value) = val);
    (((*base).next) = nullptr);
    return base;
}



void GENERATED_MAIN(){
    __init_global_static_arrays();
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
