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

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct Node {
    s64 a;
};

/*BSS SECTION GLOBAL VARIAABLES*/
s64 x = (-351);

void __init_global_static_arrays(){
}

inline void _init_Node(Node* self){
    self->a = x;
}

/*FUNCTION BODIES*/


void GENERATED_MAIN(){
    __init_global_static_arrays();
    Node n;
    _init_Node(&n);
    printf("n.a = %d\n",(n.a));
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
