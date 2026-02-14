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
struct Inner;
struct Outer;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct Inner {
    int * data;
};
struct Outer {
    Inner a;
    Inner b;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Inner(Inner* self){
}

inline void _init_Outer(Outer* self){
    _init_Inner(&self->a);
    _init_Inner(&self->b);
}

/*FUNCTION BODIES*/

#line 5 "C:/pax/tests/demo_nested_init.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    Outer o1;
    _init_Outer(&o1);
    int val = 42;
    int val2 = 69;
#line 10 "C:/pax/tests/demo_nested_init.pax"
    (((o1.a).data) = (&val));
#line 11 "C:/pax/tests/demo_nested_init.pax"
    (((o1.b).data) = (&val2));
    int x = (*((o1.a).data));
    int y = (*((o1.b).data));
#line 17 "C:/pax/tests/demo_nested_init.pax"
    printf("DONE\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
