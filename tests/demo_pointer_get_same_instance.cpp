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
struct AAA;
struct BBB;
struct CCC;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct AAA {
    BBB * b;
};
struct BBB {
    CCC * c;
};
struct CCC {
    s64 value;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_AAA(AAA* self){
}

inline void _init_BBB(BBB* self){
}

inline void _init_CCC(CCC* self){
}

/*FUNCTION BODIES*/

#line 5 "C:/pax/tests/demo_pointer_get_same_instance.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    AAA a;
    _init_AAA(&a);
    BBB b;
    _init_BBB(&b);
#line 7 "C:/pax/tests/demo_pointer_get_same_instance.pax"
    ((a.b) = (&b));
    CCC c;
    _init_CCC(&c);
#line 11 "C:/pax/tests/demo_pointer_get_same_instance.pax"
    (((*(a.b)).c) = (&c));
#line 12 "C:/pax/tests/demo_pointer_get_same_instance.pax"
    ((b.c) = (&c));
#line 13 "C:/pax/tests/demo_pointer_get_same_instance.pax"
    (((*((*(a.b)).c)).value) = 42699);
#line 16 "C:/pax/tests/demo_pointer_get_same_instance.pax"
    printf("a.b.c.value = %d\n",((*((*(a.b)).c)).value));
#line 16 "C:/pax/tests/demo_pointer_get_same_instance.pax"
    if((((*(a.b)).c) != (b.c))){
#line 18 "C:/pax/tests/demo_pointer_get_same_instance.pax"
        printf("this is bad\n");
    }
    else {
#line 21 "C:/pax/tests/demo_pointer_get_same_instance.pax"
        printf("this is good\n");
    }
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
