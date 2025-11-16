/* GENERATED FILE */

#include <stdlib.h>
#include <stdio.h>
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

/*BSS SECTION GLOBAL VARIAABLES*/

/*STRUCT FORWARD DECLARATIONS*/
struct F;
struct E;
struct D;
struct C;
struct B;
struct A;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct F {
int value = -24;
};
struct E {
F f;
};
struct D {
E e;
};
struct C {
D d;
};
struct B {
C c;
};
struct A {
B b;
};

/*FUNCTION BODIES*/


void GENERATED_MAIN(){
    A a;
    (((((((a.b).c).d).e).f).value) = 42);
    printf("a.b.c.d.e.f.value = %d\n",((((((a.b).c).d).e).f).value));
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
