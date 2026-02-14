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
struct F;
struct E;
struct D;
struct C;
struct B;
struct A;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void function_1(void);

/*STRUCTS DEFINITIONS*/
struct F {
    int value;
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

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_F(F* self){
    self->value = -24;
}

inline void _init_E(E* self){
    _init_F(&self->f);
}

inline void _init_D(D* self){
    _init_E(&self->e);
}

inline void _init_C(C* self){
    _init_D(&self->d);
}

inline void _init_B(B* self){
    _init_C(&self->c);
}

inline void _init_A(A* self){
    _init_B(&self->b);
}

/*FUNCTION BODIES*/
#line 9 "C:/pax/tests/demo_simple_nested_structs.pax"
void function_1 () {
    A a;
    _init_A(&a);
#line 11 "C:/pax/tests/demo_simple_nested_structs.pax"
    (((((((a.b).c).d).e).f).value) = 42);
}


#line 5 "C:/pax/tests/demo_simple_nested_structs.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
#line 6 "C:/pax/tests/demo_simple_nested_structs.pax"
    function_1();
#line 7 "C:/pax/tests/demo_simple_nested_structs.pax"
    printf("DONE\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
