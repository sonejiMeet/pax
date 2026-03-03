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
struct AA;
struct BB;
struct EE;
struct DD;
struct CC;
struct A;
struct B;
struct C;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct AA {
    BB * b;
};
struct BB {
    CC * c;
};
struct EE {
    s64 num;
};
struct DD {
    EE e;
};
struct CC {
    DD d;
};
struct A {
    B * b;
};
struct B {
    C * c;
};
struct C {
    int final;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_AA(AA* self){
}

inline void _init_BB(BB* self){
}

inline void _init_EE(EE* self){
    self->num = 343;
}

inline void _init_DD(DD* self){
    _init_EE(&self->e);
}

inline void _init_CC(CC* self){
    _init_DD(&self->d);
}

inline void _init_A(A* self){
}

inline void _init_B(B* self){
}

inline void _init_C(C* self){
    self->final = -24;
}

/*FUNCTION BODIES*/


void GENERATED_MAIN(){
    __init_global_static_arrays();
    AA sdf;
    _init_AA(&sdf);
    AA * a = (&sdf);
    BB _b;
    _init_BB(&_b);
    (((*a).b) = (&_b));
    CC _c;
    _init_CC(&_c);
    (((*((*a).b)).c) = (&_c));
    (((((*((*((*a).b)).c)).d).e).num) = 87);
    float local = ((((*((*((*a).b)).c)).d).e).num);
    printf("local = %f\n",local);
    A first;
    _init_A(&first);
    B second;
    _init_B(&second);
    ((first.b) = (&second));
    C third;
    _init_C(&third);
    (((*(first.b)).c) = (&third));
    (((*((*(first.b)).c)).final) = 24606);
    int some = ((*((*(first.b)).c)).final);
    printf("some =  %d\n",some);
    A first1;
    _init_A(&first1);
    B second1;
    _init_B(&second1);
    ((first1.b) = (&second1));
    C third1;
    _init_C(&third1);
    (((*(first1.b)).c) = (&third1));
    int some1 = ((*((*(first1.b)).c)).final);
    printf("some1 =  %d\n",some1);
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
