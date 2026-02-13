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
struct CC;
struct BB;
struct EE;
struct DD;
struct FF;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void function_3(void);

/*STRUCTS DEFINITIONS*/
struct AA {
    BB * b;
};
struct CC {
    DD * d;
};
struct BB {
    CC c;
};
struct EE {
    FF * f;
};
struct DD {
    EE e;
};
struct FF {
    int value;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_AA(AA* self){
}

inline void _init_CC(CC* self){
}

inline void _init_BB(BB* self){
    _init_CC(&self->c);
}

inline void _init_EE(EE* self){
}

inline void _init_DD(DD* self){
    _init_EE(&self->e);
}

inline void _init_FF(FF* self){
    self->value = -24;
}

/*FUNCTION BODIES*/
#line 7 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
void function_3 () {
    AA a;
    _init_AA(&a);
    BB b;
    _init_BB(&b);
    DD d;
    _init_DD(&d);
    FF f;
    _init_FF(&f);
#line 13 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
    ((a.b) = (&b));
#line 14 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
    (((b.c).d) = (&d));
#line 15 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
    (((d.e).f) = (&f));
#line 17 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
    (((*(((*(((*(a.b)).c).d)).e).f)).value) = 42);
#line 19 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
    printf("a.b.c.d.e.f.value = %d\n",((*(((*(((*(a.b)).c).d)).e).f)).value));
#line 20 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
    printf("Sucess\n");
}


#line 5 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
#line 6 "C:/pax/tests/demo_pass_nested_structs_with_members_as_pointers.pax"
    function_3();
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
