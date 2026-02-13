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
struct Data;
struct S;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void modify_2(Data d);

/*STRUCTS DEFINITIONS*/
struct Data {
    S * ptr;
};
struct S {
    int a;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Data(Data* self){
}

inline void _init_S(S* self){
    self->a = -24;
}

/*FUNCTION BODIES*/
#line 23 "C:/pax/tests/demo_pass_initialized_ptr_member_passed_by_value.pax"
void modify_2 (Data d) {
    S s;
    _init_S(&s);
#line 25 "C:/pax/tests/demo_pass_initialized_ptr_member_passed_by_value.pax"
    ((d.ptr) = (&s));
    S asd = (*(d.ptr));
    _init_S(&asd);
}


#line 5 "C:/pax/tests/demo_pass_initialized_ptr_member_passed_by_value.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    Data local;
    _init_Data(&local);
    S s_local;
    _init_S(&s_local);
#line 8 "C:/pax/tests/demo_pass_initialized_ptr_member_passed_by_value.pax"
    ((local.ptr) = (&s_local));
#line 11 "C:/pax/tests/demo_pass_initialized_ptr_member_passed_by_value.pax"
    modify_2(local);
    S y = (*(local.ptr));
    _init_S(&y);
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
