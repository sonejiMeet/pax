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
void modify_2 (Data d) {
    S s;
    _init_S(&s);
    ((d.ptr) = (&s));
    S asd = (*(d.ptr));
    _init_S(&asd);
}



void GENERATED_MAIN(){
    __init_global_static_arrays();
    Data local;
    _init_Data(&local);
    S s_local;
    _init_S(&s_local);
    ((local.ptr) = (&s_local));
    modify_2(local);
    S y = (*(local.ptr));
    _init_S(&y);
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
