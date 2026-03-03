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
struct Data_1;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void function_4(void);

/*STRUCTS DEFINITIONS*/
struct Data_1 {
    int value;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Data_1(Data_1* self){
    self->value = -24;
}

/*FUNCTION BODIES*/
void function_4 () {
    Data_1 d;
    _init_Data_1(&d);
    Data_1 * p1;
    Data_1 * p2;
    (p1 = (&d));
    (p2 = p1);
    (((*p2).value) = 100);
    printf("p2.value = %d\n",((*p2).value));
    printf("DONE\n");
}



void GENERATED_MAIN(){
    __init_global_static_arrays();
    function_4();
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
