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
struct Data_2;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct Data_2 {
    Data_2 * nested;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Data_2(Data_2* self){
}

/*FUNCTION BODIES*/


void GENERATED_MAIN(){
    __init_global_static_arrays();
    Data_2 d1;
    _init_Data_2(&d1);
    Data_2 d2;
    _init_Data_2(&d2);
    ((d1.nested) = (&d2));
    Data_2 copy = (*(d1.nested));
    _init_Data_2(&copy);
    printf("d1.nested   = %p\n",(&(d1.nested)));
    printf("copy.nested = %p\n",(&(copy.nested)));
    if(((&(d1.nested)) == (&(copy.nested)))){
        printf("THIS IS BAD\n");
    }
    else {
        printf("THIS IS GOOD\n");
    }
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
