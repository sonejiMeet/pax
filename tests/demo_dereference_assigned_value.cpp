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
struct Data_4;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct Data_4 {
    int * ptr;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Data_4(Data_4* self){
}

/*FUNCTION BODIES*/

#line 9 "C:/pax/tests/demo_dereference_assigned_value.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    Data_4 d;
    _init_Data_4(&d);
    int i = 2544;
#line 13 "C:/pax/tests/demo_dereference_assigned_value.pax"
    ((d.ptr) = (&i));
#line 17 "C:/pax/tests/demo_dereference_assigned_value.pax"
    ((*(d.ptr)) = 224);
#line 19 "C:/pax/tests/demo_dereference_assigned_value.pax"
    printf("*d.ptr = %d\n",(*(d.ptr)));
#line 19 "C:/pax/tests/demo_dereference_assigned_value.pax"
    if(((*(d.ptr)) == 224)){
#line 20 "C:/pax/tests/demo_dereference_assigned_value.pax"
        printf("This is correct\n");
    }
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
