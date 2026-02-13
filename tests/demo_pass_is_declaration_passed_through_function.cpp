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
struct Data_3;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void modify(Data_3 * d);

/*STRUCTS DEFINITIONS*/
struct Data_3 {
    int * ptr;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Data_3(Data_3* self){
}

/*FUNCTION BODIES*/
#line 17 "C:/pax/tests/demo_pass_is_declaration_passed_through_function.pax"
void modify (Data_3 * d) {
    int val = 10;
#line 19 "C:/pax/tests/demo_pass_is_declaration_passed_through_function.pax"
    (((*d).ptr) = (&val));
}


#line 5 "C:/pax/tests/demo_pass_is_declaration_passed_through_function.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    Data_3 local;
    _init_Data_3(&local);
#line 8 "C:/pax/tests/demo_pass_is_declaration_passed_through_function.pax"
    modify((&local));
    int y = (*(local.ptr));
#line 11 "C:/pax/tests/demo_pass_is_declaration_passed_through_function.pax"
    printf("DONE\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
