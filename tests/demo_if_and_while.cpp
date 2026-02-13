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
struct Static_Array;
struct Dynamic_Array;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
Dynamic_Array __NewArray_impl(s64 count, s64 element_size);

/*STRUCTS DEFINITIONS*/
struct Static_Array {
    s64 count;
    void * data;
};
struct Dynamic_Array {
    s64 count;
    s64 allocated;
    void * data;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Static_Array(Static_Array* self){
}

inline void _init_Dynamic_Array(Dynamic_Array* self){
}

/*FUNCTION BODIES*/
#line 27 "C:/pax/tests/General.pax"
Dynamic_Array __NewArray_impl (s64 count, s64 element_size) {
    Dynamic_Array arr;
    _init_Dynamic_Array(&arr);
#line 30 "C:/pax/tests/General.pax"
    ((arr.data) = (void *)malloc((count  *element_size)));
#line 31 "C:/pax/tests/General.pax"
    ((arr.count) = count);
#line 32 "C:/pax/tests/General.pax"
    ((arr.allocated) = count);
#line 34 "C:/pax/tests/General.pax"
    return arr;
}


#line 5 "C:/pax/tests/demo_if_and_while.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    s64 i = 0;
#line 6 "C:/pax/tests/demo_if_and_while.pax"
    while(((i < 10) && ((i != 5) || false))){
#line 7 "C:/pax/tests/demo_if_and_while.pax"
        (i = (i + 1));
#line 9 "C:/pax/tests/demo_if_and_while.pax"
        printf("i = %d\n",i);
    }
    bool a = ((i <= 5) && (4 != 5));
#line 13 "C:/pax/tests/demo_if_and_while.pax"
    if((!a)){
#line 16 "C:/pax/tests/demo_if_and_while.pax"
        printf("its false\n");
    }
#line 16 "C:/pax/tests/demo_if_and_while.pax"
    if(a){
#line 20 "C:/pax/tests/demo_if_and_while.pax"
        printf("its true\n");
    }
    int SIZE = 5;
    int __data__arr[5];
    Static_Array arr;
    arr.data = (void *)__data__arr;
    arr.count = 5;
#line 22 "C:/pax/tests/demo_if_and_while.pax"
    (i = 0);
#line 23 "C:/pax/tests/demo_if_and_while.pax"
    while((i != SIZE)){
#line 24 "C:/pax/tests/demo_if_and_while.pax"
        ((((int*)arr.data)[i]) = i);
#line 25 "C:/pax/tests/demo_if_and_while.pax"
        (i = (i + 1));
    }
#line 28 "C:/pax/tests/demo_if_and_while.pax"
    (i = 0);
#line 29 "C:/pax/tests/demo_if_and_while.pax"
    while((i < (arr.count))){
#line 31 "C:/pax/tests/demo_if_and_while.pax"
        printf("arr[i] = %d\n",(((int*)arr.data)[i]));
#line 31 "C:/pax/tests/demo_if_and_while.pax"
        (i = (i + 1));
    }
#line 34 "C:/pax/tests/demo_if_and_while.pax"
    if(((!a) && (a == true))){
#line 35 "C:/pax/tests/demo_if_and_while.pax"
        printf("its true\n");
    }
    else if((!a)){
#line 37 "C:/pax/tests/demo_if_and_while.pax"
        printf("its false\n");
    }
    else {
#line 41 "C:/pax/tests/demo_if_and_while.pax"
        printf("its nothing\n");
    }
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
