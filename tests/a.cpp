/* GENERATED FILE */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
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

/*BSS SECTION GLOBAL VARIAABLES*/

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

/*FUNCTION BODIES*/
Dynamic_Array __NewArray_impl (s64 count, s64 element_size) {
    Dynamic_Array arr;
    ((arr.data) = (void *)malloc((count * element_size)));
    ((arr.count) = count);
    ((arr.allocated) = count);
    return arr;
}



void GENERATED_MAIN(){
    s64 i = 0;
    while((i < 10) && ((i != 5) || false)){
        (i = (i + 1));
        printf("i = %d\n",i);
    }
    bool a = ((i <= 5) && (4 != 5));
    if((!a)){
        printf("its false\n");
    }
    if(a){
        printf("its true\n");
    }
    int SIZE = 5;
    int __data__arr[5];
    Static_Array arr;
    arr.data = (void *)__data__arr;
    arr.count = 5;
    (i = 0);
    while(i != SIZE){
        ((((int*)arr.data)[i]) = i);
        (i = (i + 1));
    }
    (i = 0);
    while(i != SIZE){
        printf("arr[i] = %d\n",(((int*)arr.data)[i]));
        (i = (i + 1));
    }
    if(((!a) && (a == true))){
        printf("its true\n");
    }
    else     if((!a)){
        printf("its false\n");
    }
    else {
        printf("its nothing\n");
    }
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
