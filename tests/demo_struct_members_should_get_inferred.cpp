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
struct Config;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct Config {
    u8 * name;
    s64 count;
    int * data;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Config(Config* self){
    self->name = (u8 *)("default");
    self->count = 10;
}

/*FUNCTION BODIES*/


void GENERATED_MAIN(){
    __init_global_static_arrays();
    Config c;
    _init_Config(&c);
    int val = 42;
    ((c.data) = (&val));
    int x = (*(c.data));
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
