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

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void recurse(void);
void recurse_1(int a);
void newline(void);

/*STRUCTS DEFINITIONS*/

/*BSS SECTION GLOBAL VARIAABLES*/
s64 x = 0;

void __init_global_static_arrays(){
}

/*FUNCTION BODIES*/
#line 17 "C:/pax/tests/demo_recursion.pax"
void recurse () {
#line 18 "C:/pax/tests/demo_recursion.pax"
    if((x == 10)){
#line 19 "C:/pax/tests/demo_recursion.pax"
        return;
    }
#line 21 "C:/pax/tests/demo_recursion.pax"
    (x = (x + 1));
#line 23 "C:/pax/tests/demo_recursion.pax"
    printf("x = %d\n",x);
#line 24 "C:/pax/tests/demo_recursion.pax"
    recurse();
}

#line 28 "C:/pax/tests/demo_recursion.pax"
void recurse_1 (int a) {
#line 29 "C:/pax/tests/demo_recursion.pax"
    if((a == 10)){
#line 30 "C:/pax/tests/demo_recursion.pax"
        return;
    }
#line 32 "C:/pax/tests/demo_recursion.pax"
    (a = (a + 1));
#line 34 "C:/pax/tests/demo_recursion.pax"
    recurse_1(a);
#line 36 "C:/pax/tests/demo_recursion.pax"
    printf("a = %d\n",a);
}

#line 38 "C:/pax/tests/demo_recursion.pax"
void newline () {
#line 38 "C:/pax/tests/demo_recursion.pax"
    printf("\n");
}


#line 6 "C:/pax/tests/demo_recursion.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
#line 7 "C:/pax/tests/demo_recursion.pax"
    printf("print on way down recursion\n");
#line 9 "C:/pax/tests/demo_recursion.pax"
    recurse();
#line 11 "C:/pax/tests/demo_recursion.pax"
    newline();
#line 12 "C:/pax/tests/demo_recursion.pax"
    printf("print on way up recursion\n");
#line 13 "C:/pax/tests/demo_recursion.pax"
    recurse_1(0);
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
