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
s64 x = 0;

/*STRUCT FORWARD DECLARATIONS*/

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void recurse(void);
void recurse_1(int a);
void newline(void);

/*STRUCTS DEFINITIONS*/

/*FUNCTION BODIES*/

void recurse () {
    if((x == 10)){
        return;
    }
    (x = (x + 1));
    printf("x = %d\n",x);
    recurse();
}


void recurse_1 (int a) {
    if((a == 10)){
        return;
    }
    (a = (a + 1));
    recurse_1(a);
    printf("a = %d\n",a);
}


void newline () {
    printf("\n");
}



void GENERATED_MAIN(){
    printf("print on way down recursion\n");
    recurse();
    newline();
    printf("print on way up recursion\n");
    recurse_1(0);
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
