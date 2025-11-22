/* GENERATED FILE */

#include <stdlib.h>
#include <stdio.h>
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
struct Point;
struct Container;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
int add(int a, int b);
int mul(int a, int b);
void print_(Point * p);
Container create_data(void);
void something(void);

/*STRUCTS DEFINITIONS*/
struct Point {
int x = -24;
int y = -24;
};
struct Container {
int value = -24;
Point * data;
};

/*FUNCTION BODIES*/

int add (int a, int b) {
    return (a + b);
}


int mul (int a, int b) {
    int result = (a * b);
    return result;
}


void print_ (Point * p) {
    printf("Point: x=%d, y=%d\n",((*p).x),((*p).y));
}


Container create_data () {
    Container c;
    ((c.value) = 999);
    return c;
}


void something () {
    printf("We are inside fake2.pax\n");
}



void GENERATED_MAIN(){
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
