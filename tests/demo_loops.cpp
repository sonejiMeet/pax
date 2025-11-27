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
s64 x = 434;

/*STRUCT FORWARD DECLARATIONS*/
struct Node;
struct AA;
struct BB;
struct EE;
struct DD;
struct CC;
struct A;
struct B;
struct C;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct Node {
int value = x;
Node * next;
};
struct AA {
BB * b;
};
struct BB {
CC * c;
};
struct EE {
s64 num = 343;
};
struct DD {
EE e;
};
struct CC {
DD d;
};
struct A {
B * b;
};
struct B {
C * c;
};
struct C {
int final = -24;
};

/*FUNCTION BODIES*/


void GENERATED_MAIN(){
    AA sdf;
    AA * a = (&sdf);
    BB _b;
    (((*a).b) = (&_b));
    CC _c;
    (((*((*a).b)).c) = (&_c));
    (((((*((*((*a).b)).c)).d).e).num) = 87);
    float local = ((((*((*((*a).b)).c)).d).e).num);
    printf("local = %f\n",local);
    A first;
    B second;
    ((first.b) = (&second));
    C third;
    (((*(first.b)).c) = (&third));
    (((*((*(first.b)).c)).final) = 24606);
    int some = ((*((*(first.b)).c)).final);
    printf("some =  %d\n",some);
    A first1;
    B second1;
    ((first1.b) = (&second1));
    C third1;
    (((*(first1.b)).c) = (&third1));
    int some1 = ((*((*(first1.b)).c)).final);
    printf("some1 =  %d\n",some1);
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
