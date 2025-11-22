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
s64 frame = 0;

/*STRUCT FORWARD DECLARATIONS*/
struct F;
struct E;
struct D;
struct C;
struct B;
struct A;
struct Inner;
struct Outer;
struct Data_1;
struct Data_2;
struct AAA;
struct BBB;
struct CCC;
struct Config;
struct Data_3;
struct Data_4;
struct Vec2;
struct PhysicsObject;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void function_1(void);
void function_3(void);
void function_4(void);
void function_5(void);
void function_6(void);
void function_7(void);
void function_8(void);
void modify(Data_3 * d);
void function_9(void);
void simulate(PhysicsObject * obj);
void function_10(void);
void update(PhysicsObject * obj);

/*STRUCTS DEFINITIONS*/
struct F {
int value = -24;
};
struct E {
F f;
};
struct D {
E e;
};
struct C {
D d;
};
struct B {
C c;
};
struct A {
B b;
};
struct Inner {
int * data;
};
struct Outer {
Inner a;
Inner b;
};
struct Data_1 {
int value = -24;
};
struct Data_2 {
Data_2 * nested;
};
struct AAA {
BBB * b;
};
struct BBB {
CCC * c;
};
struct CCC {
s64 value;
};
struct Config {
char * name = "default";
s64 count = 10;
int * data;
};
struct Data_3 {
int * ptr;
};
struct Data_4 {
int * ptr;
};
struct Vec2 {
int x = -24;
int y = -24;
};
struct PhysicsObject {
Vec2 position;
Vec2 velocity;
};

/*FUNCTION BODIES*/

void function_1 () {
    A a;
    (((((((a.b).c).d).e).f).value) = 42);
    printf("a.b.c.d.e.f.value = %d\n",((((((a.b).c).d).e).f).value));
}


void function_3 () {
    Outer o1;
    int val = 42;
    int val2 = 69;
    (((o1.a).data) = (&val));
    (((o1.b).data) = (&val2));
    int x = (*((o1.a).data));
    int y = (*((o1.b).data));
}


void function_4 () {
    Data_1 d;
    Data_1 * p1;
    Data_1 * p2;
    (p1 = (&d));
    (p2 = p1);
    (((*p2).value) = 100);
    printf("p2.value = %d\n",((*p2).value));
}


void function_5 () {
    Data_2 d1;
    Data_2 d2;
    ((d1.nested) = (&d2));
    Data_2 copy = (*(d1.nested));
    printf("d1.nested = %p\n",(&(d1.nested)));
    printf("copy.nested = %p\n",(&(copy.nested)));
}


void function_6 () {
    AAA a;
    BBB b;
    ((a.b) = (&b));
    CCC c;
    (((*(a.b)).c) = (&c));
    ((b.c) = (&c));
    (((*((*(a.b)).c)).value) = 42699);
    printf("a.b.c.value = %d\n",((*((*(a.b)).c)).value));
}


void function_7 () {
    Config c;
    int val = 42;
    ((c.data) = (&val));
    int x = (*(c.data));
}


void function_8 () {
    Data_3 local;
    modify((&local));
    int y = (*(local.ptr));
}


void modify (Data_3 * d) {
    int val = 10;
    (((*d).ptr) = (&val));
}


void function_9 () {
    Data_4 d;
    int i = 2544;
    ((d.ptr) = (&i));
    ((*(d.ptr)) = 224);
    printf("*d.ptr = %d\n",(*(d.ptr)));
}


void simulate (PhysicsObject * obj) {
    update(obj);
    (frame = (frame + 1));
    printf("Frame %d: (%d, %d)\n",frame,(((*obj).position).x),(((*obj).position).y));
}


void function_10 () {
    PhysicsObject obj;
    (((obj.position).x) = 0);
    (((obj.position).y) = 0);
    (((obj.velocity).x) = 5);
    (((obj.velocity).y) = 3);
    printf("Initial position: %d, %d\n",((obj.position).x),((obj.position).y));
    printf("Velocity: %d, %d\n",((obj.velocity).x),((obj.velocity).y));
    simulate((&obj));
    simulate((&obj));
    simulate((&obj));
    simulate((&obj));
    simulate((&obj));
    simulate((&obj));
    simulate((&obj));
    simulate((&obj));
    (((obj.velocity).y) = (((obj.velocity).y) - 10));
    update((&obj));
    printf("After gravity: (%d, %d) velocity.y=%d\n",((obj.position).x),((obj.position).y),((obj.velocity).y));
}


void update (PhysicsObject * obj) {
    ((((*obj).position).x) = ((((*obj).position).x) + (((*obj).velocity).x)));
    ((((*obj).position).y) = ((((*obj).position).y) + (((*obj).velocity).y)));
}



void GENERATED_MAIN(){
    function_1();
    function_3();
    function_4();
    function_5();
    function_6();
    function_7();
    function_8();
    function_9();
    function_10();
    printf("\n---THE END---\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
