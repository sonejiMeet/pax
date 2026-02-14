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
struct Point;
struct Container;
struct Data;
struct some;
struct Node;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
Dynamic_Array __NewArray_impl(s64 count, s64 element_size);
int add(int a, int b);
int mul(int a, int b);
void print_(Point * p);
Container create_data(void);
void testing_malloc_and_sizeof(void);
void random_modules_testing(void);
void print_list(Node * n);
void testing_linked_node_using_malloc(void);
void fibonacci_series(void);
int fib(int x);

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
struct Point {
    int x;
    int y;
};
struct Container {
    int value;
    Point * data;
};
struct Data {
    int x;
    int y;
    char * name;
};
struct some {
    some * local;
};
struct Node {
    int value;
    Node * next;
};

/*BSS SECTION GLOBAL VARIAABLES*/
s64 temp = 0;

void __init_global_static_arrays(){
}

inline void _init_Static_Array(Static_Array* self){
}

inline void _init_Dynamic_Array(Dynamic_Array* self){
}

inline void _init_Point(Point* self){
    self->x = -24;
    self->y = -24;
}

inline void _init_Container(Container* self){
    self->value = -24;
}

inline void _init_Data(Data* self){
    self->x = -24;
    self->y = -24;
}

inline void _init_some(some* self){
}

inline void _init_Node(Node* self){
    self->value = -24;
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

#line 15 "C:/pax/tests/fake.pax"
int add (int a, int b) {
#line 16 "C:/pax/tests/fake.pax"
    return (a + b);
}

#line 19 "C:/pax/tests/fake.pax"
int mul (int a, int b) {
    int result = (a  *b);
#line 21 "C:/pax/tests/fake.pax"
    return result;
}

#line 24 "C:/pax/tests/fake.pax"
void print_ (Point * p) {
#line 26 "C:/pax/tests/fake.pax"
    printf("Point: x=%d, y=%d\n",((*p).x),((*p).y));
}

#line 28 "C:/pax/tests/fake.pax"
Container create_data () {
    Container c;
    _init_Container(&c);
#line 30 "C:/pax/tests/fake.pax"
    ((c.value) = 999);
#line 31 "C:/pax/tests/fake.pax"
    return c;
}

#line 32 "C:/pax/tests/demo_memory.pax"
void testing_malloc_and_sizeof () {
    s64 a = (sizeof(int)  *8);
#line 36 "C:/pax/tests/demo_memory.pax"
    printf("a= %d\n",a);
    int * ptr = (int *)malloc((sizeof(int)  *10));
    some l;
    _init_some(&l);
    some * s = (some *)malloc(sizeof(some));
#line 40 "C:/pax/tests/demo_memory.pax"
    ((l.local) = s);
    some * ss = (l.local);
    s64 aa = sizeof(some);
#line 46 "C:/pax/tests/demo_memory.pax"
    printf("aa= %d\n",aa);
}

#line 48 "C:/pax/tests/demo_memory.pax"
void random_modules_testing () {
    int res = add(10,20);
#line 55 "C:/pax/tests/demo_memory.pax"
    printf("add(10, 20) = %d\n",res);
    Point p;
    _init_Point(&p);
#line 56 "C:/pax/tests/demo_memory.pax"
    ((p.x) = 100);
#line 57 "C:/pax/tests/demo_memory.pax"
    ((p.y) = 200);
#line 61 "C:/pax/tests/demo_memory.pax"
    print_((&p));
    Container data = create_data();
    _init_Container(&data);
#line 64 "C:/pax/tests/demo_memory.pax"
    printf("data.value = %d\n",(data.value));
    Data d;
    _init_Data(&d);
#line 65 "C:/pax/tests/demo_memory.pax"
    ((d.x) = 42);
#line 66 "C:/pax/tests/demo_memory.pax"
    ((d.y) = 84);
#line 67 "C:/pax/tests/demo_memory.pax"
    ((d.name) = "Main Data");
#line 71 "C:/pax/tests/demo_memory.pax"
    printf("\nLocal struct: x=%d, y=%d, name=%s\n",(d.x),(d.y),(d.name));
#line 74 "C:/pax/tests/demo_memory.pax"
    printf("\nDONE\n");
}

#line 82 "C:/pax/tests/demo_memory.pax"
void print_list (Node * n) {
#line 83 "C:/pax/tests/demo_memory.pax"
    if((n == nullptr)){
#line 83 "C:/pax/tests/demo_memory.pax"
        return;
    }
#line 85 "C:/pax/tests/demo_memory.pax"
    (temp = (temp + 1));
#line 87 "C:/pax/tests/demo_memory.pax"
    printf("Node %d %d\n",temp,((*n).value));
#line 88 "C:/pax/tests/demo_memory.pax"
    print_list(((*n).next));
}

#line 90 "C:/pax/tests/demo_memory.pax"
void testing_linked_node_using_malloc () {
    Node * head = (Node *)malloc(sizeof(Node));
#line 94 "C:/pax/tests/demo_memory.pax"
    (((*head).value) = 10);
#line 95 "C:/pax/tests/demo_memory.pax"
    (((*head).next) = nullptr);
    Node * second = (Node *)malloc(sizeof(Node));
#line 98 "C:/pax/tests/demo_memory.pax"
    (((*second).value) = 20);
#line 99 "C:/pax/tests/demo_memory.pax"
    (((*second).next) = nullptr);
#line 101 "C:/pax/tests/demo_memory.pax"
    (((*head).next) = second);
#line 105 "C:/pax/tests/demo_memory.pax"
    print_list(head);
#line 106 "C:/pax/tests/demo_memory.pax"
    free(second);
#line 107 "C:/pax/tests/demo_memory.pax"
    free(head);
}

#line 111 "C:/pax/tests/demo_memory.pax"
void fibonacci_series () {
    s64 x = 10;
    int d = fib(x);
#line 119 "C:/pax/tests/demo_memory.pax"
    printf("\nfib of %d = %d\n",x,d);
}

#line 121 "C:/pax/tests/demo_memory.pax"
int fib (int x) {
#line 122 "C:/pax/tests/demo_memory.pax"
    if((x <= 0)){
#line 123 "C:/pax/tests/demo_memory.pax"
        return 0;
    }
#line 125 "C:/pax/tests/demo_memory.pax"
    if((x == 1)){
#line 126 "C:/pax/tests/demo_memory.pax"
        return 1;
    }
#line 128 "C:/pax/tests/demo_memory.pax"
    return (fib((x - 1)) + fib((x - 2)));
}


#line 6 "C:/pax/tests/demo_memory.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
#line 7 "C:/pax/tests/demo_memory.pax"
    printf("\n---------------------------\n");
#line 8 "C:/pax/tests/demo_memory.pax"
    testing_malloc_and_sizeof();
#line 9 "C:/pax/tests/demo_memory.pax"
    printf("\n---------------------------\n");
#line 10 "C:/pax/tests/demo_memory.pax"
    random_modules_testing();
#line 11 "C:/pax/tests/demo_memory.pax"
    printf("\n---------------------------\n");
#line 12 "C:/pax/tests/demo_memory.pax"
    testing_linked_node_using_malloc();
#line 14 "C:/pax/tests/demo_memory.pax"
    printf("\n---------------------------\n");
#line 16 "C:/pax/tests/demo_memory.pax"
    fibonacci_series();
#line 17 "C:/pax/tests/demo_memory.pax"
    printf("\nALL DONE\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
