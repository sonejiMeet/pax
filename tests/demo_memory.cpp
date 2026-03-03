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
    u8 * name;
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
Dynamic_Array __NewArray_impl (s64 count, s64 element_size) {
    Dynamic_Array arr;
    _init_Dynamic_Array(&arr);
    ((arr.data) = (void *)malloc((count * element_size)));
    ((arr.count) = count);
    ((arr.allocated) = count);
    return arr;
}

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
    _init_Container(&c);
    ((c.value) = 999);
    return c;
}

void testing_malloc_and_sizeof () {
    s64 a = (sizeof(int) * 8);
    printf("a= %d\n",a);
    int * ptr = (int *)malloc((sizeof(int) * 10));
    some l;
    _init_some(&l);
    some * s = (some *)malloc(sizeof(some));
    ((l.local) = s);
    some * ss = (l.local);
    s64 aa = sizeof(some);
    printf("aa= %d\n",aa);
}

void random_modules_testing () {
    int res = add(10,20);
    printf("add(10, 20) = %d\n",res);
    Point p;
    _init_Point(&p);
    ((p.x) = 100);
    ((p.y) = 200);
    print_((&p));
    Container data = create_data();
    _init_Container(&data);
    printf("data.value = %d\n",(data.value));
    Data d;
    _init_Data(&d);
    ((d.x) = 42);
    ((d.y) = 84);
    ((d.name) = (u8 *)("Main Data"));
    printf("\nLocal struct: x=%d, y=%d, name=%s\n",(d.x),(d.y),(d.name));
    printf("\nDONE\n");
}

void print_list (Node * n) {
    if((n == nullptr)){
        return;
    }
    (temp = (temp + 1));
    printf("Node %d %d\n",temp,((*n).value));
    print_list(((*n).next));
}

void testing_linked_node_using_malloc () {
    Node * head = (Node *)malloc(sizeof(Node));
    (((*head).value) = 10);
    (((*head).next) = nullptr);
    Node * second = (Node *)malloc(sizeof(Node));
    (((*second).value) = 20);
    (((*second).next) = nullptr);
    (((*head).next) = second);
    print_list(head);
    free(second);
    free(head);
}

void fibonacci_series () {
    s64 x = 10;
    int d = fib(x);
    printf("\nfib of %d = %d\n",x,d);
}

int fib (int x) {
    if((x <= 0)){
        return 0;
    }
    if((x == 1)){
        return 1;
    }
    return (fib((x - 1)) + fib((x - 2)));
}



void GENERATED_MAIN(){
    __init_global_static_arrays();
    printf("\n---------------------------\n");
    testing_malloc_and_sizeof();
    printf("\n---------------------------\n");
    random_modules_testing();
    printf("\n---------------------------\n");
    testing_linked_node_using_malloc();
    printf("\n---------------------------\n");
    fibonacci_series();
    printf("\nALL DONE\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
