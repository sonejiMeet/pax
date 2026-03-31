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
struct Node;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
Dynamic_Array __NewArray_impl(s64 count, s64 element_size);
Node * make_list(s64 val);
Node * do_list(struct Static_Array * arr);
void print_whole_list(Node * _list);

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
struct Node {
    int value;
    Node * next;
};

/*BSS SECTION GLOBAL VARIAABLES*/
s64 x = (-2247);

void __init_global_static_arrays(){
}

inline void _init_Static_Array(Static_Array* self){
}

inline void _init_Dynamic_Array(Dynamic_Array* self){
}

inline void _init_Node(Node* self){
    self->value = x;
    self->next = nullptr;
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

Node * make_list (s64 val) {
    Node * base = (Node *)malloc(sizeof(Node));
    (((*base).value) = val);
    (((*base).next) = nullptr);
    return base;
}

Node * do_list (struct Static_Array * arr) {
    Node * base = nullptr;
    Node * temp;
    s64 i = 0;
    while((i < ((*arr).count))){
        if((!base)){
            (temp = (Node *)malloc(sizeof(Node)));
            (((*temp).value) = (((s64*)(*arr).data)[i]));
            (((*temp).next) = nullptr);
            (base = temp);
        }
        else {
            (((*temp).next) = (Node *)malloc(sizeof(Node)));
            (((*((*temp).next)).value) = (((s64*)(*arr).data)[i]));
            (temp = ((*temp).next));
            (((*temp).next) = nullptr);
        }
        (i = (i + 1));
    }
    return base;
}

void print_whole_list (Node * _list) {
    while(_list){
        printf("%d\n",((*_list).value));
        (_list = ((*_list).next));
    }
}



void GENERATED_MAIN(){
    __init_global_static_arrays();
    Node n;
    _init_Node(&n);
    printf("n.value = %d\n",(n.value));
    Node * ll = make_list(50);
    printf("ll.value = %d\n",((*ll).value));
    s64 __data__arr[5];
    Static_Array arr;
    arr.data = (void *)__data__arr;
    arr.count = 5;
    ((((s64*)arr.data)[0]) = 1);
    ((((s64*)arr.data)[1]) = 2);
    ((((s64*)arr.data)[2]) = 3);
    ((((s64*)arr.data)[3]) = 4);
    ((((s64*)arr.data)[4]) = 5);
    Node * list = do_list((&arr));
    printf("------------------------------\n");
    print_whole_list(list);
    printf("------------------------------\n");
    printf("---DONE---\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
