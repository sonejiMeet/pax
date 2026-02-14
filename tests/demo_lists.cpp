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

#line 33 "C:/pax/tests/demo_lists.pax"
Node * make_list (s64 val) {
    Node * base = (Node *)malloc(sizeof(Node));
#line 36 "C:/pax/tests/demo_lists.pax"
    (((*base).value) = val);
#line 37 "C:/pax/tests/demo_lists.pax"
    (((*base).next) = nullptr);
#line 38 "C:/pax/tests/demo_lists.pax"
    return base;
}

#line 42 "C:/pax/tests/demo_lists.pax"
Node * do_list (struct Static_Array * arr) {
    s64 i = 0;
    Node * base = nullptr;
    Node * temp;
#line 47 "C:/pax/tests/demo_lists.pax"
    while((i != ((*arr).count))){
#line 48 "C:/pax/tests/demo_lists.pax"
        if((!base)){
#line 49 "C:/pax/tests/demo_lists.pax"
            (temp = (Node *)malloc(sizeof(Node)));
#line 51 "C:/pax/tests/demo_lists.pax"
            (((*temp).value) = (((s64*)(*arr).data)[i]));
#line 52 "C:/pax/tests/demo_lists.pax"
            (((*temp).next) = nullptr);
#line 53 "C:/pax/tests/demo_lists.pax"
            (base = temp);
        }
        else {
#line 55 "C:/pax/tests/demo_lists.pax"
            (((*temp).next) = (Node *)malloc(sizeof(Node)));
#line 56 "C:/pax/tests/demo_lists.pax"
            (((*((*temp).next)).value) = (((s64*)(*arr).data)[i]));
#line 57 "C:/pax/tests/demo_lists.pax"
            (temp = ((*temp).next));
#line 58 "C:/pax/tests/demo_lists.pax"
            (((*temp).next) = nullptr);
        }
#line 61 "C:/pax/tests/demo_lists.pax"
        (i = (i + 1));
    }
#line 64 "C:/pax/tests/demo_lists.pax"
    return base;
}

#line 67 "C:/pax/tests/demo_lists.pax"
void print_whole_list (Node * _list) {
#line 68 "C:/pax/tests/demo_lists.pax"
    while(_list){
#line 70 "C:/pax/tests/demo_lists.pax"
        printf("%d\n",((*_list).value));
#line 70 "C:/pax/tests/demo_lists.pax"
        (_list = ((*_list).next));
    }
}


#line 5 "C:/pax/tests/demo_lists.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    Node n;
    _init_Node(&n);
#line 8 "C:/pax/tests/demo_lists.pax"
    printf("n.value %d\n",(n.value));
    Node * ll = make_list(50);
#line 11 "C:/pax/tests/demo_lists.pax"
    printf("ll.value %d\n",((*ll).value));
    s64 __data__arr[5];
    Static_Array arr;
    arr.data = (void *)__data__arr;
    arr.count = 5;
#line 12 "C:/pax/tests/demo_lists.pax"
    ((((s64*)arr.data)[0]) = 56677);
#line 13 "C:/pax/tests/demo_lists.pax"
    ((((s64*)arr.data)[1]) = (-23592));
#line 14 "C:/pax/tests/demo_lists.pax"
    ((((s64*)arr.data)[2]) = 3352);
#line 15 "C:/pax/tests/demo_lists.pax"
    ((((s64*)arr.data)[3]) = (-579));
#line 16 "C:/pax/tests/demo_lists.pax"
    ((((s64*)arr.data)[4]) = 2555);
    Node * list = do_list((&arr));
#line 22 "C:/pax/tests/demo_lists.pax"
    print_whole_list(list);
#line 23 "C:/pax/tests/demo_lists.pax"
    printf("---DONE---\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
