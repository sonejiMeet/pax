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
struct ArrNode;
struct Node2;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
Dynamic_Array __NewArray_impl(s64 count, s64 element_size);

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
    Static_Array arr;
};
struct ArrNode {
    int an;
};
struct Node2 {
    Static_Array arrNode;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Static_Array(Static_Array* self){
}

inline void _init_Dynamic_Array(Dynamic_Array* self){
}

inline void _init_Node(Node* self){
}

inline void _init_ArrNode(ArrNode* self){
    self->an = -24;
}

inline void _init_Node2(Node2* self){
    for(int _i=0; _i < 4; ++_i) _init_ArrNode(&((ArrNode*)self->arrNode.data)[_i]);
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


#line 5 "C:/pax/tests/demo_array_subscript_dot.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    int __data__n_arr[4];
    Node n;
    n.arr.data = (void *)__data__n_arr;
    n.arr.count = 4;
    _init_Node(&n);
#line 6 "C:/pax/tests/demo_array_subscript_dot.pax"
    ((((int*)(n.arr).data)[0]) = (-22353));
#line 9 "C:/pax/tests/demo_array_subscript_dot.pax"
    printf("n.arr[0] = %d\n",(((int*)(n.arr).data)[0]));
    ArrNode __data__arn[4];
    Static_Array arn;
    arn.data = (void *)__data__arn;
    arn.count = 4;
    for(int _i=0; _i < 4; ++_i) _init_ArrNode(&((ArrNode*)__data__arn)[_i]);
#line 10 "C:/pax/tests/demo_array_subscript_dot.pax"
    (((((ArrNode*)arn.data)[0]).an) = (-2435));
#line 13 "C:/pax/tests/demo_array_subscript_dot.pax"
    printf("arn[0].an = %d\n",((((ArrNode*)arn.data)[0]).an));
    ArrNode __data__nn_arrNode[4];
    Node2 nn;
    nn.arrNode.data = (void *)__data__nn_arrNode;
    nn.arrNode.count = 4;
    _init_Node2(&nn);
#line 14 "C:/pax/tests/demo_array_subscript_dot.pax"
    (((((ArrNode*)(nn.arrNode).data)[0]).an) = (-6781));
#line 16 "C:/pax/tests/demo_array_subscript_dot.pax"
    printf("nn.arrNode[0].an = %d\n",((((ArrNode*)(nn.arrNode).data)[0]).an));
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
