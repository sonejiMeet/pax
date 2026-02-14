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
struct SS;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
Dynamic_Array __NewArray_impl(s64 count, s64 element_size);
Static_Array * func(struct Static_Array * arr);

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
struct SS {
    Static_Array a;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Static_Array(Static_Array* self){
}

inline void _init_Dynamic_Array(Dynamic_Array* self){
}

inline void _init_SS(SS* self){
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

#line 107 "C:/pax/tests/demo_static_array.pax"
Static_Array * func (struct Static_Array * arr) {
    struct Static_Array * local = arr;
#line 109 "C:/pax/tests/demo_static_array.pax"
    ((((int*)(*local).data)[0]) = 2434355);
#line 111 "C:/pax/tests/demo_static_array.pax"
    printf("local[0] = %d\n",(((int*)(*local).data)[0]));
#line 111 "C:/pax/tests/demo_static_array.pax"
    return local;
}


#line 6 "C:/pax/tests/demo_static_array.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
#line 7 "C:/pax/tests/demo_static_array.pax"
    printf("----------------------------------------\n");
#line 15 "C:/pax/tests/demo_static_array.pax"
#line 15 "C:/pax/tests/demo_static_array.pax"
    {
        int __data__a[4];
        Static_Array a;
        a.data = (void *)__data__a;
        a.count = 4;
        struct Static_Array * arr = (&a);
#line 10 "C:/pax/tests/demo_static_array.pax"
        ((((int*)(*arr).data)[1]) = 14);
#line 12 "C:/pax/tests/demo_static_array.pax"
        printf("arr[1] = %d\n",(((int*)(*arr).data)[1]));
#line 14 "C:/pax/tests/demo_static_array.pax"
        printf("arr.count = %d\n",((*arr).count));
    }
#line 19 "C:/pax/tests/demo_static_array.pax"
    printf("----------------------------------------\n");
#line 32 "C:/pax/tests/demo_static_array.pax"
#line 32 "C:/pax/tests/demo_static_array.pax"
    {
        int __data__a[4];
        Static_Array a;
        a.data = (void *)__data__a;
        a.count = 4;
#line 21 "C:/pax/tests/demo_static_array.pax"
        ((((int*)a.data)[0]) = (-354));
#line 23 "C:/pax/tests/demo_static_array.pax"
        printf("a[0] = %d\n",(((int*)a.data)[0]));
#line 24 "C:/pax/tests/demo_static_array.pax"
        printf("a.data = %p\n",(a.data));
#line 25 "C:/pax/tests/demo_static_array.pax"
        printf("a.count = %d\n",(a.count));
        struct Static_Array * b = (&a);
#line 26 "C:/pax/tests/demo_static_array.pax"
        ((((int*)(*b).data)[0]) = 897599);
#line 28 "C:/pax/tests/demo_static_array.pax"
        printf("b[0] = %d\n",(((int*)(*b).data)[0]));
#line 29 "C:/pax/tests/demo_static_array.pax"
        printf("b.data = %p\n",((*b).data));
#line 31 "C:/pax/tests/demo_static_array.pax"
        printf("b.count = %d\n",((*b).count));
    }
#line 36 "C:/pax/tests/demo_static_array.pax"
    printf("----------------------------------------\n");
#line 56 "C:/pax/tests/demo_static_array.pax"
#line 56 "C:/pax/tests/demo_static_array.pax"
    {
        int __data__a[4];
        Static_Array a;
        a.data = (void *)__data__a;
        a.count = 4;
#line 38 "C:/pax/tests/demo_static_array.pax"
        ((((int*)a.data)[0]) = 834);
#line 40 "C:/pax/tests/demo_static_array.pax"
        printf("a[0] = %d\n",(((int*)a.data)[0]));
        struct Static_Array * b = (&a);
        int pc = 449895;
#line 45 "C:/pax/tests/demo_static_array.pax"
        printf("pc = %p\n",(&pc));
        int* __data__c[4];
        Static_Array c;
        c.data = (void *)__data__c;
        c.count = 4;
#line 46 "C:/pax/tests/demo_static_array.pax"
        ((((int**)c.data)[3]) = (&pc));
#line 48 "C:/pax/tests/demo_static_array.pax"
        printf("c[3] = %p\n",(((int**)c.data)[3]));
#line 50 "C:/pax/tests/demo_static_array.pax"
        printf("c[3] = %d\n",(*(((int**)c.data)[3])));
#line 50 "C:/pax/tests/demo_static_array.pax"
        (b = (&a));
#line 51 "C:/pax/tests/demo_static_array.pax"
        ((((int*)(*b).data)[0]) = (*(((int**)c.data)[3])));
#line 54 "C:/pax/tests/demo_static_array.pax"
        printf("b[0] = %d\n",(((int*)(*b).data)[0]));
        Static_Array * d;
    }
#line 65 "C:/pax/tests/demo_static_array.pax"
    printf("----------------------------------------\n");
#line 76 "C:/pax/tests/demo_static_array.pax"
#line 76 "C:/pax/tests/demo_static_array.pax"
    {
        int __data__a[4];
        Static_Array a;
        a.data = (void *)__data__a;
        a.count = 4;
#line 67 "C:/pax/tests/demo_static_array.pax"
        ((((int*)a.data)[0]) = 2);
#line 69 "C:/pax/tests/demo_static_array.pax"
        printf("a[0] = %d\n",(((int*)a.data)[0]));
        Static_Array * ddd = func((&a));
#line 72 "C:/pax/tests/demo_static_array.pax"
        printf("a[0] = %d\n",(((int*)a.data)[0]));
#line 73 "C:/pax/tests/demo_static_array.pax"
        printf("ddd[0] = %d\n",(((int*)(*ddd).data)[0]));
#line 73 "C:/pax/tests/demo_static_array.pax"
        ((((int*)(*ddd).data)[0]) = 33);
#line 75 "C:/pax/tests/demo_static_array.pax"
        printf("ddd[0] = %d\n",(((int*)(*ddd).data)[0]));
    }
#line 81 "C:/pax/tests/demo_static_array.pax"
    printf("----------------------------------------\n");
#line 101 "C:/pax/tests/demo_static_array.pax"
#line 101 "C:/pax/tests/demo_static_array.pax"
    {
        int __data__s_a[4];
        SS s;
        s.a.data = (void *)__data__s_a;
        s.a.count = 4;
        _init_SS(&s);
#line 83 "C:/pax/tests/demo_static_array.pax"
        ((((int*)(s.a).data)[2]) = 6948999);
#line 86 "C:/pax/tests/demo_static_array.pax"
        printf("s.a[2] = %d\n",(((int*)(s.a).data)[2]));
#line 88 "C:/pax/tests/demo_static_array.pax"
        printf("s.a.count = %d\n",((s.a).count));
        int __data__ar[4];
        Static_Array ar;
        ar.data = (void *)__data__ar;
        ar.count = 4;
#line 89 "C:/pax/tests/demo_static_array.pax"
        ((((int*)ar.data)[2]) = (-2469));
#line 91 "C:/pax/tests/demo_static_array.pax"
        ((s.a) = ar);
#line 94 "C:/pax/tests/demo_static_array.pax"
        printf("s.a[2] = %d\n",(((int*)(s.a).data)[2]));
#line 96 "C:/pax/tests/demo_static_array.pax"
        printf("s.a.count = %d\n",((s.a).count));
        SS * p_s = (&s);
#line 98 "C:/pax/tests/demo_static_array.pax"
        printf("p_s.a[2] = %d\n",(((int*)((*p_s).a).data)[2]));
#line 100 "C:/pax/tests/demo_static_array.pax"
        printf("p_s.a.count = %d\n",(((*p_s).a).count));
    }
#line 104 "C:/pax/tests/demo_static_array.pax"
    printf("----------------------------------------\n");
#line 105 "C:/pax/tests/demo_static_array.pax"
    printf("DONEEEE\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
