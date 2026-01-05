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

/*FUNCTION BODIES*/
Dynamic_Array __NewArray_impl (s64 count, s64 element_size) {
    Dynamic_Array arr;
    ((arr.data) = (void *)malloc((count * element_size)));
    ((arr.count) = count);
    ((arr.allocated) = count);
    return arr;
}

Static_Array * func (struct Static_Array * arr) {
    struct Static_Array * local = arr;
    ((((int*)(*local).data)[0]) = 2434355);
    printf("local[0] = %d\n",(((int*)(*local).data)[0]));
    return local;
}



void GENERATED_MAIN(){
    printf("----------------------------------------\n");
    {
        int __data__a[4];
        Static_Array a;
        a.data = (void *)__data__a;
        a.count = 4;
        struct Static_Array * arr = (&a);
        ((((int*)(*arr).data)[1]) = 14);
        printf("arr[1] = %d\n",(((int*)(*arr).data)[1]));
        printf("arr.count = %d\n",((*arr).count));
    }
    printf("----------------------------------------\n");
    {
        int __data__a[4];
        Static_Array a;
        a.data = (void *)__data__a;
        a.count = 4;
        ((((int*)a.data)[0]) = (-354));
        printf("a[0] = %d\n",(((int*)a.data)[0]));
        printf("a.data = %p\n",(a.data));
        printf("a.count = %d\n",(a.count));
        struct Static_Array * b = (&a);
        ((((int*)(*b).data)[0]) = 897599);
        printf("b[0] = %d\n",(((int*)(*b).data)[0]));
        printf("b.data = %p\n",((*b).data));
        printf("b.count = %d\n",((*b).count));
    }
    printf("----------------------------------------\n");
    {
        int __data__a[4];
        Static_Array a;
        a.data = (void *)__data__a;
        a.count = 4;
        ((((int*)a.data)[0]) = 834);
        printf("a[0] = %d\n",(((int*)a.data)[0]));
        struct Static_Array * b = (&a);
        int pc = 449895;
        printf("pc = %p\n",(&pc));
        int* __data__c[4];
        Static_Array c;
        c.data = (void *)__data__c;
        c.count = 4;
        ((((int**)c.data)[3]) = (&pc));
        printf("c[3] = %p\n",(((int**)c.data)[3]));
        printf("c[3] = %d\n",(*(((int**)c.data)[3])));
        (b = (&a));
        ((((int*)(*b).data)[0]) = (*(((int**)c.data)[3])));
        printf("b[0] = %d\n",(((int*)(*b).data)[0]));
        Static_Array * d;
    }
    printf("----------------------------------------\n");
    {
        int __data__a[4];
        Static_Array a;
        a.data = (void *)__data__a;
        a.count = 4;
        ((((int*)a.data)[0]) = 2);
        printf("a[0] = %d\n",(((int*)a.data)[0]));
        Static_Array * ddd = func((&a));
        printf("a[0] = %d\n",(((int*)a.data)[0]));
        printf("ddd[0] = %d\n",(((int*)(*ddd).data)[0]));
        ((((int*)(*ddd).data)[0]) = 33);
        printf("ddd[0] = %d\n",(((int*)(*ddd).data)[0]));
    }
    printf("----------------------------------------\n");
    {
        int __data__s_a[4];
        SS s;
        s.a.data = (void *)__data__s_a;
        s.a.count = 4;
        ((((int*)(s.a).data)[2]) = 6948999);
        printf("s.a[2] = %d\n",(((int*)(s.a).data)[2]));
        printf("s.a.count = %d\n",((s.a).count));
        int __data__ar[4];
        Static_Array ar;
        ar.data = (void *)__data__ar;
        ar.count = 4;
        ((((int*)ar.data)[2]) = (-2469));
        ((s.a) = ar);
        printf("s.a[2] = %d\n",(((int*)(s.a).data)[2]));
        printf("s.a.count = %d\n",((s.a).count));
        SS * p_s = (&s);
        printf("p_s.a[2] = %d\n",(((int*)((*p_s).a).data)[2]));
        printf("p_s.a.count = %d\n",(((*p_s).a).count));
    }
    printf("----------------------------------------\n");
    printf("DONEEEE\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
