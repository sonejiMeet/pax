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
struct String;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
Dynamic_Array __NewArray_impl(s64 count, s64 element_size);
s64 strlen(u8 * str);
String new_string(u8 * cstr);
String concat(String a, String b);
String slice(String s, s64 start, s64 count);
s64 string_compare(String a, String b);
bool equals(String a, String b);
u8 get(String s, s64 index);
String empty(void);
s64 len(String s);
bool is_empty(String s);
void destroy_str(String * s);
String string_clone(String s);
void printstr(String s);

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
struct String {
    s64 length;
    u8 * data;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Static_Array(Static_Array* self){
}

inline void _init_Dynamic_Array(Dynamic_Array* self){
}

inline void _init_String(String* self){
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

s64 strlen (u8 * str) {
    s64 len = 0;
    while(((*(str + len)) != 0)){
        (len = (len + 1));
    }
    return len;
}

String new_string (u8 * cstr) {
    String s;
    _init_String(&s);
    ((s.length) = strlen(cstr));
    ((s.data) = (u8 *)((void *)malloc(((s.length) + 1))));
    if(((s.data) != nullptr)){
        memcpy((s.data),cstr,((s.length) + 1));
    }
    return s;
}

String concat (String a, String b) {
    String result;
    _init_String(&result);
    ((result.length) = ((a.length) + (b.length)));
    ((result.data) = (u8 *)((void *)malloc(((result.length) + 1))));
    if(((result.data) != nullptr)){
        memcpy((result.data),(a.data),(a.length));
        memcpy(((result.data) + (a.length)),(b.data),(b.length));
        ((*((result.data) + (result.length))) = (u8)(0));
    }
    return result;
}

String slice (String s, s64 start, s64 count) {
    String result;
    _init_String(&result);
    if((start < 0)){
        (start = 0);
    }
    if((start >= (s.length))){
        return empty();
    }
    if(((start + count) > (s.length))){
        (count = ((s.length) - start));
    }
    ((result.length) = count);
    ((result.data) = (u8 *)((void *)malloc((count + 1))));
    if(((result.data) != nullptr)){
        memcpy((result.data),((s.data) + start),count);
        ((*((result.data) + count)) = (u8)(0));
    }
    return result;
}

s64 string_compare (String a, String b) {
    s64 min = (a.length);
    if(((b.length) < min)){
        (min = (b.length));
    }
    s64 i = 0;
    while((i < min)){
        if(((*((a.data) + i)) != (*((b.data) + i)))){
            return ((s64)((*((a.data) + i))) - (s64)((*((b.data) + i))));
        }
        (i = (i + 1));
    }
    return ((a.length) - (b.length));
}

bool equals (String a, String b) {
    return (((a.length) == (b.length)) && (string_compare(a,b) == 0));
}

u8 get (String s, s64 index) {
    if(((index < 0) || (index >= (s.length)))){
        return (u8)(0);
    }
    return (*((s.data) + index));
}

String empty () {
    String s;
    _init_String(&s);
    ((s.data) = nullptr);
    ((s.length) = 0);
    return s;
}

s64 len (String s) {
    return (s.length);
}

bool is_empty (String s) {
    return ((s.length) == 0);
}

void destroy_str (String * s) {
    if((((*s).data) != nullptr)){
        free(((*s).data));
        (((*s).data) = nullptr);
        (((*s).length) = 0);
    }
}

String string_clone (String s) {
    String result;
    _init_String(&result);
    ((result.length) = (s.length));
    ((result.data) = (u8 *)((void *)malloc(((s.length) + 1))));
    if(((result.data) != nullptr)){
        memcpy((result.data),(s.data),((s.length) + 1));
    }
    return result;
}

void printstr (String s) {
    printf("{{\"%.*s\"}, {%d}}\n",(s.length),(s.data),(s.length));
}



void GENERATED_MAIN(){
    __init_global_static_arrays();
    printf("\n");
    printf("\n----------string on heap as a builtin struct-------------\n");
    String a = new_string((u8 *)("Hello"));
    _init_String(&a);
    printstr(a);
    String b = new_string((u8 *)(" World"));
    _init_String(&b);
    printstr(b);
    printf("\n----------concatenate two string together-------------\n");
    String c = concat(a,b);
    _init_String(&c);
    printstr(c);
    printf("\n------------slice string-----------------\n");
    String sub = slice(c,0,5);
    _init_String(&sub);
    printf("sliced string = ");
    printstr(sub);
    destroy_str((&c));
    destroy_str((&sub));
    printf("\n----------string on stack-------------\n");
    String aa;
    _init_String(&aa);
    u8 * bb = (u8 *)("Bruh");
    ((aa.data) = bb);
    ((aa.length) = strlen(bb));
    printstr(aa);
    printf("-----------string_compare-------------\n");
    String str_1 = new_string((u8 *)("This is a test"));
    _init_String(&str_1);
    String str_2 = new_string((u8 *)("This is a test"));
    _init_String(&str_2);
    if((string_compare(str_1,str_2) == 0)){
        printf("\nBoth strings are the same\n");
    }
    else {
        printf("\nBoth strings are different\n");
    }
    destroy_str((&str_1));
    destroy_str((&str_2));
    String to_clone = new_string((u8 *)("clone this string"));
    _init_String(&to_clone);
    String cloned = string_clone(to_clone);
    _init_String(&cloned);
    if((string_compare(to_clone,cloned) == 0)){
        printf("\nCloned string is the same\n");
    }
    else {
        printf("\nCloned string is different\n");
    }
    destroy_str((&to_clone));
    destroy_str((&cloned));
    printf("---------------------------------\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
