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
String New_String(u8 * cstr);
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
void test_array_of_String(void);
void test_upper_to_lower_string(void);
int to_lower(int ch);

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
    s64 count;
    u8 * data;
};

/*BSS SECTION GLOBAL VARIAABLES*/
s64 A = 65;
s64 Z = 90;
s64 a = 32;

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

String New_String (u8 * cstr) {
    String s;
    _init_String(&s);
    ((s.count) = strlen(cstr));
    ((s.data) = (u8 *)((void *)malloc(((s.count) + 1))));
    if(((s.data) != nullptr)){
        memcpy((s.data),cstr,((s.count) + 1));
    }
    return s;
}

String concat (String a, String b) {
    String result;
    _init_String(&result);
    ((result.count) = ((a.count) + (b.count)));
    ((result.data) = (u8 *)((void *)malloc(((result.count) + 1))));
    if(((result.data) != nullptr)){
        memcpy((result.data),(a.data),(a.count));
        memcpy(((result.data) + (a.count)),(b.data),(b.count));
        ((*((result.data) + (result.count))) = (u8)(0));
    }
    return result;
}

String slice (String s, s64 start, s64 count) {
    String result;
    _init_String(&result);
    if((start < 0)){
        (start = 0);
    }
    if((start >= (s.count))){
        return empty();
    }
    if(((start + count) > (s.count))){
        (count = ((s.count) - start));
    }
    ((result.count) = count);
    ((result.data) = (u8 *)((void *)malloc((count + 1))));
    if(((result.data) != nullptr)){
        memcpy((result.data),((s.data) + start),count);
        ((*((result.data) + count)) = (u8)(0));
    }
    return result;
}

s64 string_compare (String a, String b) {
    s64 min = (a.count);
    if(((b.count) < min)){
        (min = (b.count));
    }
    s64 i = 0;
    while((i < min)){
        if(((*((a.data) + i)) != (*((b.data) + i)))){
            return ((s64)((*((a.data) + i))) - (s64)((*((b.data) + i))));
        }
        (i = (i + 1));
    }
    return ((a.count) - (b.count));
}

bool equals (String a, String b) {
    return (((a.count) == (b.count)) && (string_compare(a,b) == 0));
}

u8 get (String s, s64 index) {
    if(((index < 0) || (index >= (s.count)))){
        return (u8)(0);
    }
    return (*((s.data) + index));
}

String empty () {
    String s;
    _init_String(&s);
    ((s.data) = nullptr);
    ((s.count) = 0);
    return s;
}

s64 len (String s) {
    return (s.count);
}

bool is_empty (String s) {
    return ((s.count) == 0);
}

void destroy_str (String * s) {
    if((((*s).data) != nullptr)){
        free(((*s).data));
        (((*s).data) = nullptr);
        (((*s).count) = 0);
    }
}

String string_clone (String s) {
    String result;
    _init_String(&result);
    ((result.count) = (s.count));
    ((result.data) = (u8 *)((void *)malloc(((s.count) + 1))));
    if(((result.data) != nullptr)){
        memcpy((result.data),(s.data),((s.count) + 1));
    }
    return result;
}

void printstr (String s) {
    printf("{{\"%.*s\"}, {%d}}\n",(s.count),(s.data),(s.count));
}

void test_array_of_String () {
    String __data__p_str[5];
    Static_Array p_str;
    p_str.data = (void *)__data__p_str;
    p_str.count = 5;
    for(int _i=0; _i < 5; ++_i) _init_String(&((String*)__data__p_str)[_i]);
    struct Static_Array * arr = (&p_str);
    ((((String*)(*arr).data)[0].data) = (u8 *)("idk what should I write"));
    printf("arr[0] = %s\n",(((String*)(*arr).data)[0].data));
    int a = 256;
    s8 * b = (s8 *)((&a));
    printf("b = %d\n",(*b));
    ((((String*)p_str.data)[1]) = New_String((u8 *)("this string is on the heap")));
    printstr((((String*)p_str.data)[1]));
    String idk = (((String*)p_str.data)[1]);
    _init_String(&idk);
    printstr(idk);
}

void test_upper_to_lower_string () {
    u8 * str = (u8 *)("THIS STRING WAS IN UPPER CASE BUT NOW ITS IN LOWER CASE :)");
    while((*str)){
        printf("%c",to_lower((int)((*str))));
        (str = (str + 1));
    }
}

int to_lower (int ch) {
    if(((ch >= A) && (ch <= Z))){
        return (ch + a);
    }
    else {
        return ch;
    }
}



void GENERATED_MAIN(){
    __init_global_static_arrays();
    printf("\n");
    {
        printf("\n----------string on heap as a builtin struct-------------\n");
        String a = New_String((u8 *)("Hello"));
        _init_String(&a);
        printstr(a);
        String b = New_String((u8 *)(" World"));
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
    }
    {
        printf("\n----------string on stack (read only)-------------\n");
        String aa;
        _init_String(&aa);
        u8 * bb = (u8 *)("Bruh");
        ((aa.data) = bb);
        ((aa.count) = strlen(bb));
        printf("%s, %d\n",(aa.data),(aa.count));
        printstr(aa);
    }
    {
        printf("-----------string_compare-------------\n");
        String str_1 = New_String((u8 *)("This is a test"));
        _init_String(&str_1);
        String str_2 = New_String((u8 *)("This is a test"));
        _init_String(&str_2);
        if((string_compare(str_1,str_2) == 0)){
            printf("\nBoth strings are the same\n");
        }
        else {
            printf("\nBoth strings are different\n");
        }
        destroy_str((&str_1));
        destroy_str((&str_2));
    }
    {
        String to_clone = New_String((u8 *)("clone this string"));
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
    }
    {
        printf("--------array subscript String---------\n");
        String str = New_String((u8 *)("Hello"));
        _init_String(&str);
        printstr(str);
        s64 i = 0;
        while((i != (str.count))){
            printf("str[%d] = %c\n",i,(u8)(((str.data)[i])));
            (i = (i + 1));
        }
        destroy_str((&str));
    }
    {
        printf("\n----------test_array_of_String()-------\n");
        test_array_of_String();
        printf("\n-------test_upper_to_lower_string()----\n");
        test_upper_to_lower_string();
    }
    printf("\n-------------------------------\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
