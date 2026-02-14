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

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
bool func1(void);
int func(int * aa, int sssd);
void newline(void);

/*STRUCTS DEFINITIONS*/

/*BSS SECTION GLOBAL VARIAABLES*/
s64 a = (((1 + ((2  *3)  *4)) + 5) / 2);
s64 b = (4 + (5  *2));
s64 c = ((5  *2) + 4);
s64 d = ((5  *2) / 4);
float e = ((2 / 4)  *5);
s64 h = 1;
int num = (2  *(5 - h));
bool something = true;
s64 cc = (((3  *4) + 5) / 2);
float dd = ((6 / 3.00000000000000000) + (2  *7));
s64 ee = ((10 - (4 / 2)) + (3  *5));
s64 ff = ((1 + (2  *3)) - (4 / 5));
s64 gg = (((7  *8) + 9) / (10 - 2));
float hh = ((5  *2) + ((3  *4) / 2.00000000000000000));
int idk = 1;
bool boolean = true;

void __init_global_static_arrays(){
}

/*FUNCTION BODIES*/
#line 26 "C:/pax/tests/demo.pax"
bool func1 () {
    int f = (-100);
#line 29 "C:/pax/tests/demo.pax"
    func((&f),a);
#line 29 "C:/pax/tests/demo.pax"
    if((f != 100)){
#line 30 "C:/pax/tests/demo.pax"
        (f = 2);
#line 31 "C:/pax/tests/demo.pax"
        return true;
    }
    else {
#line 34 "C:/pax/tests/demo.pax"
        return false;
    }
}

#line 228 "C:/pax/tests/demo.pax"
int func (int * aa, int sssd) {
#line 242 "C:/pax/tests/demo.pax"
    printf("Address of aa = %p\n",aa);
    s64 something = ((*aa)  *2);
#line 245 "C:/pax/tests/demo.pax"
    ((*aa) = 699);
    int * ss = aa;
#line 248 "C:/pax/tests/demo.pax"
    printf("Address of ss = %p\n",ss);
#line 249 "C:/pax/tests/demo.pax"
    ((*ss) = ((*ss) + 2));
#line 249 "C:/pax/tests/demo.pax"
    return something;
}

#line 252 "C:/pax/tests/demo.pax"
void newline () {
#line 254 "C:/pax/tests/demo.pax"
    printf("\n");
}


#line 40 "C:/pax/tests/demo.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
    s8 gamba = 127;
    s16 fsdf = 3;
    float32 bgiksd = 2.00000009999999984;
    float bgiksd1 = 2.00000009999999984;
#line 49 "C:/pax/tests/demo.pax"
    printf("gamba = %d\n",gamba);
#line 57 "C:/pax/tests/demo.pax"
#line 57 "C:/pax/tests/demo.pax"
    {
        int a = 2;
        int * var = (&a);
        int * * var2 = ((&var) + 1);
#line 54 "C:/pax/tests/demo.pax"
        printf("var = %x\n",(&var));
#line 55 "C:/pax/tests/demo.pax"
        printf("var2 = %x\n",(&var2));
    }
    s64 vvv = ((-2) - 4);
#line 77 "C:/pax/tests/demo.pax"
#line 77 "C:/pax/tests/demo.pax"
    {
        int f = 100;
#line 64 "C:/pax/tests/demo.pax"
        printf("Address of f = %p\n",(&f));
        int af = func((&f),a);
#line 67 "C:/pax/tests/demo.pax"
        printf("f = %d\n",f);
        bool ret2 = func1();
#line 69 "C:/pax/tests/demo.pax"
        printf("af =  %d\n",af);
#line 69 "C:/pax/tests/demo.pax"
        if((ret2 == false)){
#line 71 "C:/pax/tests/demo.pax"
            printf("ret2 == false\n");
        }
#line 72 "C:/pax/tests/demo.pax"
        if((ret2 == true)){
#line 74 "C:/pax/tests/demo.pax"
            printf("ret2 == true\n");
        }
    }
#line 90 "C:/pax/tests/demo.pax"
#line 90 "C:/pax/tests/demo.pax"
    {
        s64 Name = 100;
#line 82 "C:/pax/tests/demo.pax"
        printf("Name = %d\n",Name);
        s64 * Name2 = (&Name);
#line 84 "C:/pax/tests/demo.pax"
        ((*Name2) = ((*Name2)  *420));
#line 86 "C:/pax/tests/demo.pax"
        printf("Name = %d\n",Name);
        s64 Name3 = (*Name2);
#line 88 "C:/pax/tests/demo.pax"
        printf("Name3 = %d\n",Name3);
    }
#line 121 "C:/pax/tests/demo.pax"
#line 121 "C:/pax/tests/demo.pax"
    {
#line 93 "C:/pax/tests/demo.pax"
        newline();
        int aint = 5;
#line 94 "C:/pax/tests/demo.pax"
        (aint = 10);
#line 96 "C:/pax/tests/demo.pax"
        printf("aint = %d\n",aint);
#line 98 "C:/pax/tests/demo.pax"
        printf("aint = %p\n",(&aint));
        int * pInt = (&aint);
#line 102 "C:/pax/tests/demo.pax"
        printf("pInt = %p\n",pInt);
        int * p_uninit_int;
#line 103 "C:/pax/tests/demo.pax"
        (p_uninit_int = pInt);
#line 106 "C:/pax/tests/demo.pax"
        printf("p_uninit_int = %d\n",(*p_uninit_int));
#line 107 "C:/pax/tests/demo.pax"
        ((*p_uninit_int) = 20);
#line 108 "C:/pax/tests/demo.pax"
        printf("p_uninit_int = %d\n",(*p_uninit_int));
#line 110 "C:/pax/tests/demo.pax"
        printf("aint = %d\n",aint);
        int * rint = (&aint);
#line 113 "C:/pax/tests/demo.pax"
        printf("rint = %p\n",rint);
#line 114 "C:/pax/tests/demo.pax"
        ((*rint) = 30);
#line 116 "C:/pax/tests/demo.pax"
        printf("aint = %d\n",aint);
#line 117 "C:/pax/tests/demo.pax"
        ((*p_uninit_int) = 40);
#line 118 "C:/pax/tests/demo.pax"
        printf("p_uninit_int = %d\n",(*p_uninit_int));
#line 119 "C:/pax/tests/demo.pax"
        printf("aint = %d\n",aint);
    }
#line 164 "C:/pax/tests/demo.pax"
#line 164 "C:/pax/tests/demo.pax"
    {
#line 124 "C:/pax/tests/demo.pax"
        newline();
        int what = 999;
#line 127 "C:/pax/tests/demo.pax"
        printf("what = %d\n",what);
        int * ligma = (&what);
#line 130 "C:/pax/tests/demo.pax"
        ((*ligma) = 68);
        int what1 = (*ligma);
#line 132 "C:/pax/tests/demo.pax"
        printf("what1 = %d\n",what1);
#line 134 "C:/pax/tests/demo.pax"
        printf("what = %d\n",what);
        int * * whatever = (&ligma);
#line 136 "C:/pax/tests/demo.pax"
        ((*(*whatever)) = 82);
#line 138 "C:/pax/tests/demo.pax"
        printf("what = %d\n",what);
        int * * * whatever2 = (&whatever);
#line 140 "C:/pax/tests/demo.pax"
        ((*(*(*whatever2))) = (*(*whatever)));
#line 142 "C:/pax/tests/demo.pax"
        printf("what = %d\n",what);
#line 143 "C:/pax/tests/demo.pax"
        ((*(*(*whatever2))) = 96);
#line 145 "C:/pax/tests/demo.pax"
        printf("what = %d\n",what);
    }
#line 179 "C:/pax/tests/demo.pax"
#line 179 "C:/pax/tests/demo.pax"
    {
        s64 me = 5;
        s64 * mee = (&me);
        int meee = ((*mee)  *(*mee));
#line 170 "C:/pax/tests/demo.pax"
        printf("meee = %d\n",meee);
        s64 * * m2 = (&mee);
        int m3 = ((*(*m2))  *(*(*m2)));
#line 174 "C:/pax/tests/demo.pax"
        printf("m3 = %d\n",m3);
#line 175 "C:/pax/tests/demo.pax"
        printf("me = %p\n",(&me));
#line 176 "C:/pax/tests/demo.pax"
        printf("mee = %p\n",mee);
#line 177 "C:/pax/tests/demo.pax"
        printf("m2 = %p\n",(*m2));
    }
#line 181 "C:/pax/tests/demo.pax"
    printf("\n");
    int v = 4;
#line 182 "C:/pax/tests/demo.pax"
    if((a != b)){
        float x = ((2223434243434334 - 94) - 294.13435453457407220);
#line 185 "C:/pax/tests/demo.pax"
        printf("x's value = %.8f\n",x);
#line 188 "C:/pax/tests/demo.pax"
#line 188 "C:/pax/tests/demo.pax"
        {
            float block_inside_if = 3.10000000000000009;
        }
        s64 block_inside_if = 0;
    }
    else {
#line 192 "C:/pax/tests/demo.pax"
        printf("a == b\n");
    }
#line 194 "C:/pax/tests/demo.pax"
    printf("a=%d\n",a);
#line 195 "C:/pax/tests/demo.pax"
    printf("b=%d\n",b);
#line 196 "C:/pax/tests/demo.pax"
    printf("c=%d\n",c);
#line 197 "C:/pax/tests/demo.pax"
    printf("d=%d\n",d);
#line 198 "C:/pax/tests/demo.pax"
    printf("e=%f\n",e);
#line 200 "C:/pax/tests/demo.pax"
    printf("h=%d\n",h);
    float scopedint = 5.00000000000000000;
#line 201 "C:/pax/tests/demo.pax"
    (scopedint = 2.00000000000000000);
#line 204 "C:/pax/tests/demo.pax"
    printf("scopedint=%f\n",scopedint);
    int zeroChild = 0;
#line 216 "C:/pax/tests/demo.pax"
#line 216 "C:/pax/tests/demo.pax"
    {
        s64 g = 5;
#line 207 "C:/pax/tests/demo.pax"
        (g = 3);
        s64 firstChild = 0;
#line 209 "C:/pax/tests/demo.pax"
        (firstChild = 22);
#line 214 "C:/pax/tests/demo.pax"
#line 214 "C:/pax/tests/demo.pax"
        {
            s64 secondChild = 0;
#line 212 "C:/pax/tests/demo.pax"
            (secondChild = 335);
        }
    }
#line 216 "C:/pax/tests/demo.pax"
    (idk = 293824);
#line 219 "C:/pax/tests/demo.pax"
    printf("\nidk = %d\n",idk);
#line 219 "C:/pax/tests/demo.pax"
    (boolean = false);
#line 221 "C:/pax/tests/demo.pax"
    printf("\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
