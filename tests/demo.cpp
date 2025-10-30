/* GENERATED FILE */

#include <stdlib.h>
#include <stdio.h>
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

s64 a = (((1 + ((2 * 3) * 4)) + 5) / 2);
s64 b = (4 + (5 * 2));
s64 c = ((5 * 2) + 4);
s64 d = ((5 * 2) / 4);
float e = ((2 / 4) * 5);
s64 h = 1;
int num = (2 * (5 - h));
bool something = true;
s64 cc = (((3 * 4) + 5) / 2);
float dd = ((6 / 3.00000000000000000) + (2 * 7));
s64 ee = ((10 - (4 / 2)) + (3 * 5));
s64 ff = ((1 + (2 * 3)) - (4 / 5));
s64 gg = (((7 * 8) + 9) / (10 - 2));
float hh = ((5 * 2) + ((3 * 4) / 2.00000000000000000));

bool func1 () {
    int f = (-100);
    func((&f),a);
    if((f != 100)){
        (f = 2);
        return true;
    }
    else {
        return false;
    }
}

int idk = 1;
bool boolean = true;

int func (int * aa, int sssd) {
    printf("Address of aa = %p\n",aa);
    s64 something = ((*aa) * 2);
    ((*aa) = 699);
    int * ss = aa;
    printf("Address of ss = %p\n",ss);
    ((*ss) = ((*ss) + 2));
    return something;
}


void newline () {
    printf("\n");
}


void GENERATED_MAIN(){
    s8 gamba = 127;
    s16 fsdf = 3;
    float32 bgiksd = 2.00000009999999984;
    float bgiksd1 = 2.00000009999999984;
    printf("gamba = %d\n",gamba);
    {
        int a = 2;
        int * var = (&a);
        int * * var2 = ((&var) + 1);
        printf("var = %x\n",(&var));
        printf("var2 = %x\n",(&var2));
    }
    s64 vvv = ((-2) - 4);
    {
        int f = 100;
        printf("Address of f = %p\n",(&f));
        int af = func((&f),a);
        printf("f = %d\n",f);
        bool ret2 = func1();
        printf("af =  %d\n",af);
        if((ret2 == false)){
            printf("ret2 == false\n");
        }
        if((ret2 == true)){
            printf("ret2 == true\n");
        }
    }
    {
        s64 Name = 100;
        printf("Name = %d\n",Name);
        s64 * Name2 = (&Name);
        ((*Name2) = ((*Name2) * 420));
        printf("Name = %d\n",Name);
        s64 Name3 = (*Name2);
        printf("Name3 = %d\n",Name3);
    }
    {
        newline();
        int aint = 5;
        (aint = 10);
        printf("aint = %d\n",aint);
        printf("aint = %p\n",(&aint));
        int * pInt = (&aint);
        printf("pInt = %p\n",pInt);
        int * p_uninit_int;
        (p_uninit_int = pInt);
        printf("p_uninit_int = %d\n",(*p_uninit_int));
        ((*p_uninit_int) = 20);
        printf("p_uninit_int = %d\n",(*p_uninit_int));
        printf("aint = %d\n",aint);
        int * rint = (&aint);
        printf("rint = %p\n",rint);
        ((*rint) = 30);
        printf("aint = %d\n",aint);
        ((*p_uninit_int) = 40);
        printf("p_uninit_int = %d\n",(*p_uninit_int));
        printf("aint = %d\n",aint);
    }
    {
        newline();
        int what = 999;
        printf("what = %d\n",what);
        int * ligma = (&what);
        ((*ligma) = 68);
        int what1 = (*ligma);
        printf("what1 = %d\n",what1);
        printf("what = %d\n",what);
        int * * whatever = (&ligma);
        ((*(*whatever)) = 82);
        printf("what = %d\n",what);
        int * * * whatever2 = (&whatever);
        ((*(*(*whatever2))) = (*(*whatever)));
        printf("what = %d\n",what);
        ((*(*(*whatever2))) = 96);
        printf("what = %d\n",what);
    }
    int arrStaticInt[4];
    {
        s64 me = 5;
        s64 * mee = (&me);
        int meee = ((*mee) * (*mee));
        printf("meee = %d\n",meee);
        s64 * * m2 = (&mee);
        int m3 = ((*(*m2)) * (*(*m2)));
        printf("m3 = %d\n",m3);
        printf("me = %p\n",(&me));
        printf("mee = %p\n",mee);
        printf("m2 = %p\n",(*m2));
    }
    printf("\n");
    int v = 4;
    if((a != b)){
        float x = ((2223434243434334 - 94) - 294.13435453457407220);
        printf("x's value = %.8f\n",x);
        {
            float block_inside_if = 3.10000000000000009;
        }
        s64 block_inside_if = 0;
    }
    else {
        printf("a == b\n");
    }
    printf("a=%d\n",a);
    printf("b=%d\n",b);
    printf("c=%d\n",c);
    printf("d=%d\n",d);
    printf("e=%f\n",e);
    printf("h=%d\n",h);
    float scopedint = 5.00000000000000000;
    (scopedint = 2.00000000000000000);
    printf("scopedint=%f\n",scopedint);
    int zeroChild = 0;
    {
        s64 g = 5;
        (g = 3);
        s64 firstChild = 0;
        (firstChild = 22);
        {
            s64 secondChild = 0;
            (secondChild = 335);
        }
    }
    (idk = 293824);
    printf("\nidk = %d\n",idk);
    (boolean = false);
    printf("\n");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
