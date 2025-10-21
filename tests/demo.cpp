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

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
bool func1(void);
int func(int * aa, int sssd);
void newline(void);

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
    func(/*ADDRESS_OF*/ &f,a);
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
    s64 something = (/*DEREF*/ * aa * 2);
    (/*DEREF*/ * aa = 699);
    int * ss = aa;
    printf("Address of ss = %p\n",ss);
    (/*DEREF*/ * ss = (/*DEREF*/ * ss + 2));
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
        int * var = /*ADDRESS_OF*/ &a;
        int * * var2 = (/*ADDRESS_OF*/ &var + 1);
        printf("var = %x\n",/*ADDRESS_OF*/ &var);
        printf("var2 = %x\n",/*ADDRESS_OF*/ &var2);
    }
    s64 vvv = ((-2) - 4);
    {
        int f = 100;
        printf("Address of f = %p\n",/*ADDRESS_OF*/ &f);
        int af = func(/*ADDRESS_OF*/ &f,a);
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
        s64 * Name2 = /*ADDRESS_OF*/ &Name;
        (/*DEREF*/ * Name2 = (/*DEREF*/ * Name2 * 420));
        printf("Name = %d\n",Name);
        s64 Name3 = /*DEREF*/ * Name2;
        printf("Name3 = %d\n",Name3);
    }
    {
        newline();
        int aint = 5;
        (aint = 10);
        printf("aint = %d\n",aint);
        printf("aint = %p\n",/*ADDRESS_OF*/ &aint);
        int * pInt = /*ADDRESS_OF*/ &aint;
        printf("pInt = %p\n",pInt);
        int * p_uninit_int;
        (p_uninit_int = pInt);
        printf("p_uninit_int = %d\n",/*DEREF*/ * p_uninit_int);
        (/*DEREF*/ * p_uninit_int = 20);
        printf("p_uninit_int = %d\n",/*DEREF*/ * p_uninit_int);
        printf("aint = %d\n",aint);
        int * rint = /*ADDRESS_OF*/ &aint;
        printf("rint = %p\n",rint);
        (/*DEREF*/ * rint = 30);
        printf("aint = %d\n",aint);
        (/*DEREF*/ * p_uninit_int = 40);
        printf("p_uninit_int = %d\n",/*DEREF*/ * p_uninit_int);
        printf("aint = %d\n",aint);
    }
    {
        newline();
        int what = 999;
        printf("what = %d\n",what);
        int * ligma = /*ADDRESS_OF*/ &what;
        (/*DEREF*/ * ligma = 68);
        int what1 = /*DEREF*/ * ligma;
        printf("what1 = %d\n",what1);
        printf("what = %d\n",what);
        int * * whatever = /*ADDRESS_OF*/ &ligma;
        (/*DEREF*/ * /*DEREF*/ * whatever = 82);
        printf("what = %d\n",what);
        int * * * whatever2 = /*ADDRESS_OF*/ &whatever;
        (/*DEREF*/ * /*DEREF*/ * /*DEREF*/ * whatever2 = /*DEREF*/ * /*DEREF*/ * whatever);
        printf("what = %d\n",what);
        (/*DEREF*/ * /*DEREF*/ * /*DEREF*/ * whatever2 = 96);
        printf("what = %d\n",what);
    }
    int arrStaticInt[4];
    {
        s64 me = 5;
        s64 * mee = /*ADDRESS_OF*/ &me;
        int meee = (/*DEREF*/ * mee * /*DEREF*/ * mee);
        printf("meee = %d\n",meee);
        s64 * * m2 = /*ADDRESS_OF*/ &mee;
        int m3 = (/*DEREF*/ * /*DEREF*/ * m2 * /*DEREF*/ * /*DEREF*/ * m2);
        printf("m3 = %d\n",m3);
        printf("me = %p\n",/*ADDRESS_OF*/ &me);
        printf("mee = %p\n",mee);
        printf("m2 = %p\n",/*DEREF*/ * m2);
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
