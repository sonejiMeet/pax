/* GENERATED FILE */

#include <stdlib.h>
#include <stdio.h>

int a = (((1 + ((2 * 3) * 4)) + 5) / 2);
int b = (4 + (5 * 2));
int c = ((5 * 2) + 4);
int d = ((5 * 2) / 4);
float e = ((2 / 4) * 5);
int h = 1;
int num = (2 * (5 - h));
bool something = true;

int func (int * aa) {
    printf("Address of aa = %p\n",aa);
    int something = (/*DEREF*/ * aa * 2);
    (/*DEREF*/ * aa = 699);
    int * ss = aa;
    printf("Address of ss = %p\n",ss);
    (/*DEREF*/ * ss = (/*DEREF*/ * ss + 2));
    return something;
}

int idk = 1;
bool boolean = true;

void GENERATED_MAIN(){
    {
        int f = 100;
        printf("Address of f = %p\n",/*ADDRESS_OF*/ &f);
        int af = func(/*ADDRESS_OF*/ &f);
        printf("f is equal to: %d\n",f);
        printf("af is equal to: %d\n",af);
    }
    {
        int bruh = 333;
        int * ligma = /*ADDRESS_OF*/ &bruh;
        (/*DEREF*/ * ligma = 420);
        int what = /*DEREF*/ * ligma;
        printf("what = %d\n",what);
        printf("bruh = %d\n",bruh);
    }
    {
        printf("\n");
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
    {
        printf("\n");
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
    int arrStaticInt[4];
    printf("\n");
    int v = 4;
    if((a != b)){
        float x = 2334.340088;
        printf("x's value = %f\n",x);
        {
            float block_inside_if = 3.100000;
        }
        int block_inside_if = 0;
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
    float scopedint = 5.900000;
    (scopedint = 2);
    printf("scopedint=%f\n",scopedint);
    int zeroChild = 0;
    {
        int g = 5;
        (g = 3);
        int firstChild = 0;
        {
            int secondChild = 0;
        }
    }
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
