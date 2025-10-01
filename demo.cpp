/* GENERATED FILE */

#include <stdlib.h>
#include <stdio.h>

/*GLOBAL FUNCTION DECLARATIONS*/
bool func1(void);
int func(int * aa, int sssd);

int a = (((1 + ((2 * 3) * 4)) + 5) / 2);
int b = (4 + (5 * 2));
int c = ((5 * 2) + 4);
int d = ((5 * 2) / 4);
float e = ((2 / 4) * 5);
int h = 1;
int num = (2 * (5 - h));
bool something = true;
float cc = (((3 * 4) + 5) / 2.000000);
float dd = ((6 / 3.000000) + (2 * 7));
float ee = ((10 - (4 / 2.000000)) + (3 * 5));
float ff = ((1 + (2 * 3)) - (4.000000 / 5));
float gg = (((7 * 8) + 9) / (10 - 2.000000));
float hh = ((5 * 2) + ((3 * 4) / 2.000000));

bool func1 () {
    int f = -100;
    func(/*ADDRESS_OF*/ &f,a);
    printf("f inside func1 is equal to: %d\n",f);
    printf("f inside func1 is equal to: %p\n",/*ADDRESS_OF*/ &f);
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
    int something = (/*DEREF*/ * aa * 2);
    (/*DEREF*/ * aa = 699);
    int * ss = aa;
    printf("Address of ss = %p\n",ss);
    (/*DEREF*/ * ss = (/*DEREF*/ * ss + 2));
    return something;
}


void GENERATED_MAIN(){
    int numa = -2;
    printf("numa= %d\n",numa);
    printf("cc =%f\n",cc);
    printf("dd =%f\n",dd);
    printf("ee =%f\n",ee);
    printf("ff =%f\n",ff);
    printf("gg =%f\n",gg);
    printf("hh =%f\n",hh);
    {
        int f = 100;
        printf("Address of f = %p\n",/*ADDRESS_OF*/ &f);
        int af = func(/*ADDRESS_OF*/ &f,a);
        bool ret2 = func1();
        printf("f = %d\n",f);
        printf("af =  %d\n",af);
        if((ret2 == false)){
            printf("ret2 == false\n");
        }
        if((ret2 == true)){
            printf("ret2 == true\n");
        }
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
    int arrStaticInt[4];
    {
        int me = 5;
        int * mee = /*ADDRESS_OF*/ &me;
        int meee = (/*DEREF*/ * mee * /*DEREF*/ * mee);
        printf("meee = %d\n",meee);
        int * * m2 = /*ADDRESS_OF*/ &mee;
        int m3 = (/*DEREF*/ * /*DEREF*/ * m2 + /*DEREF*/ * /*DEREF*/ * m2);
        printf("m3 = %d\n",m3);
        printf("me = %p\n",/*ADDRESS_OF*/ &me);
        printf("mee = %p\n",mee);
        printf("m2 = %p\n",/*DEREF*/ * m2);
    }
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
        (firstChild = 22);
        {
            int secondChild = 0;
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
