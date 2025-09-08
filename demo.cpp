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
int idk = 1;
bool boolean = true;

void GENERATED_MAIN(){
    int aint = 5;
    int bint;
    (aint = 10);
    printf("aint = %d\n",aint);
    printf("aint = %p\n",&aint);
    int * pInt = &aint;
    printf("pInt = %p\n",pInt);
    int * p_uninit_int = pInt;
    printf("p_uninit_int = %d\n",* p_uninit_int);
    (* p_uninit_int = 20);
    printf("p_uninit_int = %d\n",* p_uninit_int);
    printf("aint = %d\n",aint);
    int * rint = &aint;
    printf("rint = %p\n",rint);
    (* rint = 30);
    printf("aint = %d\n",aint);
    (* p_uninit_int = 40);
    printf("p_uninit_int = %d\n",* p_uninit_int);
    printf("aint = %d\n",aint);
    int v = 4;
    if((a != b)){
        float x = 2334.340088;
        printf("x's value = %f\n",x);
        {
            int block_inside_if = 0;
        }
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
    (scopedint = 21);
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
