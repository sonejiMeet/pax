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
struct f;
struct f;
struct Subject;
struct Student;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void func1(Student * stud);
void func2(Student * stud);
void func3(Student stud);
void print_struct(Student * stud, char * str);
void ss1(void);
void newline(void);

/*STRUCTS DEFINITIONS*/
struct f {
};
struct f {
};
struct Subject {
char * name = "Math";
};
struct Student {
int grade;
float * gpa;
char * name = "Default";
Subject sub;
};


void func1 (Student * stud) {
    Student local = (*stud);
    ((local.grade) = 0);
    print_struct((&local),"local");
    Student * local2 = (&local);
    (stud = local2);
}


void func2 (Student * stud) {
    (((*stud).grade) = 10);
    (((*stud).name) = "Skibidi");
    ((((*stud).sub).name) = "Architecture");
}


void func3 (Student stud) {
    Student * sstud = (&stud);
    (((*sstud).grade) = 10);
    (((*sstud).name) = "Ligma");
    ((((*sstud).sub).name) = "Operating sytem");
    print_struct((&stud),"func3 stud");
}


void print_struct (Student * stud, char * str) {
    newline();
    printf("%s.grade = %d\n",str,((*stud).grade));
    printf("%s.name = %s\n",str,((*stud).name));
    printf("%s.sub.name = %s\n",str,(((*stud).sub).name));
}

s64 x = 0;

void ss1 () {
    if((x == 10)){
        return;
    }
    (x = (x + 1));
    printf("x = %d\n",x);
    ss1();
}


void newline () {
    printf("\n");
}


void GENERATED_MAIN(){
    printf("------------------------------------");
    {
        Student stud;
        ((stud.grade) = 5902);
        ((stud.name) = "idkkkk");
        print_struct((&stud),"stud");
        func1((&stud));
        print_struct((&stud),"stud");
    }
    printf("------------------------------------");
    {
        Student stud;
        ((stud.grade) = 5902);
        ((stud.name) = "idkkkk");
        print_struct((&stud),"stud");
        func2((&stud));
        print_struct((&stud),"stud");
    }
    printf("------------------------------------");
    {
        Student stud;
        ((stud.grade) = 5902);
        ((stud.name) = "idkkkk");
        print_struct((&stud),"stud");
        func3(stud);
    }
    printf("------------------------------------");
    {
        newline();
        Student b;
        ((b.grade) = 1);
        printf("b.grade = %d\n",(b.grade));
        newline();
        Student * s = (&b);
        (((*s).grade) = 2332);
        printf("b.grade = %d\n",(b.grade));
        printf("s.grade = %d\n",((*s).grade));
        newline();
        Student * * ss = (&s);
        (((**ss).grade) = 2000);
        printf("b.grade = %d\n",(b.grade));
        printf("ss.grade = %d\n",((**ss).grade));
        newline();
        Student * * * sss = (&ss);
        (((***sss).grade) = 3000);
        printf("b.grade = %d\n",(b.grade));
        printf("sss.grade = %d\n",((***sss).grade));
        newline();
    }
    printf("------------------------------------");
    {
        newline();
        Student b;
        Student * s = (&b);
        float f = 4;
        printf("&f = %p\n",(&f));
        (((*s).gpa) = (&f));
        printf("s.gpa = %f\n",(*((*s).gpa)));
        printf("s.gpa = %p\n",((*s).gpa));
        ((*((*s).gpa)) = 5.00000000000000000);
        printf("s.gpa = %f\n",(*((*s).gpa)));
    }
    printf("------------------------------------");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
