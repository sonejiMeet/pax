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
struct Subject;
struct Student;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/
void func1(Student * stud);
void func2(Student * stud);
void func3(Student stud);
void print_struct(Student * stud, u8 * str);
void newline(void);

/*STRUCTS DEFINITIONS*/
struct Subject {
    u8 * name;
};
struct Student {
    int grade;
    float * gpa;
    u8 * name;
    Subject sub;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Subject(Subject* self){
    self->name = (u8 *)("Math");
}

inline void _init_Student(Student* self){
    self->grade = -24;
    self->name = (u8 *)("Default");
    _init_Subject(&self->sub);
}

/*FUNCTION BODIES*/
void func1 (Student * stud) {
    Student local = (*stud);
    _init_Student(&local);
    ((local.grade) = 0);
    print_struct((&local),(u8 *)("local"));
    Student * local2 = (&local);
    (stud = local2);
}

void func2 (Student * stud) {
    (((*stud).grade) = 10);
    (((*stud).name) = (u8 *)("Skibidi"));
    ((((*stud).sub).name) = (u8 *)("Architecture"));
}

void func3 (Student stud) {
    Student * sstud = (&stud);
    (((*sstud).grade) = 10);
    (((*sstud).name) = (u8 *)("Ligma"));
    ((((*sstud).sub).name) = (u8 *)("Operating sytem"));
    print_struct((&stud),(u8 *)("func3 stud"));
}

void print_struct (Student * stud, u8 * str) {
    newline();
    printf("%s.grade = %d\n",str,((*stud).grade));
    printf("%s.name = %s\n",str,((*stud).name));
    printf("%s.sub.name = %s\n",str,(((*stud).sub).name));
}

void newline () {
    printf("\n");
}



void GENERATED_MAIN(){
    __init_global_static_arrays();
    printf("------------------------------------");
    {
        Student stud;
        _init_Student(&stud);
        ((stud.grade) = 5902);
        ((stud.name) = (u8 *)("idkkkk"));
        print_struct((&stud),(u8 *)("stud"));
        func1((&stud));
        print_struct((&stud),(u8 *)("stud"));
    }
    printf("------------------------------------");
    {
        Student stud;
        _init_Student(&stud);
        ((stud.grade) = 5902);
        ((stud.name) = (u8 *)("idkkkk"));
        print_struct((&stud),(u8 *)("stud"));
        func2((&stud));
        print_struct((&stud),(u8 *)("stud"));
    }
    printf("------------------------------------");
    {
        Student stud;
        _init_Student(&stud);
        ((stud.grade) = 5902);
        ((stud.name) = (u8 *)("idkkkk"));
        print_struct((&stud),(u8 *)("stud"));
        func3(stud);
    }
    printf("------------------------------------");
    {
        newline();
        Student b;
        _init_Student(&b);
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
        _init_Student(&b);
        Student * s = (&b);
        float f = 4;
        printf("&f = %p\n",(&f));
        (((*s).gpa) = (&f));
        printf("s.gpa = %f\n",(*((*s).gpa)));
        printf("s.gpa = %p\n",((*s).gpa));
        printf("\nf = %f\n",f);
        ((*((*s).gpa)) = 6420.00000000000000000);
        printf("s.gpa = %f\n",(*((*s).gpa)));
        printf("f = %f\n",f);
        printf("^s.gpa = %p\n",(&(*((*s).gpa))));
        printf("^f = %p\n",(&f));
    }
    printf("------------------------------------");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
