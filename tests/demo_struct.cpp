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
void print_struct(Student * stud, char * str);
void newline(void);

/*STRUCTS DEFINITIONS*/
struct Subject {
    char * name;
};
struct Student {
    int grade;
    float * gpa;
    char * name;
    Subject sub;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Subject(Subject* self){
    self->name = "Math";
}

inline void _init_Student(Student* self){
    self->grade = -24;
    self->name = "Default";
    _init_Subject(&self->sub);
}

/*FUNCTION BODIES*/
#line 3 "C:/pax/tests/demo_struct.pax"
void func1 (Student * stud) {
    Student local = (*stud);
    _init_Student(&local);
#line 6 "C:/pax/tests/demo_struct.pax"
    ((local.grade) = 0);
#line 10 "C:/pax/tests/demo_struct.pax"
    print_struct((&local),"local");
    Student * local2 = (&local);
#line 13 "C:/pax/tests/demo_struct.pax"
    (stud = local2);
}

#line 16 "C:/pax/tests/demo_struct.pax"
void func2 (Student * stud) {
#line 18 "C:/pax/tests/demo_struct.pax"
    (((*stud).grade) = 10);
#line 19 "C:/pax/tests/demo_struct.pax"
    (((*stud).name) = "Skibidi");
#line 20 "C:/pax/tests/demo_struct.pax"
    ((((*stud).sub).name) = "Architecture");
}

#line 23 "C:/pax/tests/demo_struct.pax"
void func3 (Student stud) {
    Student * sstud = (&stud);
#line 26 "C:/pax/tests/demo_struct.pax"
    (((*sstud).grade) = 10);
#line 27 "C:/pax/tests/demo_struct.pax"
    (((*sstud).name) = "Ligma");
#line 28 "C:/pax/tests/demo_struct.pax"
    ((((*sstud).sub).name) = "Operating sytem");
#line 30 "C:/pax/tests/demo_struct.pax"
    print_struct((&stud),"func3 stud");
}

#line 44 "C:/pax/tests/demo_struct.pax"
void print_struct (Student * stud, char * str) {
#line 46 "C:/pax/tests/demo_struct.pax"
    newline();
#line 47 "C:/pax/tests/demo_struct.pax"
    printf("%s.grade = %d\n",str,((*stud).grade));
#line 48 "C:/pax/tests/demo_struct.pax"
    printf("%s.name = %s\n",str,((*stud).name));
#line 50 "C:/pax/tests/demo_struct.pax"
    printf("%s.sub.name = %s\n",str,(((*stud).sub).name));
}

#line 156 "C:/pax/tests/demo_struct.pax"
void newline () {
#line 156 "C:/pax/tests/demo_struct.pax"
    printf("\n");
}


#line 55 "C:/pax/tests/demo_struct.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
#line 57 "C:/pax/tests/demo_struct.pax"
    printf("------------------------------------");
#line 67 "C:/pax/tests/demo_struct.pax"
#line 67 "C:/pax/tests/demo_struct.pax"
    {
        Student stud;
        _init_Student(&stud);
#line 59 "C:/pax/tests/demo_struct.pax"
        ((stud.grade) = 5902);
#line 60 "C:/pax/tests/demo_struct.pax"
        ((stud.name) = "idkkkk");
#line 63 "C:/pax/tests/demo_struct.pax"
        print_struct((&stud),"stud");
#line 64 "C:/pax/tests/demo_struct.pax"
        func1((&stud));
#line 66 "C:/pax/tests/demo_struct.pax"
        print_struct((&stud),"stud");
    }
#line 68 "C:/pax/tests/demo_struct.pax"
    printf("------------------------------------");
#line 78 "C:/pax/tests/demo_struct.pax"
#line 78 "C:/pax/tests/demo_struct.pax"
    {
        Student stud;
        _init_Student(&stud);
#line 70 "C:/pax/tests/demo_struct.pax"
        ((stud.grade) = 5902);
#line 71 "C:/pax/tests/demo_struct.pax"
        ((stud.name) = "idkkkk");
#line 74 "C:/pax/tests/demo_struct.pax"
        print_struct((&stud),"stud");
#line 75 "C:/pax/tests/demo_struct.pax"
        func2((&stud));
#line 77 "C:/pax/tests/demo_struct.pax"
        print_struct((&stud),"stud");
    }
#line 80 "C:/pax/tests/demo_struct.pax"
    printf("------------------------------------");
#line 88 "C:/pax/tests/demo_struct.pax"
#line 88 "C:/pax/tests/demo_struct.pax"
    {
        Student stud;
        _init_Student(&stud);
#line 82 "C:/pax/tests/demo_struct.pax"
        ((stud.grade) = 5902);
#line 83 "C:/pax/tests/demo_struct.pax"
        ((stud.name) = "idkkkk");
#line 85 "C:/pax/tests/demo_struct.pax"
        print_struct((&stud),"stud");
#line 86 "C:/pax/tests/demo_struct.pax"
        func3(stud);
    }
#line 92 "C:/pax/tests/demo_struct.pax"
    printf("------------------------------------");
#line 124 "C:/pax/tests/demo_struct.pax"
#line 124 "C:/pax/tests/demo_struct.pax"
    {
#line 95 "C:/pax/tests/demo_struct.pax"
        newline();
        Student b;
        _init_Student(&b);
#line 96 "C:/pax/tests/demo_struct.pax"
        ((b.grade) = 1);
#line 99 "C:/pax/tests/demo_struct.pax"
        printf("b.grade = %d\n",(b.grade));
#line 105 "C:/pax/tests/demo_struct.pax"
        newline();
        Student * s = (&b);
#line 106 "C:/pax/tests/demo_struct.pax"
        (((*s).grade) = 2332);
#line 108 "C:/pax/tests/demo_struct.pax"
        printf("b.grade = %d\n",(b.grade));
#line 109 "C:/pax/tests/demo_struct.pax"
        printf("s.grade = %d\n",((*s).grade));
#line 111 "C:/pax/tests/demo_struct.pax"
        newline();
        Student * * ss = (&s);
#line 112 "C:/pax/tests/demo_struct.pax"
        (((**ss).grade) = 2000);
#line 114 "C:/pax/tests/demo_struct.pax"
        printf("b.grade = %d\n",(b.grade));
#line 115 "C:/pax/tests/demo_struct.pax"
        printf("ss.grade = %d\n",((**ss).grade));
#line 117 "C:/pax/tests/demo_struct.pax"
        newline();
        Student * * * sss = (&ss);
#line 118 "C:/pax/tests/demo_struct.pax"
        (((***sss).grade) = 3000);
#line 120 "C:/pax/tests/demo_struct.pax"
        printf("b.grade = %d\n",(b.grade));
#line 121 "C:/pax/tests/demo_struct.pax"
        printf("sss.grade = %d\n",((***sss).grade));
#line 122 "C:/pax/tests/demo_struct.pax"
        newline();
    }
#line 126 "C:/pax/tests/demo_struct.pax"
    printf("------------------------------------");
#line 148 "C:/pax/tests/demo_struct.pax"
#line 148 "C:/pax/tests/demo_struct.pax"
    {
#line 128 "C:/pax/tests/demo_struct.pax"
        newline();
        Student b;
        _init_Student(&b);
        Student * s = (&b);
        float f = 4;
#line 132 "C:/pax/tests/demo_struct.pax"
        printf("&f = %p\n",(&f));
#line 132 "C:/pax/tests/demo_struct.pax"
        (((*s).gpa) = (&f));
#line 134 "C:/pax/tests/demo_struct.pax"
        printf("s.gpa = %f\n",(*((*s).gpa)));
#line 137 "C:/pax/tests/demo_struct.pax"
        printf("s.gpa = %p\n",((*s).gpa));
#line 138 "C:/pax/tests/demo_struct.pax"
        printf("\nf = %f\n",f);
#line 140 "C:/pax/tests/demo_struct.pax"
        ((*((*s).gpa)) = 6420.00000000000000000);
#line 141 "C:/pax/tests/demo_struct.pax"
        printf("s.gpa = %f\n",(*((*s).gpa)));
#line 143 "C:/pax/tests/demo_struct.pax"
        printf("f = %f\n",f);
#line 144 "C:/pax/tests/demo_struct.pax"
        printf("^s.gpa = %p\n",(&(*((*s).gpa))));
#line 146 "C:/pax/tests/demo_struct.pax"
        printf("^f = %p\n",(&f));
    }
#line 153 "C:/pax/tests/demo_struct.pax"
    printf("------------------------------------");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
