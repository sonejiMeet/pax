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

/*BSS SECTION GLOBAL VARIAABLES*/

/*STRUCT FORWARD DECLARATIONS*/
struct City;
struct Address;
struct Person;
struct Document;
struct Teacher;
struct Room;
struct Book;
struct Course;
struct Building;
struct Faculty;
struct Campus;
struct University;
struct Node;
struct Company;
struct Employee;
struct Config;
struct Schema;
struct Database;
struct Statistics;
struct Table;
struct DataType;
struct Column;
struct Student_;
struct Teacher_;
struct Classroom;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct City {
char * name;
int population = -24;
};
struct Address {
char * street;
City city;
};
struct Person {
char * name;
Address address;
};
struct Document {
char * title;
Person author;
};
struct Teacher {
char * name;
int id = -24;
};
struct Room {
int number = -24;
char * building;
};
struct Book {
char * title;
char * author;
};
struct Course {
char * name;
Teacher instructor;
Room classroom;
Book textbook;
};
struct Building {
char * name;
int floors = -24;
};
struct Faculty {
char * dean;
Building location;
};
struct Campus {
int size = -24;
Building main_building;
};
struct University {
char * name;
Faculty faculty;
Campus facilities;
};
struct Node {
int value = -24;
Node * next;
Node * prev;
};
struct Company {
char * name;
Employee * employees;
};
struct Employee {
char * name;
Company * employer;
};
struct Config {
int timeout = -24;
int retry_count = -24;
};
struct Schema {
int version = -24;
Config config;
};
struct Database {
char * name;
Table * tables;
Schema metadata;
};
struct Statistics {
int row_count = -24;
int size_bytes = -24;
};
struct Table {
char * name;
Column * columns;
Table * next_table;
Statistics stats;
};
struct DataType {
char * name;
int size = -24;
};
struct Column {
char * name;
DataType type_info;
Column * next;
};
struct Student_ {
char * name;
int grade = -24;
};
struct Teacher_ {
char * name;
char * subject;
};
struct Classroom {
Teacher_ teacher;
Student_ students;
};

/*FUNCTION BODIES*/


void GENERATED_MAIN(){
    printf("test ended");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
