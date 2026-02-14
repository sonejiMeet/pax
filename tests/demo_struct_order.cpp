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
    int population;
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
    int id;
};
struct Room {
    int number;
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
    int floors;
};
struct Faculty {
    char * dean;
    Building location;
};
struct Campus {
    int size;
    Building main_building;
};
struct University {
    char * name;
    Faculty faculty;
    Campus facilities;
};
struct Node {
    int value;
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
    int timeout;
    int retry_count;
};
struct Schema {
    int version;
    Config config;
};
struct Database {
    char * name;
    Table * tables;
    Schema metadata;
};
struct Statistics {
    int row_count;
    int size_bytes;
};
struct Table {
    char * name;
    Column * columns;
    Table * next_table;
    Statistics stats;
};
struct DataType {
    char * name;
    int size;
};
struct Column {
    char * name;
    DataType type_info;
    Column * next;
};
struct Student_ {
    char * name;
    int grade;
};
struct Teacher_ {
    char * name;
    char * subject;
};
struct Classroom {
    Teacher_ teacher;
    Student_ students;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_City(City* self){
    self->population = -24;
}

inline void _init_Address(Address* self){
    _init_City(&self->city);
}

inline void _init_Person(Person* self){
    _init_Address(&self->address);
}

inline void _init_Document(Document* self){
    _init_Person(&self->author);
}

inline void _init_Teacher(Teacher* self){
    self->id = -24;
}

inline void _init_Room(Room* self){
    self->number = -24;
}

inline void _init_Book(Book* self){
}

inline void _init_Course(Course* self){
    _init_Teacher(&self->instructor);
    _init_Room(&self->classroom);
    _init_Book(&self->textbook);
}

inline void _init_Building(Building* self){
    self->floors = -24;
}

inline void _init_Faculty(Faculty* self){
    _init_Building(&self->location);
}

inline void _init_Campus(Campus* self){
    self->size = -24;
    _init_Building(&self->main_building);
}

inline void _init_University(University* self){
    _init_Faculty(&self->faculty);
    _init_Campus(&self->facilities);
}

inline void _init_Node(Node* self){
    self->value = -24;
}

inline void _init_Company(Company* self){
}

inline void _init_Employee(Employee* self){
}

inline void _init_Config(Config* self){
    self->timeout = -24;
    self->retry_count = -24;
}

inline void _init_Schema(Schema* self){
    self->version = -24;
    _init_Config(&self->config);
}

inline void _init_Database(Database* self){
    _init_Schema(&self->metadata);
}

inline void _init_Statistics(Statistics* self){
    self->row_count = -24;
    self->size_bytes = -24;
}

inline void _init_Table(Table* self){
    _init_Statistics(&self->stats);
}

inline void _init_DataType(DataType* self){
    self->size = -24;
}

inline void _init_Column(Column* self){
    _init_DataType(&self->type_info);
}

inline void _init_Student_(Student_* self){
    self->grade = -24;
}

inline void _init_Teacher_(Teacher_* self){
}

inline void _init_Classroom(Classroom* self){
    _init_Teacher_(&self->teacher);
    _init_Student_(&self->students);
}

/*FUNCTION BODIES*/

#line 172 "C:/pax/tests/demo_struct_order.pax"
void GENERATED_MAIN(){
    __init_global_static_arrays();
#line 173 "C:/pax/tests/demo_struct_order.pax"
    printf("test ended");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
