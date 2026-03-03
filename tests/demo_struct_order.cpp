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
struct Engine;
struct Car;
struct Garage;
struct Author;
struct Publisher;
struct Book;
struct Address;
struct Person;
struct Company;
struct Employee;
struct Node;
struct Container;
struct Item;
struct Inventory;

/*GLOBAL FUNCTION FORWARD DECLARATIONS*/

/*STRUCTS DEFINITIONS*/
struct Engine {
    int horsepower;
};
struct Car {
    u8 * model;
    Engine engine;
};
struct Garage {
    u8 * location;
    Car car;
};
struct Author {
    u8 * name;
};
struct Publisher {
    u8 * name;
};
struct Book {
    u8 * title;
    Author author;
    Publisher publisher;
};
struct Address {
    u8 * street;
};
struct Person {
    u8 * name;
    Address home;
};
struct Company {
    u8 * name;
    Address hq;
};
struct Employee {
    u8 * name;
    Company works_at;
    Person lives_at;
};
struct Node {
    int data;
    Node * next;
    Node * prev;
};
struct Container {
    int value;
    Node * head;
};
struct Item {
    int id;
};
struct Inventory {
    Item items;
    int count;
};

/*BSS SECTION GLOBAL VARIAABLES*/

void __init_global_static_arrays(){
}

inline void _init_Engine(Engine* self){
    self->horsepower = -24;
}

inline void _init_Car(Car* self){
    _init_Engine(&self->engine);
}

inline void _init_Garage(Garage* self){
    _init_Car(&self->car);
}

inline void _init_Author(Author* self){
}

inline void _init_Publisher(Publisher* self){
}

inline void _init_Book(Book* self){
    _init_Author(&self->author);
    _init_Publisher(&self->publisher);
}

inline void _init_Address(Address* self){
}

inline void _init_Person(Person* self){
    _init_Address(&self->home);
}

inline void _init_Company(Company* self){
    _init_Address(&self->hq);
}

inline void _init_Employee(Employee* self){
    _init_Company(&self->works_at);
    _init_Person(&self->lives_at);
}

inline void _init_Node(Node* self){
    self->data = -24;
}

inline void _init_Container(Container* self){
    self->value = -24;
}

inline void _init_Item(Item* self){
    self->id = -24;
}

inline void _init_Inventory(Inventory* self){
    _init_Item(&self->items);
    self->count = -24;
}

/*FUNCTION BODIES*/


void GENERATED_MAIN(){
    __init_global_static_arrays();
    printf("Tests done");
}

int main(int argc, char **argv){
    GENERATED_MAIN();
    return 0;
}
