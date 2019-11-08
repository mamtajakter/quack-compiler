#include "Builtins.c"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void quackmain();
int main(int argc, char** argv){
quackmain();
printf("------Terminated successfully (woot!)------");

exit(0);
}

 // Starting code generation session: 
struct class_C1_struct;
typedef struct class_C1_struct*  class_C1;
typedef struct obj_C1_struct { 
class_C1 clazz;



}* obj_C1;

struct class_C1_struct the_class_C1_struct;
struct class_C1_struct {
class_Obj  clazz; 
obj_C1 (* constructor) ();
obj_Obj (* foo) ();

//Just (fromList [],fromList [])
};
extern class_C1 the_class_C1;
obj_C1 new_C1(){

/*
obj_C1 (* constructor) ();
obj_Obj (* foo) ();

()
*/
obj_C1 this = (obj_C1)
	malloc(sizeof(struct obj_C1_struct));
this -> clazz = the_class_C1;

return this;

}


obj_Obj C1_method_foo(obj_C1 this){


obj_C1 temp1 = (obj_C1)new_C1();

return (obj_Obj)temp1;



}


struct class_C1_struct the_class_C1_struct = 
{
&the_class_Obj_struct,
new_C1,

C1_method_foo,

};

class_C1 the_class_C1 =  &the_class_C1_struct;



struct class_C2_struct;
typedef struct class_C2_struct*  class_C2;
typedef struct obj_C2_struct { 
class_C2 clazz;



}* obj_C2;

struct class_C2_struct the_class_C2_struct;
struct class_C2_struct {
class_C1  clazz; 
obj_C2 (* constructor) ();
obj_Obj (* foo) ();

//Just (fromList [],fromList [])
};
extern class_C2 the_class_C2;
obj_C2 new_C2(){

/*
obj_C2 (* constructor) ();
obj_Obj (* foo) ();

()
*/
obj_C2 this = (obj_C2)
	malloc(sizeof(struct obj_C2_struct));
this -> clazz = the_class_C2;

return this;

}


obj_C1 C2_method_foo(obj_C2 this){


obj_C1 temp2 = (obj_C1)new_C1();

return (obj_C1)temp2;



}


struct class_C2_struct the_class_C2_struct = 
{
&the_class_C1_struct,
new_C2,

C2_method_foo,

};

class_C2 the_class_C2 =  &the_class_C2_struct;



struct class_C3_struct;
typedef struct class_C3_struct*  class_C3;
typedef struct obj_C3_struct { 
class_C3 clazz;



}* obj_C3;

struct class_C3_struct the_class_C3_struct;
struct class_C3_struct {
class_C2  clazz; 
obj_C3 (* constructor) ();
obj_C1 (* foo) ();

//Just (fromList [],fromList [])
};
extern class_C3 the_class_C3;
obj_C3 new_C3(){

/*
obj_C3 (* constructor) ();
obj_C1 (* foo) ();

()
*/
obj_C3 this = (obj_C3)
	malloc(sizeof(struct obj_C3_struct));
this -> clazz = the_class_C3;

return this;

}


obj_C2 C3_method_foo(obj_C3 this){


obj_C2 temp4 = (obj_C2)new_C2();

return (obj_C2)temp4;



}


struct class_C3_struct the_class_C3_struct = 
{
&the_class_C2_struct,
new_C3,

C3_method_foo,

};

class_C3 the_class_C3 =  &the_class_C3_struct;



struct class_C4_struct;
typedef struct class_C4_struct*  class_C4;
typedef struct obj_C4_struct { 
class_C4 clazz;



}* obj_C4;

struct class_C4_struct the_class_C4_struct;
struct class_C4_struct {
class_C3  clazz; 
obj_C4 (* constructor) ();
obj_C2 (* foo) ();

//Just (fromList [],fromList [])
};
extern class_C4 the_class_C4;
obj_C4 new_C4(){

/*
obj_C4 (* constructor) ();
obj_C2 (* foo) ();

()
*/
obj_C4 this = (obj_C4)
	malloc(sizeof(struct obj_C4_struct));
this -> clazz = the_class_C4;

return this;

}


obj_C3 C4_method_foo(obj_C4 this){


obj_C3 temp8 = (obj_C3)new_C3();

return (obj_C3)temp8;



}


struct class_C4_struct the_class_C4_struct = 
{
&the_class_C3_struct,
new_C4,

C4_method_foo,

};

class_C4 the_class_C4 =  &the_class_C4_struct;



void quackmain(){

 // Starting code generation session: 
}
