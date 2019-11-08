#include "Builtins.h"
#include <stdio.h>
#include <stdlib.h>
#include <String.h>
void quackmain();
int main(int argc, char** argv){
quackmain();
char* undefined = "";

printf("------Terminated successfully (woot!)------");

exit(0);
}

struct class_Pt_struct;
typedef struct class_Pt_struct*  class_Pt;
typedef struct obj_Pt_struct { 
class_Pt clazz;
obj_Integer x;
obj_Integer y;
undefined
}* obj_Pt;

struct class_Pt_struct the_class_Pt_struct;
struct class_Pt_struct {
class_Obj  clazz; 
obj_Pt(* constructor)(obj_Integer,obj_Integer);

obj_Integer(* _get_x) ();

obj_Integer(* _get_y) ();

obj_Nothing(* translate) (obj_Integer,obj_Integer);

obj_Pt(* Plus) (obj_Pt);

};
extern class_Pt the_class_Pt;
obj_Pt new_Pt(obj_Integer x,obj_Integer y,undefine) {

obj_Pt new_thing = (obj_Pt)
	malloc(sizeof(struct obj_Pt_struct));
new_thing -> clazz = the_class_Pt;
return new_thing;

}


obj_Pt Pt_method__get_x(undefine){


return (obj_Nothing)undefined;


}


obj_Pt Pt_method__get_y(undefine){


return (obj_Nothing)undefined;


}


obj_Pt Pt_method_translate(obj_Integer dx,obj_Integer dy,undefine){


obj_Integer this_x = this -> x;


obj_Integer this_y = this -> y;



}


obj_Pt Pt_method_Plus(obj_Pt other,undefine){


return (obj_Nothing)undefined;


}


struct class_Pt_struct the_class_Pt_struct = 
{
new_Pt,

Pt_method__get_x,

Pt_method__get_y,

Pt_method_translate,

Pt_method_Plus,

};

class_Pt the_class_Pt =  &the_class_Pt_struct;



void quackmain(){


}
