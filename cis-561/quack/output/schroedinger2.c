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
struct class_Schroedinger_struct;
typedef struct class_Schroedinger_struct*  class_Schroedinger;
typedef struct obj_Schroedinger_struct { 
class_Schroedinger clazz;
obj_Int box;
 // Starting code generation session: 
}* obj_Schroedinger;

struct class_Schroedinger_struct the_class_Schroedinger_struct;
struct class_Schroedinger_struct {
class_Obj  clazz; 
obj_Schroedinger(* constructor)(obj_Int);
 // Starting code generation session: 
};
extern class_Schroedinger the_class_Schroedinger;
obj_Schroedinger new_Schroedinger(obj_Int box, // Starting code generation session:) {

obj_Schroedinger new_thing = (obj_Schroedinger)
	malloc(sizeof(struct obj_Schroedinger_struct));
new_thing -> clazz = the_class_Schroedinger;
obj_Int box, // Starting code generation session: 
// to_debug -> varstack: [];
obj_Int temp1 = int_literal(0);

obj_Boolean temp2 = temp1 -> clazz -> EQUALS((obj_Int) temp1)
/*("EQUALS","Int")
[("temp1","Int")]
[("temp1","Int")]
*/
;
if (temp2 == lit_true) {
goto TLABEL_1;
}
goto FLABEL_1;
TLABEL_1:;
	
new_thing -> this = obj_Boolean temp3 = lit_true;
;
;
goto ELABEL_1;
FLABEL_1:;
	;
	obj_Int box, // Starting code generation session: 
new_thing -> this = obj_Boolean temp1 = lit_false;
;
;
ELABEL_1:;



return new_thing;

}

struct class_Schroedinger_struct the_class_Schroedinger_struct = 
{
new_Schroedinger,


};

 // Starting code generation session: 
class_Schroedinger the_class_Schroedinger =  &the_class_Schroedinger_struct;



void quackmain(){

 // Starting code generation session: 
obj_Int temp1 = int_literal(1);
;
obj_Int temp2 = int_literal(2);

obj_Int temp3 = temp2 -> clazz -> PLUS((obj_Int) temp2, temp1)
/*("PLUS","Int")
[("temp2","Int"),("temp1","Int")]
[("temp2","Int"),("temp1","Int")]
*/
;
obj_Int a = temp3;



// to_debug -> localvars: []
obj_Obj temp4 = a -> clazz -> PRINT((obj_Int) a);

;

}
