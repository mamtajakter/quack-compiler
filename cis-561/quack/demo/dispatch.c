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



obj_String temp4 = str_literal("Hello world");
;
obj_String b = temp4;



// to_debug -> localvars: [];
obj_Int c = temp3;



// to_debug -> localvars: [];
obj_String d = temp3;



// to_debug -> localvars: [];
obj_String e = temp3;



// to_debug -> localvars: []
obj_Obj temp57 = e -> clazz -> PRINT((obj_Int) e);

;

obj_Int temp114 = int_literal(3);
;
a = temp114;



}
