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
obj_Int temp1 = int_literal(3);
;
obj_Int temp2 = int_literal(2);

obj_Int temp3 = temp2 -> clazz -> PLUS((obj_Int) temp2, temp1)
/*("PLUS","Int")
[("temp2","Int"),("temp1","Int")]
[("temp2","Int"),("temp1","Int")]
*/
;
obj_Int temp4 = int_literal(4);

obj_Int temp5 = temp4 -> clazz -> PLUS((obj_Int) temp4, temp3)
/*("PLUS","Int")
[("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
[("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
*/
;
obj_Int temp6 = int_literal(7);

obj_Int temp7 = temp6 -> clazz -> PLUS((obj_Int) temp6, temp5)
/*("PLUS","Int")
[("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
[("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
*/
;
obj_Int a = temp7;



// to_debug -> localvars: [];
obj_Int temp8 = int_literal(5);

obj_Boolean temp9 = temp8 -> clazz -> LESS((obj_Int) temp8, temp7)
/*("LESS","Int")
[("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
[("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
*/
;
if (temp9 == lit_true) {
goto TLABEL_1;
}
goto FLABEL_1;
TLABEL_1:;
	
obj_Int temp10 = int_literal(11);
;
obj_Int temp11 = int_literal(12);

obj_Int temp12 = temp11 -> clazz -> PLUS((obj_Int) temp11, temp10)
/*("PLUS","Int")
[("temp11","Int"),("temp10","Int"),("temp9","Boolean"),("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
[("temp11","Int"),("temp10","Int"),("temp9","Boolean"),("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
*/
;
obj_Obj temp13 = temp12 -> clazz -> PRINT((obj_Int) temp12)
/*("PRINT","Int")
[("temp12","Int"),("temp11","Int"),("temp10","Int"),("temp9","Boolean"),("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
[("temp12","Int"),("temp11","Int"),("temp10","Int"),("temp9","Boolean"),("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
hello world
*/
;

// to_debug -> localvars: [];
obj_Int temp22 = int_literal(1);

obj_Int temp23 = temp22 -> clazz -> PLUS((obj_Int) temp22, temp9)
/*("PLUS","Int")
[("temp22","Int"),("temp9","Boolean"),("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int"),("temp12","Int"),("temp11","Int"),("temp10","Int"),("temp9","Boolean"),("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
[("temp22","Int"),("temp9","Boolean"),("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int"),("temp12","Int"),("temp11","Int"),("temp10","Int"),("temp9","Boolean"),("temp8","Int"),("temp7","Int"),("temp6","Int"),("temp5","Int"),("temp4","Int"),("temp3","Int"),("temp2","Int"),("temp1","Int")]
*/
;
a = temp23;


;
goto ELABEL_1;
FLABEL_1:;
	;
	;
ELABEL_1:;



// to_debug -> localvars: []
obj_Obj temp15 = a -> clazz -> PRINT((obj_Int) a);

;

}
