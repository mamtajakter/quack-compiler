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
obj_String temp1 = str_literal("hi");
;
obj_String temp2 = str_literal("Hello");

obj_Boolean temp3 = temp2 -> clazz -> EQUALS((obj_String) temp2, temp1)
/*("EQUALS","String")
[("temp2","String"),("temp1","String")]
[("temp2","String"),("temp1","String")]
*/
;

}
