/*
   Squawked with love, by ducks
*/

#include "Builtins.h"
#include <stdlib.h>

typedef struct class_Printer_struct *class_Printer;
typedef struct obj_Printer_struct *obj_Printer;

typedef struct class_Node_struct *class_Node;
typedef struct obj_Node_struct *obj_Node;

struct class_Printer_struct {
	class_Obj	super;
	obj_Boolean	(*method_EQUALS) (obj_Obj, obj_Obj);
	obj_Nothing	(*method_PRINT) (obj_Obj);
	obj_String	(*method_STR) (obj_Obj);
	obj_Nothing	(*method_feed) (obj_Printer);
	obj_Nothing	(*method_println) (obj_Printer, obj_Obj);
};
struct obj_Printer_struct {
	class_Printer	class;
};

struct class_Node_struct {
	class_Obj	super;
	obj_Boolean	(*method_EQUALS) (obj_Obj, obj_Obj);
	obj_Nothing	(*method_PRINT) (obj_Obj);
	obj_String	(*method_STR) (obj_Node);
	obj_Nothing	(*method_bubbleSort) (obj_Node);
	obj_Int	(*method_head) (obj_Node);
	obj_Int	(*method_len) (obj_Node);
	obj_Nothing	(*method_setCar) (obj_Node, obj_Int);
	obj_Nothing	(*method_swap) (obj_Node, obj_Node, obj_Node);
	obj_Node	(*method_tail) (obj_Node);
};
struct obj_Node_struct {
	class_Node	class;
	obj_Int	field_car;
	obj_Obj	field_cdr;
};

// Forward-declare methods
obj_Printer new_Printer();
obj_Nothing Printer_method_println(obj_Printer this, obj_Obj local_obj);
obj_Nothing Printer_method_feed(obj_Printer this);

obj_Node new_Node(obj_Int local_car, obj_Obj local_cdr);
obj_String Node_method_STR(obj_Node this);
obj_Nothing Node_method_setCar(obj_Node this, obj_Int local_i);
obj_Int Node_method_head(obj_Node this);
obj_Node Node_method_tail(obj_Node this);
obj_Int Node_method_len(obj_Node this);
obj_Nothing Node_method_bubbleSort(obj_Node this);
obj_Nothing Node_method_swap(obj_Node this, obj_Node local_a, obj_Node local_b);


struct class_Node_struct the_class_Node_struct = {
	.super = &the_class_Obj_struct,
	.method_EQUALS = Obj_method_EQUALS,
	.method_PRINT = Obj_method_PRINT,
	.method_STR = Node_method_STR,
	.method_bubbleSort = Node_method_bubbleSort,
	.method_head = Node_method_head,
	.method_len = Node_method_len,
	.method_setCar = Node_method_setCar,
	.method_swap = Node_method_swap,
	.method_tail = Node_method_tail
};
const class_Node the_class_Node = &the_class_Node_struct;
struct class_Printer_struct the_class_Printer_struct = {
	.super = &the_class_Obj_struct,
	.method_EQUALS = Obj_method_EQUALS,
	.method_PRINT = Obj_method_PRINT,
	.method_STR = Obj_method_STR,
	.method_feed = Printer_method_feed,
	.method_println = Printer_method_println
};
const class_Printer the_class_Printer = &the_class_Printer_struct;
// Printer Methods
obj_Printer new_Printer() {
	obj_Printer this = (obj_Printer)malloc(sizeof(*this));
	this->class = the_class_Printer;

	return this;
}
obj_Nothing Printer_method_println(obj_Printer this, obj_Obj local_obj) {

	obj_Nothing temp0 = local_obj->class->method_PRINT((obj_Obj)local_obj);
	obj_Nothing temp1 = this->class->method_feed((obj_Printer)this);
	return lit_none;
}
obj_Nothing Printer_method_feed(obj_Printer this) {

	obj_String temp2 = str_literal("\n");
	obj_Nothing temp3 = temp2->class->method_PRINT((obj_Obj)temp2);
	return lit_none;
}

// Node Methods
obj_Node new_Node(obj_Int local_car, obj_Obj local_cdr) {
	obj_Node this = (obj_Node)malloc(sizeof(*this));
	this->class = the_class_Node;

	this->field_car = (obj_Int)local_car;
	this->field_cdr = (obj_Obj)local_cdr;
	return this;
}
obj_String Node_method_STR(obj_Node this) {
	obj_Node local_ll_n;
	obj_Obj local_obj;

	class_Obj temp4 = (class_Obj)this->field_cdr->class;
	while (temp4) {
		if (temp4 == (class_Obj)the_class_Node) {
			local_ll_n = (obj_Node)this->field_cdr;
			obj_String temp5 = this->field_car->class->method_STR((obj_Int)this->field_car);
			obj_String temp6 = str_literal(", ");
			obj_String temp7 = temp5->class->method_PLUS((obj_String)temp5, (obj_String)temp6);
			obj_String temp8 = this->field_cdr->class->method_STR((obj_Obj)this->field_cdr);
			obj_String temp9 = temp7->class->method_PLUS((obj_String)temp7, (obj_String)temp8);
			return (obj_String)temp9;
			break;
		} else if (temp4 == (class_Obj)the_class_Obj) {
			local_obj = (obj_Obj)this->field_cdr;
			obj_String temp10 = this->field_car->class->method_STR((obj_Int)this->field_car);
			return (obj_String)temp10;
			break;
		} else {
			temp4 = temp4->super;
		}
	}
	obj_String temp11 = this->field_car->class->method_STR((obj_Int)this->field_car);
	obj_String temp12 = str_literal(",");
	obj_String temp13 = temp11->class->method_PLUS((obj_String)temp11, (obj_String)temp12);
	obj_String temp14 = this->field_cdr->class->method_STR((obj_Obj)this->field_cdr);
	obj_String temp15 = temp13->class->method_PLUS((obj_String)temp13, (obj_String)temp14);
	return (obj_String)temp15;
}
obj_Nothing Node_method_setCar(obj_Node this, obj_Int local_i) {

	this->field_car = (obj_Int)local_i;
	return lit_none;
}
obj_Int Node_method_head(obj_Node this) {

	return (obj_Int)this->field_car;
}
obj_Node Node_method_tail(obj_Node this) {
	obj_Node local_ll_n;
	obj_Obj local_obj;

	class_Obj temp16 = (class_Obj)this->field_cdr->class;
	while (temp16) {
		if (temp16 == (class_Obj)the_class_Node) {
			local_ll_n = (obj_Node)this->field_cdr;
			return (obj_Node)local_ll_n;
			break;
		} else if (temp16 == (class_Obj)the_class_Obj) {
			local_obj = (obj_Obj)this->field_cdr;
			return (obj_Node)this;
			break;
		} else {
			temp16 = temp16->super;
		}
	}
	return (obj_Node)this;
}
obj_Int Node_method_len(obj_Node this) {
	obj_Int local_l;
	obj_Node local_temp;

	obj_Int temp17 = int_literal(1);
	local_l = (obj_Int)temp17;
	local_temp = (obj_Node)this;
	goto LABEL0_TEST;
LABEL1_AGAIN:;
	obj_Int temp18 = int_literal(1);
	obj_Int temp19 = local_l->class->method_PLUS((obj_Int)local_l, (obj_Int)temp18);
	local_l = (obj_Int)temp19;
	obj_Node temp20 = local_temp->class->method_tail((obj_Node)local_temp);
	local_temp = (obj_Node)temp20;
LABEL0_TEST:;
	obj_Node temp21 = local_temp->class->method_tail((obj_Node)local_temp);
	obj_Boolean temp22 = temp21->class->method_EQUALS((obj_Obj)temp21, (obj_Obj)local_temp);
	if (temp22 == lit_true) {
		goto LABEL2_DONE;
	}
	goto LABEL1_AGAIN;
LABEL2_DONE:;
	return (obj_Int)local_l;
}
obj_Nothing Node_method_bubbleSort(obj_Node this) {
	obj_Boolean local_decision;
	obj_Int local_i;
	obj_Node local_ll_n_1;
	obj_Node local_ll_n_2;
	obj_Node local_ll_n_3;
	obj_Node local_node;

	local_node = (obj_Node)this;
	obj_Int temp23 = this->class->method_len((obj_Node)this);
	obj_Int temp24 = int_literal(1);
	obj_Boolean temp25 = temp23->class->method_EQUALS((obj_Int)temp23, (obj_Obj)temp24);
	if (temp25 == lit_true) {
		goto LABEL3_TRUE;
	}
	goto LABEL4_FALSE;
LABEL3_TRUE:;
	return (obj_Nothing)lit_none;
	goto LABEL5_DONE;
LABEL4_FALSE:;
LABEL5_DONE:;
	obj_Int temp26 = this->class->method_len((obj_Node)this);
	local_i = (obj_Int)temp26;
	goto LABEL6_TEST;
LABEL7_AGAIN:;
	goto LABEL8_TEST;
LABEL9_AGAIN:;
	local_decision = (obj_Boolean)lit_false;
	obj_Obj temp27 = local_node->field_cdr;
	class_Obj temp28 = (class_Obj)temp27->class;
	while (temp28) {
		if (temp28 == (class_Obj)the_class_Node) {
			local_ll_n_1 = (obj_Node)temp27;
			obj_Int temp29 = local_node->field_car;
			obj_Int temp30 = local_ll_n_1->field_car;
			obj_Boolean temp31 = temp29->class->method_MORE((obj_Int)temp29, (obj_Int)temp30);
			if (temp31 == lit_true) {
				goto LABEL10_TRUE;
			}
			goto LABEL11_FALSE;
		LABEL10_TRUE:;
			local_decision = (obj_Boolean)lit_true;
			goto LABEL12_DONE;
		LABEL11_FALSE:;
		LABEL12_DONE:;
			break;
		} else {
			temp28 = temp28->super;
		}
	}
	if (local_decision == lit_true) {
		goto LABEL13_TRUE;
	}
	goto LABEL14_FALSE;
LABEL13_TRUE:;
	obj_Obj temp32 = local_node->field_cdr;
	class_Obj temp33 = (class_Obj)temp32->class;
	while (temp33) {
		if (temp33 == (class_Obj)the_class_Node) {
			local_ll_n_2 = (obj_Node)temp32;
			obj_Nothing temp34 = this->class->method_swap((obj_Node)this, (obj_Node)local_node, (obj_Node)local_ll_n_2);
			break;
		} else {
			temp33 = temp33->super;
		}
	}
	goto LABEL15_DONE;
LABEL14_FALSE:;
LABEL15_DONE:;
	obj_Obj temp35 = local_node->field_cdr;
	class_Obj temp36 = (class_Obj)temp35->class;
	while (temp36) {
		if (temp36 == (class_Obj)the_class_Node) {
			local_ll_n_3 = (obj_Node)temp35;
			local_node = (obj_Node)local_ll_n_3;
			break;
		} else {
			temp36 = temp36->super;
		}
	}
LABEL8_TEST:;
	obj_Int temp37 = local_node->class->method_len((obj_Node)local_node);
	obj_Int temp38 = int_literal(1);
	obj_Boolean temp39 = temp37->class->method_EQUALS((obj_Int)temp37, (obj_Obj)temp38);
	if (temp39 == lit_true) {
		goto LABEL16_DONE;
	}
	goto LABEL9_AGAIN;
LABEL16_DONE:;
	local_node = (obj_Node)this;
	obj_Int temp40 = int_literal(1);
	obj_Int temp41 = local_i->class->method_MINUS((obj_Int)local_i, (obj_Int)temp40);
	local_i = (obj_Int)temp41;
LABEL6_TEST:;
	obj_Int temp42 = int_literal(0);
	obj_Boolean temp43 = local_i->class->method_MORE((obj_Int)local_i, (obj_Int)temp42);
	if (temp43 == lit_true) {
		goto LABEL7_AGAIN;
	}
	goto LABEL17_DONE;
LABEL17_DONE:;
	return (obj_Nothing)lit_none;
}
obj_Nothing Node_method_swap(obj_Node this, obj_Node local_a, obj_Node local_b) {
	obj_Int local_temp;

	obj_Int temp44 = local_a->field_car;
	local_temp = (obj_Int)temp44;
	obj_Int temp45 = local_b->field_car;
	obj_Nothing temp46 = local_a->class->method_setCar((obj_Node)local_a, (obj_Int)temp45);
	obj_Nothing temp47 = local_b->class->method_setCar((obj_Node)local_b, (obj_Int)local_temp);
	return lit_none;
}

int main() {
	obj_Node local_link;
	obj_Printer local_p;

	obj_Int temp49 = int_literal(6342);
	obj_Int temp51 = int_literal(847);
	obj_Int temp53 = int_literal(9829);
	obj_Int temp55 = int_literal(8802);
	obj_Int temp57 = int_literal(1011);
	obj_Int temp59 = int_literal(3184);
	obj_Int temp61 = int_literal(3500);
	obj_Int temp63 = int_literal(7133);
	obj_Int temp65 = int_literal(942);
	obj_Int temp67 = int_literal(2875);
	obj_Int temp69 = int_literal(2955);
	obj_Int temp71 = int_literal(5565);
	obj_Int temp73 = int_literal(2610);
	obj_Int temp75 = int_literal(5469);
	obj_Int temp77 = int_literal(4975);
	obj_Int temp79 = int_literal(209);
	obj_Int temp81 = int_literal(2229);
	obj_Int temp83 = int_literal(75);
	obj_Int temp85 = int_literal(2863);
	obj_Int temp87 = int_literal(6832);
	obj_Int temp89 = int_literal(8012);
	obj_Int temp91 = int_literal(8817);
	obj_Int temp93 = int_literal(6452);
	obj_Int temp95 = int_literal(9327);
	obj_Int temp97 = int_literal(3724);
	obj_Int temp99 = int_literal(7101);
	obj_Int temp101 = int_literal(11);
	obj_Int temp103 = int_literal(1179);
	obj_Int temp105 = int_literal(8066);
	obj_Int temp107 = int_literal(1616);
	obj_Int temp109 = int_literal(7580);
	obj_Int temp111 = int_literal(835);
	obj_Int temp113 = int_literal(3827);
	obj_Int temp115 = int_literal(5657);
	obj_Int temp117 = int_literal(8696);
	obj_Int temp119 = int_literal(6843);
	obj_Int temp121 = int_literal(2987);
	obj_Int temp123 = int_literal(9889);
	obj_Int temp125 = int_literal(2865);
	obj_Int temp127 = int_literal(3872);
	obj_Int temp129 = int_literal(6706);
	obj_Int temp131 = int_literal(6717);
	obj_Int temp133 = int_literal(3860);
	obj_Int temp135 = int_literal(2575);
	obj_Int temp137 = int_literal(6510);
	obj_Int temp139 = int_literal(3114);
	obj_Int temp141 = int_literal(5580);
	obj_Int temp143 = int_literal(8835);
	obj_Int temp145 = int_literal(126);
	obj_Int temp147 = int_literal(7784);
	obj_Int temp149 = int_literal(6888);
	obj_Int temp151 = int_literal(4040);
	obj_Int temp153 = int_literal(5501);
	obj_Int temp155 = int_literal(7210);
	obj_Int temp157 = int_literal(5168);
	obj_Int temp159 = int_literal(5746);
	obj_Int temp161 = int_literal(2861);
	obj_Int temp163 = int_literal(2196);
	obj_Int temp165 = int_literal(5481);
	obj_Int temp167 = int_literal(9329);
	obj_Int temp169 = int_literal(9181);
	obj_Int temp171 = int_literal(4139);
	obj_Int temp173 = int_literal(589);
	obj_Int temp175 = int_literal(6107);
	obj_Int temp177 = int_literal(4572);
	obj_Int temp179 = int_literal(8371);
	obj_Int temp181 = int_literal(3595);
	obj_Int temp183 = int_literal(4448);
	obj_Int temp185 = int_literal(6740);
	obj_Int temp187 = int_literal(8223);
	obj_Int temp189 = int_literal(7431);
	obj_Int temp191 = int_literal(6382);
	obj_Int temp193 = int_literal(9652);
	obj_Int temp195 = int_literal(7585);
	obj_Int temp197 = int_literal(3086);
	obj_Int temp199 = int_literal(5664);
	obj_Int temp201 = int_literal(229);
	obj_Int temp203 = int_literal(9548);
	obj_Int temp205 = int_literal(66);
	obj_Int temp207 = int_literal(9070);
	obj_Int temp209 = int_literal(5445);
	obj_Int temp211 = int_literal(4844);
	obj_Int temp213 = int_literal(817);
	obj_Int temp215 = int_literal(2612);
	obj_Int temp217 = int_literal(4166);
	obj_Int temp219 = int_literal(2960);
	obj_Int temp221 = int_literal(2964);
	obj_Int temp223 = int_literal(333);
	obj_Int temp225 = int_literal(6584);
	obj_Int temp227 = int_literal(801);
	obj_Int temp229 = int_literal(6968);
	obj_Int temp231 = int_literal(2571);
	obj_Int temp233 = int_literal(2628);
	obj_Int temp235 = int_literal(8922);
	obj_Int temp237 = int_literal(7808);
	obj_Int temp239 = int_literal(3538);
	obj_Int temp241 = int_literal(2998);
	obj_Int temp243 = int_literal(7088);
	obj_Int temp245 = int_literal(8573);
	obj_Int temp247 = int_literal(5535);
	obj_Obj temp248 = new_Obj();
	obj_Node temp246 = new_Node((obj_Int)temp247, (obj_Obj)temp248);
	obj_Node temp244 = new_Node((obj_Int)temp245, (obj_Obj)temp246);
	obj_Node temp242 = new_Node((obj_Int)temp243, (obj_Obj)temp244);
	obj_Node temp240 = new_Node((obj_Int)temp241, (obj_Obj)temp242);
	obj_Node temp238 = new_Node((obj_Int)temp239, (obj_Obj)temp240);
	obj_Node temp236 = new_Node((obj_Int)temp237, (obj_Obj)temp238);
	obj_Node temp234 = new_Node((obj_Int)temp235, (obj_Obj)temp236);
	obj_Node temp232 = new_Node((obj_Int)temp233, (obj_Obj)temp234);
	obj_Node temp230 = new_Node((obj_Int)temp231, (obj_Obj)temp232);
	obj_Node temp228 = new_Node((obj_Int)temp229, (obj_Obj)temp230);
	obj_Node temp226 = new_Node((obj_Int)temp227, (obj_Obj)temp228);
	obj_Node temp224 = new_Node((obj_Int)temp225, (obj_Obj)temp226);
	obj_Node temp222 = new_Node((obj_Int)temp223, (obj_Obj)temp224);
	obj_Node temp220 = new_Node((obj_Int)temp221, (obj_Obj)temp222);
	obj_Node temp218 = new_Node((obj_Int)temp219, (obj_Obj)temp220);
	obj_Node temp216 = new_Node((obj_Int)temp217, (obj_Obj)temp218);
	obj_Node temp214 = new_Node((obj_Int)temp215, (obj_Obj)temp216);
	obj_Node temp212 = new_Node((obj_Int)temp213, (obj_Obj)temp214);
	obj_Node temp210 = new_Node((obj_Int)temp211, (obj_Obj)temp212);
	obj_Node temp208 = new_Node((obj_Int)temp209, (obj_Obj)temp210);
	obj_Node temp206 = new_Node((obj_Int)temp207, (obj_Obj)temp208);
	obj_Node temp204 = new_Node((obj_Int)temp205, (obj_Obj)temp206);
	obj_Node temp202 = new_Node((obj_Int)temp203, (obj_Obj)temp204);
	obj_Node temp200 = new_Node((obj_Int)temp201, (obj_Obj)temp202);
	obj_Node temp198 = new_Node((obj_Int)temp199, (obj_Obj)temp200);
	obj_Node temp196 = new_Node((obj_Int)temp197, (obj_Obj)temp198);
	obj_Node temp194 = new_Node((obj_Int)temp195, (obj_Obj)temp196);
	obj_Node temp192 = new_Node((obj_Int)temp193, (obj_Obj)temp194);
	obj_Node temp190 = new_Node((obj_Int)temp191, (obj_Obj)temp192);
	obj_Node temp188 = new_Node((obj_Int)temp189, (obj_Obj)temp190);
	obj_Node temp186 = new_Node((obj_Int)temp187, (obj_Obj)temp188);
	obj_Node temp184 = new_Node((obj_Int)temp185, (obj_Obj)temp186);
	obj_Node temp182 = new_Node((obj_Int)temp183, (obj_Obj)temp184);
	obj_Node temp180 = new_Node((obj_Int)temp181, (obj_Obj)temp182);
	obj_Node temp178 = new_Node((obj_Int)temp179, (obj_Obj)temp180);
	obj_Node temp176 = new_Node((obj_Int)temp177, (obj_Obj)temp178);
	obj_Node temp174 = new_Node((obj_Int)temp175, (obj_Obj)temp176);
	obj_Node temp172 = new_Node((obj_Int)temp173, (obj_Obj)temp174);
	obj_Node temp170 = new_Node((obj_Int)temp171, (obj_Obj)temp172);
	obj_Node temp168 = new_Node((obj_Int)temp169, (obj_Obj)temp170);
	obj_Node temp166 = new_Node((obj_Int)temp167, (obj_Obj)temp168);
	obj_Node temp164 = new_Node((obj_Int)temp165, (obj_Obj)temp166);
	obj_Node temp162 = new_Node((obj_Int)temp163, (obj_Obj)temp164);
	obj_Node temp160 = new_Node((obj_Int)temp161, (obj_Obj)temp162);
	obj_Node temp158 = new_Node((obj_Int)temp159, (obj_Obj)temp160);
	obj_Node temp156 = new_Node((obj_Int)temp157, (obj_Obj)temp158);
	obj_Node temp154 = new_Node((obj_Int)temp155, (obj_Obj)temp156);
	obj_Node temp152 = new_Node((obj_Int)temp153, (obj_Obj)temp154);
	obj_Node temp150 = new_Node((obj_Int)temp151, (obj_Obj)temp152);
	obj_Node temp148 = new_Node((obj_Int)temp149, (obj_Obj)temp150);
	obj_Node temp146 = new_Node((obj_Int)temp147, (obj_Obj)temp148);
	obj_Node temp144 = new_Node((obj_Int)temp145, (obj_Obj)temp146);
	obj_Node temp142 = new_Node((obj_Int)temp143, (obj_Obj)temp144);
	obj_Node temp140 = new_Node((obj_Int)temp141, (obj_Obj)temp142);
	obj_Node temp138 = new_Node((obj_Int)temp139, (obj_Obj)temp140);
	obj_Node temp136 = new_Node((obj_Int)temp137, (obj_Obj)temp138);
	obj_Node temp134 = new_Node((obj_Int)temp135, (obj_Obj)temp136);
	obj_Node temp132 = new_Node((obj_Int)temp133, (obj_Obj)temp134);
	obj_Node temp130 = new_Node((obj_Int)temp131, (obj_Obj)temp132);
	obj_Node temp128 = new_Node((obj_Int)temp129, (obj_Obj)temp130);
	obj_Node temp126 = new_Node((obj_Int)temp127, (obj_Obj)temp128);
	obj_Node temp124 = new_Node((obj_Int)temp125, (obj_Obj)temp126);
	obj_Node temp122 = new_Node((obj_Int)temp123, (obj_Obj)temp124);
	obj_Node temp120 = new_Node((obj_Int)temp121, (obj_Obj)temp122);
	obj_Node temp118 = new_Node((obj_Int)temp119, (obj_Obj)temp120);
	obj_Node temp116 = new_Node((obj_Int)temp117, (obj_Obj)temp118);
	obj_Node temp114 = new_Node((obj_Int)temp115, (obj_Obj)temp116);
	obj_Node temp112 = new_Node((obj_Int)temp113, (obj_Obj)temp114);
	obj_Node temp110 = new_Node((obj_Int)temp111, (obj_Obj)temp112);
	obj_Node temp108 = new_Node((obj_Int)temp109, (obj_Obj)temp110);
	obj_Node temp106 = new_Node((obj_Int)temp107, (obj_Obj)temp108);
	obj_Node temp104 = new_Node((obj_Int)temp105, (obj_Obj)temp106);
	obj_Node temp102 = new_Node((obj_Int)temp103, (obj_Obj)temp104);
	obj_Node temp100 = new_Node((obj_Int)temp101, (obj_Obj)temp102);
	obj_Node temp98 = new_Node((obj_Int)temp99, (obj_Obj)temp100);
	obj_Node temp96 = new_Node((obj_Int)temp97, (obj_Obj)temp98);
	obj_Node temp94 = new_Node((obj_Int)temp95, (obj_Obj)temp96);
	obj_Node temp92 = new_Node((obj_Int)temp93, (obj_Obj)temp94);
	obj_Node temp90 = new_Node((obj_Int)temp91, (obj_Obj)temp92);
	obj_Node temp88 = new_Node((obj_Int)temp89, (obj_Obj)temp90);
	obj_Node temp86 = new_Node((obj_Int)temp87, (obj_Obj)temp88);
	obj_Node temp84 = new_Node((obj_Int)temp85, (obj_Obj)temp86);
	obj_Node temp82 = new_Node((obj_Int)temp83, (obj_Obj)temp84);
	obj_Node temp80 = new_Node((obj_Int)temp81, (obj_Obj)temp82);
	obj_Node temp78 = new_Node((obj_Int)temp79, (obj_Obj)temp80);
	obj_Node temp76 = new_Node((obj_Int)temp77, (obj_Obj)temp78);
	obj_Node temp74 = new_Node((obj_Int)temp75, (obj_Obj)temp76);
	obj_Node temp72 = new_Node((obj_Int)temp73, (obj_Obj)temp74);
	obj_Node temp70 = new_Node((obj_Int)temp71, (obj_Obj)temp72);
	obj_Node temp68 = new_Node((obj_Int)temp69, (obj_Obj)temp70);
	obj_Node temp66 = new_Node((obj_Int)temp67, (obj_Obj)temp68);
	obj_Node temp64 = new_Node((obj_Int)temp65, (obj_Obj)temp66);
	obj_Node temp62 = new_Node((obj_Int)temp63, (obj_Obj)temp64);
	obj_Node temp60 = new_Node((obj_Int)temp61, (obj_Obj)temp62);
	obj_Node temp58 = new_Node((obj_Int)temp59, (obj_Obj)temp60);
	obj_Node temp56 = new_Node((obj_Int)temp57, (obj_Obj)temp58);
	obj_Node temp54 = new_Node((obj_Int)temp55, (obj_Obj)temp56);
	obj_Node temp52 = new_Node((obj_Int)temp53, (obj_Obj)temp54);
	obj_Node temp50 = new_Node((obj_Int)temp51, (obj_Obj)temp52);
	obj_Node temp48 = new_Node((obj_Int)temp49, (obj_Obj)temp50);
	local_link = (obj_Node)temp48;
	obj_Printer temp249 = new_Printer();
	local_p = (obj_Printer)temp249;
	obj_Nothing temp250 = local_p->class->method_feed((obj_Printer)local_p);
	obj_Nothing temp251 = local_p->class->method_println((obj_Printer)local_p, (obj_Obj)local_link);
	obj_Nothing temp252 = local_p->class->method_feed((obj_Printer)local_p);
	obj_String temp253 = str_literal("Sorting...");
	obj_Nothing temp254 = local_p->class->method_println((obj_Printer)local_p, (obj_Obj)temp253);
	obj_Nothing temp255 = local_link->class->method_bubbleSort((obj_Node)local_link);
	obj_Nothing temp256 = local_p->class->method_feed((obj_Printer)local_p);
	obj_Nothing temp257 = local_p->class->method_println((obj_Printer)local_p, (obj_Obj)local_link);
	obj_Nothing temp258 = local_p->class->method_feed((obj_Printer)local_p);
}
