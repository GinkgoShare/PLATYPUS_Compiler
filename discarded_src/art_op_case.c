if (op1) { /* binary operation */
	if (op1->code == FPL_T || op2->code == FPL_T) {
		exp_val.attribute.flt_value = 	(tkn->attribute.arr_op == PLUS  ? ((op1->code == FPL_T ? op1->attribute.flt_value : (float)op1->attribute.int_value) 
											+ (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value)) :
										 tkn->attribute.arr_op == MINUS ? ((op1->code == FPL_T ? op1->attribute.flt_value : (float)op1->attribute.int_value) 
											- (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value)) :
										 tkn->attribute.arr_op == MULT  ? ((op1->code == FPL_T ? op1->attribute.flt_value : (float)op1->attribute.int_value) 
											* (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value)) :
																		  ((op1->code == FPL_T ? op1->attribute.flt_value : (float)op1->attribute.int_value) 
											/ (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value)));
	} else {
		exp_val.attribute.int_value = 	(tkn->attribute.arr_op == PLUS  ? (op1->attribute.int_value + op2->attribute.int_value) :
										 tkn->attribute.arr_op == MINUS ? (op1->attribute.int_value - op2->attribute.int_value) :
										 tkn->attribute.arr_op == MULT  ? (op1->attribute.int_value * op2->attribute.int_value) :
																		  (op1->attribute.int_value / op2->attribute.int_value));
	}
} else if (op2) { /* unary operation */
	if (tkn->attribute.arr_op == UMINUS) {
		if (op2->code == FPL_T) exp_val.attribute.flt_value = -op2->attribute.flt_value;
		else exp_val.attribute.int_value = -op2->attribute.int_value;
	} else {
		if (op2->code == FPL_T) exp_val.attribute.flt_value = op2->attribute.flt_value;
		else exp_val.attribute.int_value = op2->attribute.int_value;
	}
}