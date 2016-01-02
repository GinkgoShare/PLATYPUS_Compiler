switch (op1->code) {
	case AVID_T:
		if (st_get_type(sym_table, op1->attribute.vid_offset) == 'F') {
			if (op2->code == AVID_T) {
				exp_val.attribute.int_value = sym_table.pstvr[op1->attribute.vid_offset].i_value.fpl_val
											== (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
												(float)sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val);
			} else
				exp_val.attribute.int_value = sym_table.pstvr[op1->attribute.vid_offset].i_value.fpl_val
											== (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
		} else {
			if (op2->code == AVID_T) {
				exp_val.attribute.int_value = sym_table.pstvr[op1->attribute.vid_offset].i_value.int_val
											== (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? (int)sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
												sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val);
			} else
				exp_val.attribute.int_value = sym_table.pstvr[op1->attribute.vid_offset].i_value.int_val
											== (op2->code == FPL_T ? (int)op2->attribute.flt_value : op2->attribute.int_value);
		}
		break;
	case FPL_T:
		if (op2->code == AVID_T) {
				exp_val.attribute.int_value = op1->attribute.flt_value
											== (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
												(float)sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val);
		} else
			exp_val.attribute.int_value = op1->attribute.flt_value
										== (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
		break;
	case INL_T:
		if (op2->code == AVID_T) {
			exp_val.attribute.int_value = op1->attribute.int_value
										== (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? (int)sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
											sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val);
		} else
			exp_val.attribute.int_value = op1->attribute.int_value
										== (op2->code == FPL_T ? (int)op2->attribute.flt_value : op2->attribute.int_value);
		break;
	case SVID_T: case STR_T:
		exp_val.attribute.int_value = (op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : op1->attribute.str_offset)
									== (op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : 
										op2->attribute.str_offset;
		break;
	}
}


switch (op1->code) {
	case AVID_T: case FPL_T: case INL_T:
		if (op1->code == FPL_T || op1->code == AVID_T && st_get_type(sym_table, op1->attribute.vid_offset) == 'F') {
			float flp_val = op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.fpl_val : op1->attribute.flt_value;
			if (op2->code == AVID_T) {
				exp_val.attribute.int_value = flp_val == (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
														  sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
														 (float)sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val);
			} else
				exp_val.attribute.int_value = flp_val == (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
		} else {
			int int_val = op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.int_val : op1->attribute.int_value;
			if (op2->code == AVID_T) {
				exp_val.attribute.int_value = int_val == (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? 
														 (int)sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
														  sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val);
			} else
				exp_val.attribute.int_value = int_val == (op2->code == FPL_T ? (int)op2->attribute.flt_value : op2->attribute.int_value);
		}
		break;
	case SVID_T: case STR_T:
		exp_val.attribute.int_value = op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : op1->attribute.str_offset
									== op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : op2->attribute.str_offset;
		break;
}



exp_val.attribute.int_value  = (op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : 
	(op1->code == STR_T ? op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
							== (op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : (op2->code == STR_T ? 
									op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
break;

case REL_OP_T:
#ifdef DEBUG
printf("In REL_OP_T\n");
#endif
	exp_val.code = INL_T;
	switch (tkn->attribute.rel_op) {
		case EQ:
			exp_val.attribute.int_value  = (op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : 
				(op1->code == STR_T ? op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
										== (op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : (op2->code == STR_T ? 
												op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
			break;
		case NE:
			exp_val.attribute.int_value  = (op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : 
				(op1->code == STR_T ? op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
										!= (op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : (op2->code == STR_T ? 
												op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
			break;
		case GT:
			exp_val.attribute.int_value  = (op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : 
				(op1->code == STR_T ? op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
										 > (op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : (op2->code == STR_T ? 
												op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
			break;
		case LT:
			exp_val.attribute.int_value  = (op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : 
				(op1->code == STR_T ? op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
										 < (op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : (op2->code == STR_T ? 
												op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
			break;
	}
	break;

exp_val.attribute.int_value  = (op1->code == AVID_T ? 
									(st_get_type(sym_table, op1->attribute.vid_offset) == 'F' ? 
										sym_table.pstvr[op1->attribute.vid_offset].i_value.flt_value : sym_table.pstvr[op1->attribute.vid_offset].i_value.int_value) :
									op1->code == FPL_T ? op1->attribute.flt_value : 
									op1->code == INL_T ? op1->attribute.int_value :
									op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset :
										op1->code == STR_T ? op1->attribute.str_offset)
								== (op2->code == AVID_T ? 
									(st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? 
										sym_table.pstvr[op2->attribute.vid_offset].i_value.flt_value : sym_table.pstvr[op2->attribute.vid_offset].i_value.int_value) :
									op2->code == FPL_T ? op2->attribute.flt_value : 
									op2->code == INL_T ? op2->attribute.int_value :
									op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset :
										op2->code == STR_T ? op2->attribute.str_offset);

exp_val.attribute.int_value  = (op1->code == AVID_T ? 
									(st_get_type(sym_table, op1->attribute.vid_offset) == 'F' ? 
										sym_table.pstvr[op1->attribute.vid_offset].i_value.flt_value : sym_table.pstvr[op1->attribute.vid_offset].i_value.int_value) :
									op1->code == FPL_T ? op1->attribute.flt_value : 
									op1->code == INL_T ? op1->attribute.int_value :
									op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset :
										op1->code == STR_T ? op1->attribute.str_offset)
								!= (op2->code == AVID_T ? 
									(st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? 
										sym_table.pstvr[op2->attribute.vid_offset].i_value.flt_value : sym_table.pstvr[op2->attribute.vid_offset].i_value.int_value) :
									op2->code == FPL_T ? op2->attribute.flt_value : 
									op2->code == INL_T ? op2->attribute.int_value :
									op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset :
										op2->code == STR_T ? op2->attribute.str_offset);

exp_val.attribute.int_value  = (op1->code == AVID_T ? 
									(st_get_type(sym_table, op1->attribute.vid_offset) == 'F' ? 
										sym_table.pstvr[op1->attribute.vid_offset].i_value.flt_value : sym_table.pstvr[op1->attribute.vid_offset].i_value.int_value) :
									op1->code == FPL_T ? op1->attribute.flt_value : 
									op1->code == INL_T ? op1->attribute.int_value :
									op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset :
										op1->code == STR_T ? op1->attribute.str_offset)
								>  (op2->code == AVID_T ? 
									(st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? 
										sym_table.pstvr[op2->attribute.vid_offset].i_value.flt_value : sym_table.pstvr[op2->attribute.vid_offset].i_value.int_value) :
									op2->code == FPL_T ? op2->attribute.flt_value : 
									op2->code == INL_T ? op2->attribute.int_value :
									op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset :
										op2->code == STR_T ? op2->attribute.str_offset);

exp_val.attribute.int_value  = (op1->code == AVID_T ? 
									(st_get_type(sym_table, op1->attribute.vid_offset) == 'F' ? 
										sym_table.pstvr[op1->attribute.vid_offset].i_value.flt_value : sym_table.pstvr[op1->attribute.vid_offset].i_value.int_value) :
									op1->code == FPL_T ? op1->attribute.flt_value : 
									op1->code == INL_T ? op1->attribute.int_value :
									op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset :
										op1->code == STR_T ? op1->attribute.str_offset)
								<  (op2->code == AVID_T ? 
									(st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ? 
										sym_table.pstvr[op2->attribute.vid_offset].i_value.flt_value : sym_table.pstvr[op2->attribute.vid_offset].i_value.int_value) :
									op2->code == FPL_T ? op2->attribute.flt_value : 
									op2->code == INL_T ? op2->attribute.int_value :
									op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset :
										op2->code == STR_T ? op2->attribute.str_offset);



if (op1->code == AVID_T && st_get_type(sym_table, op1->attribute.vid_offset) == 'F' || op1->code == FPL_T) {
	float flp_val = op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.fpl_val : op1->attribute.flt_value;
	switch (tkn->attribute.rel_op) {
		case EQ:
			exp_val.attribute.int_value = flp_val == (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
													  sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
													  st_get_type(sym_table, op2->attribute.vid_offset) == 'I' ?
													 (float)sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val
													  op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
			break;
		case NE:
			exp_val.attribute.int_value = flp_val != (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
													  sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
													  st_get_type(sym_table, op2->attribute.vid_offset) == 'I' ?
													 (float)sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val
													  op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
			break;
		case GT:
			exp_val.attribute.int_value = flp_val >  (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
													  sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
													  st_get_type(sym_table, op2->attribute.vid_offset) == 'I' ?
													 (float)sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val
													  op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
		case LT:
			exp_val.attribute.int_value = flp_val <  (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
													  sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
													  st_get_type(sym_table, op2->attribute.vid_offset) == 'I' ?
													 (float)sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val
													  op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
			break;
	}
} else if (op1->code == AVID_T || op1->code == FPL_T) {
	int int_val = op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.int_val : op1->attribute.int_value;
	switch (tkn->attribute.rel_op) {
		case EQ:
			exp_val.attribute.int_value = int_val == (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
													 (int)sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
													  st_get_type(sym_table, op2->attribute.vid_offset) == 'I' ?
													  sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val
													  op2->code == FPL_T ? (int)op2->attribute.flt_value : op2->attribute.int_value);
			break;
		case NE:
			exp_val.attribute.int_value = int_val != (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
													 (int)sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
													  st_get_type(sym_table, op2->attribute.vid_offset) == 'I' ?
													  sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val
													  op2->code == FPL_T ? (int)op2->attribute.flt_value : op2->attribute.int_value);
			break;
		case GT:
			exp_val.attribute.int_value = int_val >  (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
													 (int)sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
													  st_get_type(sym_table, op2->attribute.vid_offset) == 'I' ?
													  sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val
													  op2->code == FPL_T ? (int)op2->attribute.flt_value : op2->attribute.int_value);
		case LT:
			exp_val.attribute.int_value = int_val <  (st_get_type(sym_table, op2->attribute.vid_offset) == 'F' ?
													 (int)sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val :
													  st_get_type(sym_table, op2->attribute.vid_offset) == 'I' ?
													  sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val
													  op2->code == FPL_T ? (int)op2->attribute.flt_value : op2->attribute.int_value);
			break;
	}
} else {
	int str_offset1 = op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : op1->attribute.str_offset;
	int str_offset2 = op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : op2->attribute.str_offset;
	switch (tkn->attribute.rel_op) {
		case EQ:
			exp_val.attribute.int_value = b_setmark(str_LTBL, str_offset1) == b_setmark(str_LTBL, str_offset2);
			break;
		case NE:
			exp_val.attribute.int_value = b_setmark(str_LTBL, str_offset1) != b_setmark(str_LTBL, str_offset2);
			break;
		case GT:
			exp_val.attribute.int_value = b_setmark(str_LTBL, str_offset1) >  b_setmark(str_LTBL, str_offset2);
		case LT:
			exp_val.attribute.int_value = b_setmark(str_LTBL, str_offset1) <  b_setmark(str_LTBL, str_offset2);
			break;
	}
}
	exp_val.attribute.int_value =  (op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : op1->attribute.str_offset)
								== (op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : op2->attribute.str_offset);