/*******************************************************************************
File name: parser.c
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
Course: CST 8152 - Compilers, Lab Section : 012
Assignment: 4
Date: 11 December 2015
Professor: Sv. Ranev
Purpose:  Implements a Recursive Descent Predictive Parser for PLATYPUS
*******************************************************************************/
#include "parser.h"

#define DEBUG
/*#undef DEBUG*/

extern STD sym_table;								/* Symbol Table Descriptor */
extern Buffer * str_LTBL;							/* String literal table */
extern int line;									/* source code line numbers - defined in scanner.c */
extern char * kw_table[];
extern Token mlwpar_next_token(Buffer * sc_buf);

static Buffer* sc_buf;
static Token lookahead;
static Token lval;
static InitialValue rval;
static Stack* assign_stack;
static Stack* operators;
static Stack* exp_stck;
static List* rpn_exp;

int synerrno;
int assign_asys, exp_asys, unry_asys;

/* static function declarations */
static void eval_rpn(void);
static int concat_str(Token* op1, Token* op2);

/*
Author: Sv.Ranev
*/
void parser(Buffer * in_buf) {
	sc_buf = in_buf;
	assign_asys = unry_asys = exp_asys = 0;
	lookahead = mlwpar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*******************************************************************************
Purpose: Matches the lookahead and the token input for the parser
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 11 December 2015
Called functions: syn_eh(), syn_printe(), mlwpar_next_token()
Parameters: int pr_token_code, int pr_token_attribute
Return value: void
*******************************************************************************/
void match(int pr_token_code, int pr_token_attribute) {
	if (lookahead.code == pr_token_code) {
		if (lookahead.code == SEOF_T) return;
		/* push until we reach stack capacity */
		if (assign_asys && !s_push(assign_stack, &lookahead)) assign_asys = 0;
		switch(pr_token_code) {
			case KW_T:
				if (lookahead.attribute.kwt_idx == pr_token_attribute) break;
				syn_eh(pr_token_code); return;
			case LOG_OP_T:
				if (lookahead.attribute.log_op == pr_token_attribute) {
					if (exp_asys) {
						while (!s_isempty(operators)) {
							Token* tkn = (Token*)s_pop(operators);
							if (lookahead.attribute.log_op == AND && tkn->code == LOG_OP_T && tkn->attribute.log_op == OR) {
								s_push(operators, tkn);
								break;
							} else l_add(rpn_exp, tkn);
						}
						s_push(operators, &lookahead);
					}
					break;
				}
				syn_eh(pr_token_code); return;
			case ART_OP_T:
				if (lookahead.attribute.arr_op == pr_token_attribute) {
					if (exp_asys) {
						if (unry_asys) {
							Token new_tkn;
							new_tkn.code = LPR_T;
							s_push(operators, &new_tkn);
							new_tkn.code = INL_T;
							new_tkn.attribute.int_value = 0;
							l_add(rpn_exp, &new_tkn);
							unry_asys = 0;
						} else {
							while (!s_isempty(operators)) {
								Token* tkn = (Token*)s_pop(operators);
								/* MULT and DIV have highest order of precedence(oop) so if top of stack is 
								already at highest oop then add popped token to rpn_exp list */
								if (tkn->attribute.arr_op == MULT || tkn->attribute.arr_op == DIV) l_add(rpn_exp, tkn);
								/* ASS_OP_T and LPR_T are lowest oop and if the lookahead token attribute is 
								MULT or DIV then it is higher oop so continue to push to stack */
								else if (tkn->code == ASS_OP_T || tkn->code == LPR_T 
									|| lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV) {
									s_push(operators, tkn);
									break;
								} else l_add(rpn_exp, tkn);
							}
						}
						s_push(operators, &lookahead);
					}
					break;
				}
				syn_eh(pr_token_code); return;
			case LPR_T:
				if (exp_asys) s_push(operators, &lookahead);
				break;
			case RPR_T:
				if (exp_asys) {
					Token* tkn;
					for (tkn = (Token*)s_pop(operators); tkn->code != LPR_T; tkn = (Token*)s_pop(operators))
						l_add(rpn_exp, tkn);
				}
				break;
			case REL_OP_T:
				if (lookahead.attribute.rel_op == pr_token_attribute) {
					if (exp_asys) {
						while (!s_isempty(operators)) {
							Token* tkn = (Token*)s_pop(operators);
							if (tkn->code == REL_OP_T) l_add(rpn_exp, tkn);
							else {
								s_push(operators, tkn);
								break;
							}
						}
						s_push(operators, &lookahead);
					}
					break;
				}
				syn_eh(pr_token_code); return;
			case SVID_T: case STR_T:
				if (exp_asys) l_add(rpn_exp, &lookahead);
				break;
			case SCC_OP_T:
				if (exp_asys) {
					while (!s_isempty(operators)) {
						Token* tkn = (Token*)s_pop(operators);
						if (tkn->code == SCC_OP_T) l_add(rpn_exp, tkn);
						else { 
							s_push(operators, tkn); 
							break; 
						}
					}
					s_push(operators, &lookahead);
				}
				break;
			case AVID_T: case INL_T: case FPL_T:
				if (exp_asys) l_add(rpn_exp, &lookahead);
				break;
			case ASS_OP_T:
				/* this grammar cannot have nested assignments so it is safe to 
				assume we can push to the stack without meeting a condition */
				if (exp_asys) s_push(operators, &lookahead);
				break;
		}
		if (!(lookahead = mlwpar_next_token(sc_buf)).code) { /* code equals ERR_T */
			syn_printe(); 
			lookahead = mlwpar_next_token(sc_buf); 
			++synerrno; 
		}
		return;
	}
	syn_eh(pr_token_code);
}

/*
Purpose: Error handling function that performs panic mode recovery
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
History/Versions: 1.0 / 11 December 2015
Called functions: exit(), mlwpar_next_token()
Parameters: int sync_token_code
Return value: void
*/
void syn_eh(int sync_token_code) {
	syn_printe(); ++synerrno;
	while((lookahead = mlwpar_next_token(sc_buf)).code != sync_token_code) if (lookahead.code == SEOF_T) exit(synerrno);
	if (lookahead.code == SEOF_T) return;
	lookahead = mlwpar_next_token(sc_buf);
}

/*
Author: Sv.Ranev
*/
void syn_printe(void) {
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n",line);
	printf("*****  Token code:%3d Attribute: ",t.code);
	switch(t.code){
		case ERR_T: /* ERR_T     0   Error token */
			printf("%s\n",t.attribute.err_lex);
			break;
		case SEOF_T: /*SEOF_T    1   Source end-of-file token */
			printf("NA\n" );
			break;
		case AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
		case SVID_T :/* SVID_T    3  String Variable identifier token */
			printf("%s\n",sym_table.pstvr[t.attribute.get_int].plex);
			break;
		case FPL_T: /* FPL_T     4  Floating point literal token */
			printf("%5.1f\n",t.attribute.flt_value);
			break;
		case INL_T: /* INL_T      5   Integer literal token */
			printf("%d\n",t.attribute.get_int);
			break;
		case STR_T:/* STR_T     6   String literal token */
			printf("%s\n",b_setmark(str_LTBL,t.attribute.str_offset));
			break;       
		case SCC_OP_T: /* 7   String concatenation operator token */
			printf("NA\n" );
			break;	
		case ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
			printf("NA\n" );
			break;
		case ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
			printf("%d\n",t.attribute.get_int);
			break;
		case REL_OP_T: /*REL_OP_T  10   Relational operator token */ 
			printf("%d\n",t.attribute.get_int);
			break;
		case LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
			printf("%d\n",t.attribute.get_int);
			break;	
		case LPR_T: /*LPR_T    12  Left parenthesis token */
			printf("NA\n" );
			break;
		case RPR_T: /*RPR_T    13  Right parenthesis token */
			printf("NA\n" );
			break;
		case LBR_T: /*    14   Left brace token */
			printf("NA\n" );
			break;
		case RBR_T: /*    15  Right brace token */
			printf("NA\n" );
			break;		
		case KW_T: /*     16   Keyword token */
			printf("%s\n",kw_table[t.attribute.get_int]);
			break;
		case COM_T: /* 17   Comma token */
			printf("NA\n");
			break;
		case EOS_T: /*    18  End of statement *(semi - colon) */
			printf("NA\n" );
			break; 		
		default:
			printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    } /*end switch*/
} /* end syn_printe()*/

/*
Purpose: Used by production functions to print out a string argument
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
History/Versions: 1.0 / 11 December 2015
Called functions: none
Parameters: char* string
Return value: void
*/
void gen_incode(char* string) {
	printf("%s\n", string);
}

/*
*	<program>			-> PLATYPUS { <opt statements> }
*	FIRST(<program>)	=  { KW_T(PLATYPUS) }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void program(void){
	match(KW_T,PLATYPUS); match(LBR_T,NO_ATTR); 
	opt_statements(); match(RBR_T,NO_ATTR);
	gen_incode("PLATY: Program parsed");
}
/*
*	<opt statements> 		-> <statements> | e
*	FIRST(<opt statements)	=  { AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT), e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void opt_statements(void) {
	switch(lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS
		&& lookahead.attribute.get_int != ELSE
		&& lookahead.attribute.get_int != THEN
		&& lookahead.attribute.get_int != REPEAT) { statements(); break; }
	default: gen_incode("PLATY: Opt_statements parsed");
	}
}
/*
*	<statements> 		-> <statement> <statements p>
*	FIRST(<statements>) =  { AVID, SVID, IF, USING, INPUT, OUTPUT }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void statements(void) {
	statement(); statements_p();
}
/*
*	<statements p> 			-> <statement> <statements p> | e
*	FIRST(<statements p>)	=  { AVID, SVID, IF, USING, INPUT, OUTPUT, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void statements_p(void) {
	switch(lookahead.code) {
	case AVID_T:
	case SVID_T: assignment_statement(); statements_p(); break;
	case KW_T:
		switch(lookahead.attribute.kwt_idx) {
		case IF: selection_statement(); statements_p(); break;
		case USING: iteration_statement(); statements_p(); break;
		case INPUT: input_statement(); statements_p(); break;
		case OUTPUT: output_statement(); statements_p(); break;
		}
	}
}
/*
*	<statement> 		-> <assignment statement> | <selection statement>
*						|  <iteration statement>  | <input statement>
*						|  <output statement>
*	FIRST(<statement>)  =  { AVID, SVID, IF, USING, INPUT, OUTPUT }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void statement(void) {
	if (lookahead.code == AVID_T || lookahead.code == SVID_T) assignment_statement();
	else if (lookahead.code == KW_T && 
		(lookahead.attribute.kwt_idx == IF || lookahead.attribute.kwt_idx == USING 
		|| lookahead.attribute.kwt_idx == INPUT || lookahead.attribute.kwt_idx == OUTPUT)) {
		switch(lookahead.attribute.kwt_idx) {
		case IF: selection_statement(); break;
		case USING: iteration_statement(); break;
		case INPUT: input_statement(); break;
		case OUTPUT: output_statement(); break;
		}
	} else syn_printe();
}
/*
*	<assignment statement>			-> <assignment expression> ;
*	FIRST(<assignment statement>)	=  { AVID, SVID }
*   Authors: Christopher Elliott, 040 570 022
*/
void assignment_statement(void) {
	Token literal;
	lval = lookahead;
	assign_asys = 1;

	assign_stack = s_create(4, 0, sizeof(Token), 'f');

	assignment_expression(); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");

	/* if assignment only involves one literal then update type */
	if ((*((Token*)s_pop(assign_stack))).code == EOS_T) {
		char type;
		literal = *((Token*)s_pop(assign_stack));
		#ifdef DEBUG
		printf("lval(%d) = literal(%d);\n", lval.code, literal.code);
		#endif
		type = st_get_type(sym_table, lval.attribute.vid_offset);
		#ifdef DEBUG
		printf("type of lval = %c;\n", type);
		#endif
		if (type == 'I' && literal.code == FPL_T || type == 'F' && literal.code == INL_T) {
			st_update_type(sym_table, lval.attribute.vid_offset, literal.code == INL_T ? 'I' : 'F');
			#ifdef DEBUG
			printf("After st_update_type: type of lval = %c;\n", st_get_type(sym_table, lval.attribute.vid_offset));
			#endif
		}
	}

	s_destroy(assign_stack);
	assign_asys = 0;
}
/*
*	<assignment expression>			-> AVID = <arithmetic expression> |  SVID = <string expression>
*	FIRST(<assignment expression>)	=  { AVID, SVID }
*   Authors: Christopher Elliott, 040 570 022
*/
void assignment_expression(void) {
	exp_asys = 1;
	rpn_exp = l_create(10, 10, sizeof(Token));
	operators = s_create(4, 4, sizeof(Token), 'a');

	if (lookahead.code == AVID_T) {
		match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");		
	} else if (lookahead.code == SVID_T) {
		match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
	} else {
		syn_printe();
	}

	/* evaluate RPN */
	#ifdef DEBUG
	printf("Before eval_rpn\n");
	#endif
	eval_rpn();
	#ifdef DEBUG
	printf("After eval_rpn\n");
	#endif
	s_destroy(operators);
	l_destroy(rpn_exp);
	exp_asys = 0;
}
/*
*	<selection statement>			-> IF ( <conditional expression> ) THEN <opt statments>
*									   ELSE { <opt statements> } ;
*	FIRST(<selection statement>)	=  { IF }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void selection_statement(void) {
	match(KW_T, IF); match(LPR_T, NO_ATTR); conditional_expression(); 
	match(RPR_T, NO_ATTR); match(KW_T, THEN); opt_statements(); match(KW_T, ELSE); 
	match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}
/*
*	<iteration statement> 			-> USING ( <assignment expression> , <conditional expression> , <assignment_expression> )
*									   REPEAT {
*			   							   <opt statements>
*									   };
*	FIRST(<iteration statement>)	=  { USING }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void iteration_statement(void) {
	match(KW_T, USING); match(LPR_T, NO_ATTR); assignment_expression(); match(COM_T, NO_ATTR);
	conditional_expression(); match(COM_T, NO_ATTR); assignment_expression(); match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT); match(LBR_T, NO_ATTR);  opt_statements(); match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}
/*
*	<input statement>			-> INPUT ( <variable list> ) ;
*	FIRST(<input statement>)	=  { INPUT }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void input_statement(void) {
	match(KW_T, INPUT); match(LPR_T, NO_ATTR); variable_list(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}
/*
*	<variable list>			-> <variable identifier> <variable list p>
*	FIRST(<variable list>)	=  { AVID_T, SVID_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void variable_list(void) {
	variable_identifier(); variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}
/*
*	<variable list p>			-> , <variable identifier> <variable list p> | e
*	FIRST(<variable list p>)	=  { ,, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void variable_list_p(void) {
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR); variable_identifier(); variable_list_p();
	}
}
/*
*	<variable identifier>			-> AVID_T | SVID_T
*	FIRST(<variable identifier>)	=  { AVID_T, SVID_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void variable_identifier(void) {
	if (lookahead.code == AVID_T) match(AVID_T, NO_ATTR);
	else if (lookahead.code == SVID_T) match(SVID_T, NO_ATTR);
	else syn_printe();
}
/*
*	<output statement>			-> OUTPUT ( <output list> );
*	FIRST(<output statement>)	=  { OUTPUT }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void output_statement(void) {
	match(KW_T, OUTPUT); match(LPR_T, NO_ATTR); output_list(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}
/*
*	<output list>			-> <variable list> | STR_T | e
*	FIRST(<output list>)	=  { STR_T, AVID_T, SVID_T, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void output_list(void) {
	switch(lookahead.code) {
	case AVID_T: case SVID_T: variable_list(); return;
	case STR_T: match(STR_T, NO_ATTR); gen_incode("PLATY: Output list (string literal) parsed"); return;
	}
	gen_incode("PLATY: Output list (empty) parsed");
}
/*
*	<arithmetic expression>			-> <unary arithmetic expression> | <additive arrithmetic expression>
*	FIRST(<arithmetic exression>)	=  { +, -, AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void arithmetic_expression(void) {
	if (lookahead.code == ART_OP_T && (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS)) unary_arithmetic_expression();
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T || lookahead.code == LPR_T) additive_arithmetic_expression();
	else { syn_printe(); return; }
	gen_incode("PLATY: Arithmetic expression parsed");
}
/*
*	<unary arithmetic expression>			-> + <primary arithmetic expression> | - <primary arithmetic expression>
*	FIRST(<unary arithmetic expression>)	=  { +, - }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void unary_arithmetic_expression(void) {
	Token* tkn;
	unry_asys = 1;
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == PLUS) {
		match(ART_OP_T, PLUS); primary_arithmetic_expression();
	} else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MINUS) {
		match(ART_OP_T, MINUS); primary_arithmetic_expression();
	} else { syn_printe(); return; }
	gen_incode("PLATY: Unary arithmetic expression parsed");
	for (tkn = (Token*)s_pop(operators); tkn->code != LPR_T; tkn = (Token*)s_pop(operators))
		l_add(rpn_exp, tkn);
}
/*
*	<additive arithmetic expression>		-> <multiplicative arithmetic expression> <additive arithmetic expression p>
*	FIRST(<additive arithmetic expression>) =  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
}
/*
*	<additive arithmetic expression p>			-> + <multiplicative arithmetic expression> <additive arithmetic expression p>
*												|  - <multiplicative arithmetic expression> <additive arithmetic expression p>
*												|  e
*	FIRST(<additive arithmetic expression p>)	=  { +, -, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void additive_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == PLUS) {
		match(ART_OP_T, PLUS); multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
		gen_incode("PLATY: Additive arithmetic expression parsed");
	} else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MINUS) {
		match(ART_OP_T, MINUS); multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
		gen_incode("PLATY: Additive arithmetic expression parsed");
	}
}
/*
*	<multiplicative arithmetic expression>			-> <primary arithmetic expression> <multiplicative arithmetic expression p>
*	FIRST(<multiplicative arithmetic expression>)	=  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
}
/*
*	<multiplicative arithmetic expression p>		-> * <primary arithmetic expression> <multiplicative arithmetic expression p>
*													|  / <primary arithmetic expression> <multiplicative arithmetic expression p>
*													|  e
*	FIRST(<multiplicative arithmetic expression p>) =  { *, /, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void multiplicative_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MULT) {
		match(ART_OP_T, MULT); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
		gen_incode("PLATY: Multiplicative arithmetic expression parsed");
	} else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == DIV) {
		match(ART_OP_T, DIV); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
		gen_incode("PLATY: Multiplicative arithmetic expression parsed");
	}
}
/*
*	<primary arithmetic expression>			-> AVID_T | FPL_T | INL_T | ( <arithmetic expression> )
*	FIRST(<primary arithmetic expression>)	=  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void primary_arithmetic_expression(void) {
	switch(lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR); break;
	case FPL_T: match(FPL_T, NO_ATTR); break;
	case INL_T: match(INL_T, NO_ATTR); break;
	case LPR_T: match(LPR_T, NO_ATTR); arithmetic_expression(); match(RPR_T, NO_ATTR); break;
	default: syn_printe(); return;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}
/*
*	<string expression>			-> <primary string expression> <string expression p>
*	FIRST(<string expression>)	=  { SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void string_expression(void) {
	primary_string_expression(); string_expression_p();
	gen_incode("PLATY: String expression parsed");
}
/*
*	<string expression p>			-> # <primary string expression> <string expression p> | e
*	FIRST(<string expression p>)	=  { #, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void string_expression_p(void) {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR); primary_string_expression(); string_expression_p();
	}
}
/*
*	<primary string expression>			-> SVID_T | STR_T
*	FIRST(<primary string expression>)	=  { SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void primary_string_expression(void) {
	if (lookahead.code == SVID_T) match(SVID_T, NO_ATTR);
	else if (lookahead.code == STR_T) match(STR_T, NO_ATTR);
	else { syn_printe(); return; }
	gen_incode("PLATY: Primary string expression parsed");
}
/*
*	<conditional expression>		-> <logical OR expression>
*	FIRST(<conditional expression>) =  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void conditional_expression(void) {
	exp_asys = 1;
	rpn_exp = l_create(10, 10, sizeof(Token));
	operators = s_create(4, 4, sizeof(Token), 'a');

	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");

	/* evaluate RPN */
	#ifdef DEBUG
	printf("Before conditional_expression eval_rpn\n");
	#endif
	eval_rpn();
	#ifdef DEBUG
	printf("After conditional_expression  eval_rpn\n");
	#endif
	s_destroy(operators);
	l_destroy(rpn_exp);
	exp_asys = 0;
}
/*
*	<logical OR expression>			-> <logical AND expression> <logical OR expression p>
*	FIRST(<logical OR expression>)	=  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void logical_or_expression(void) {
	logical_and_expression(); logical_or_expression_p();
}
/*
*	<logical OR expression p>			-> .OR. <logical AND expression> <logical OR expression p> | e
*	FIRST(<logical OR expression p>)	=  { .OR., e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void logical_or_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == OR) {
		match(LOG_OP_T, OR); logical_and_expression(); logical_or_expression_p();
		gen_incode("PLATY: Logical OR expression parsed");
	}
}
/*
*	<logical AND expression>		-> <relational expression> <logical AND expression p>
*	FIRST(<logical AND expression>) =  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void logical_and_expression(void) {
	relational_expression(); logical_and_expression_p();
}
/*
*	<logical AND expression p>			-> .AND. <relational expression> <logical AND expression p> | e
*	FIRST(<logical AND expression p>)	=  { .AND., e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void logical_and_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == AND) {
		match(LOG_OP_T, AND); relational_expression(); logical_and_expression_p();
		gen_incode("PLATY: Logical AND expression parsed");
	}
}
/*
*	<relational expression>			-> <primary a_relational expression> <primary a_relational expression p>
*									|  <primary s_relational expression> <primary s_relational expression p>
*	FIRST(relational expression>)	=  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void relational_expression(void) {
	switch(lookahead.code) {
	case AVID_T: case FPL_T: case INL_T:
		primary_a_relational_expression(); primary_a_relational_expression_p();
		break;
	case SVID_T: case STR_T:
		primary_s_relational_expression(); primary_s_relational_expression_p();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}
/*
*	<primary a_relational expression>			-> AVID_T | FPL_T | INL_T
*	FIRST(<primary a_relational expression>)	=  { AVID_T, FPL_T, INL_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void primary_a_relational_expression(void) {
	switch(lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR); break;
	case FPL_T:
		match(FPL_T, NO_ATTR); break;
	case INL_T:
		match(INL_T, NO_ATTR); break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}
/*
*	<primary a_relational expression p> 		-> == <primary a_relational expression>
*												|  <> <primary a_relational expression>
*												|  >  <primary a_relational expression>
*												|  <  <primary a_relational expression>
*	FIRST(<primary a_relational expression p>)	=  { ==, <>, >, < }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void primary_a_relational_expression_p(void) {
	if (lookahead.code == REL_OP_T) {
		switch(lookahead.attribute.rel_op) {
		case EQ:
			match(REL_OP_T, EQ); break;
		case NE:
			match(REL_OP_T, NE); break;
		case GT:
			match(REL_OP_T, GT); break;
		case LT:
			match(REL_OP_T, LT); break;
		}
		primary_a_relational_expression();
		return;
	}
	syn_printe();
}
/*
*	<primary s_relational expression> 			-> <primary string expression>
*	FIRST(<primary s_relational expression>)	=  { SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void primary_s_relational_expression(void) {
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}
/*
*	<primary s_relational expression p> 		-> == <primary s_relational expression>
*												|  <> <primary s_relational expression>
*												|  >  <primary s_relational expression>
*												|  <  <primary s_relational expression>
*	FIRST(<primary s_relational expression p>)	=  { ==, <>, >, < }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
void primary_s_relational_expression_p(void) {
	if (lookahead.code == REL_OP_T) {
		switch(lookahead.attribute.rel_op) {
		case EQ:
			match(REL_OP_T, EQ); break;
		case NE:
			match(REL_OP_T, NE); break;
		case GT:
			match(REL_OP_T, GT); break;
		case LT:
			match(REL_OP_T, LT); break;
		}
		primary_s_relational_expression();
		return;
	}
	syn_printe();
}
/*******************************************************************************
Purpose: Evaluate an expression in reverse polish notation (RPN). 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 30/12/2015
Called functions: [ 
	s_isempty(), l_add(), l_size(), s_create(), l_get(), s_pop(), printf(),
	s_push(), st_get_type(), st_get_record(), st_update_value()
]
Algorithm: unload the remaining operators off the stack, create a new stack for
		   the evaluation process, iterate over the RPN expression, when an
		   operand is encountered then push it on the stack, if an operator is
		   encountered then pop two operands of the stack, evaluate the
		   expression and push the value on the stack
*******************************************************************************/
static void eval_rpn(void) {
	int i, sz, err;
	Token new_tkn, *tkn, *op1, *op2;
	/* empty remaining operators on stack to expression list */
	while (!s_isempty(operators)) l_add(rpn_exp, s_pop(operators));
	sz = l_size(rpn_exp);
	exp_stck = s_create(sz, 4, sizeof(Token), 'a');
	#ifdef DEBUG
	printf("Expression token size is %d;\n", sz);
	#endif
	for (i = 0, err = 0; i < sz && !err; ++i) {
		tkn = (Token*)l_get(rpn_exp, i);
		switch (tkn->code) {
			case INL_T: case FPL_T: case SVID_T: case STR_T:
				s_push(exp_stck, tkn); 
				break;
			case AVID_T:
				#ifdef DEBUG
				printf("In AVID_T block;\n");
				#endif
				if (s_isempty(exp_stck)) { s_push(exp_stck, tkn); break; } /* first VID token so keep is lval */
				switch (st_get_type(sym_table, tkn->attribute.vid_offset)) {
					case 'I':
						new_tkn.code = INL_T;
						new_tkn.attribute.int_value = st_get_record(sym_table, tkn->attribute.vid_offset).i_value.int_val;
						break;
					case 'F':
						new_tkn.code = FPL_T;
						new_tkn.attribute.flt_value = st_get_record(sym_table, tkn->attribute.vid_offset).i_value.fpl_val;
						break;
				}
				s_push(exp_stck, &new_tkn);
				break;
			case ART_OP_T:
				op2 = (Token*)s_pop(exp_stck);
				op1 = (Token*)s_pop(exp_stck);
				switch (tkn->attribute.arr_op) {
					case PLUS:
						#ifdef DEBUG
						printf("In ART_OP_T: PLUS block;\n");
						#endif
						if (op1->code == FPL_T || op2->code == FPL_T) {
							new_tkn.code = FPL_T;
							new_tkn.attribute.flt_value = (op1->code == FPL_T ? op1->attribute.flt_value : (float)op1->attribute.int_value) 
														+ (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
						} else {
							new_tkn.code = INL_T;
							new_tkn.attribute.int_value = op1->attribute.int_value + op2->attribute.int_value;
						}
						break;
					case MINUS:
						#ifdef DEBUG
						printf("In ART_OP_T: MINUS block;\n");
						#endif
						if (op1->code == FPL_T || op2->code == FPL_T) {
							new_tkn.code = FPL_T;
							new_tkn.attribute.flt_value = (op1->code == FPL_T ? op1->attribute.flt_value : (float)op1->attribute.int_value) 
														- (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
						} else {
							new_tkn.code = INL_T;
							new_tkn.attribute.int_value = op1->attribute.int_value - op2->attribute.int_value;
						}
						break;
					case MULT:
						#ifdef DEBUG
						printf("In ART_OP_T: MULT block;\n");
						#endif
						if (op1->code == FPL_T || op2->code == FPL_T) {
							new_tkn.code = FPL_T;
							new_tkn.attribute.flt_value = (op1->code == FPL_T ? op1->attribute.flt_value : (float)op1->attribute.int_value) 
														* (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
						} else {
							new_tkn.code = INL_T;
							new_tkn.attribute.int_value = op1->attribute.int_value * op2->attribute.int_value;
						}
						break;
					case DIV:
						#ifdef DEBUG
						printf("In ART_OP_T: DIV block; %f and %f\n", op1->attribute.flt_value, op2->attribute.flt_value);
						#endif
						if (op2->attribute.int_value == 0) {
							printf("Cannot divide by zero.\n");
							err = 1;
							break;
						}
						if (op1->code == FPL_T || op2->code == FPL_T) {
							printf("In FPL_T block;\n");
							new_tkn.code = FPL_T;
							new_tkn.attribute.flt_value = (op1->code == FPL_T ? op1->attribute.flt_value : (float)op1->attribute.int_value) 
														/ (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
						} else {
							new_tkn.code = INL_T;
							new_tkn.attribute.int_value = op1->attribute.int_value / op2->attribute.int_value;
						}
						break;
				}
				s_push(exp_stck, &new_tkn);
				break;
			case SCC_OP_T:
				#ifdef DEBUG
				printf("In SCC_OP_T block;\n");
				#endif
				op2 = (Token*)s_pop(exp_stck);
				op1 = (Token*)s_pop(exp_stck);
				new_tkn.code = STR_T;
				new_tkn.attribute.str_offset = concat_str(op1, op2);
				s_push(exp_stck, &new_tkn); 
				#ifdef DEBUG
				printf("String offset: %d\n", new_tkn.attribute.str_offset);
				#endif
				break;
			case LOG_OP_T:
				#ifdef DEBUG
				printf("In LOG_OP_T block;\n");
				#endif
				op2 = (Token*)s_pop(exp_stck);
				op1 = (Token*)s_pop(exp_stck);
				new_tkn.code = INL_T;
				new_tkn.attribute.int_value = ((tkn->attribute.log_op == AND) ? 
					(op1->attribute.int_value && op2->attribute.int_value) : (op1->attribute.int_value || op2->attribute.int_value));
				s_push(exp_stck, &new_tkn);
				break;
			case REL_OP_T:
				#ifdef DEBUG
				printf("In REL_OP_T block;\n");
				#endif
				op2 = (Token*)s_pop(exp_stck);
				op1 = (Token*)s_pop(exp_stck);
				switch (tkn->attribute.rel_op) {
					case EQ:
						new_tkn.attribute.int_value = (op1->code == SVID_T ? 
							st_get_record(sym_table, op1->attribute.vid_offset).i_value.str_offset : (op1->code == STR_T ? 
								op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
													== (op2->code == SVID_T ? 
														st_get_record(sym_table, op2->attribute.vid_offset).i_value.str_offset : (op2->code == STR_T ? 
															op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
						break;
					case NE:
						new_tkn.attribute.int_value = (op1->code == SVID_T ? 
							st_get_record(sym_table, op1->attribute.vid_offset).i_value.str_offset : (op1->code == STR_T ? 
								op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
													!= (op2->code == SVID_T ? 
														st_get_record(sym_table, op2->attribute.vid_offset).i_value.str_offset : (op2->code == STR_T ? 
															op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
						break;
					case GT:
						new_tkn.attribute.int_value = (op1->code == SVID_T ? 
							st_get_record(sym_table, op1->attribute.vid_offset).i_value.str_offset : (op1->code == STR_T ? 
								op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
													> (op2->code == SVID_T ? 
														st_get_record(sym_table, op2->attribute.vid_offset).i_value.str_offset : (op2->code == STR_T ? 
															op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
						break;
					case LT:
						new_tkn.attribute.int_value = (op1->code == SVID_T ? 
							st_get_record(sym_table, op1->attribute.vid_offset).i_value.str_offset : (op1->code == STR_T ? 
								op1->attribute.str_offset : (op1->code == FPL_T ? op1->attribute.flt_value : op1->attribute.int_value)))
													< (op2->code == SVID_T ? 
														st_get_record(sym_table, op2->attribute.vid_offset).i_value.str_offset : (op2->code == STR_T ? 
															op2->attribute.str_offset : (op2->code == FPL_T ? op2->attribute.flt_value : op2->attribute.int_value)));
						break;
				}
				new_tkn.code = INL_T;
				s_push(exp_stck, &new_tkn);
				break;
			case ASS_OP_T:
				#ifdef DEBUG
				printf("In ASS_OP_T block;\n");
				#endif
				op2 = (Token*)s_pop(exp_stck);
				op1 = (Token*)s_pop(exp_stck);
				switch (st_get_type(sym_table, op1->attribute.vid_offset)) {
					case 'I':
						rval.int_val = (op2->code == INL_T ? op2->attribute.int_value : (int)op2->attribute.flt_value);
						break;
					case 'F':
						rval.fpl_val = (op2->code == FPL_T ? op2->attribute.flt_value : (float)op2->attribute.int_value);
						break;
					case 'S':
						rval.str_offset = op2->attribute.str_offset;
						break;
				}		
				st_update_value(sym_table, op1->attribute.vid_offset, rval);
				break;
		}
	}
	#ifdef DEBUG
	if (!s_isempty(exp_stck)) {
		printf("Conditional expression value is %s.\n", (((Token*)s_pop(exp_stck))->attribute.int_value ? "true" : "false"));
	}
	#endif
	s_destroy(exp_stck);
}
/*******************************************************************************
Purpose: Concatenate two strings
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 30/12/2015
Called functions: [ b_size(), st_get_record(), b_setmark(), b_addc() ]
Parameters: op1 is the first operand, op2 is the second operand
Return value: the offset of the concatenated string stored on the str_LTBL
Algorithm: set the str_offset to the next available position on the str_LTBL,
		   get the offset to the first operand string value from the str_LTBL,
		   offset value of -1 represents an empty string, add each character to
		   start a new entry in the str_LTBL, repeat for the second operand and
		   return the offset to the new entry
*******************************************************************************/
static int concat_str(Token* op1, Token* op2) {
	char* str;
	int i, mark, str_offset;
	str_offset = b_size(str_LTBL);
	mark = (op1->code == SVID_T ? 
				st_get_record(sym_table, op1->attribute.vid_offset).i_value.str_offset : 
				op1->attribute.str_offset);
	if (mark >= 0) {
		str = b_setmark(str_LTBL, mark);
		for (i = 0; str[i]; ++i) b_addc(str_LTBL, str[i]);
	}
	mark = (op2->code == SVID_T ? 
				st_get_record(sym_table, op2->attribute.vid_offset).i_value.str_offset : 
				op2->attribute.str_offset);
	if (mark >= 0) {
		str = b_setmark(str_LTBL, mark);
		for (i=0; str[i]; ++i) b_addc(str_LTBL, str[i]);
	}
	b_addc(str_LTBL, '\0');
	return str_offset;
}