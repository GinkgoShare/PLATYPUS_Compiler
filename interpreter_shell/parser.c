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
#undef DEBUG
#define DEBUG0
#undef DEBUG0

#define get_num_value(op) ((op).code == AVID_T && st_get_type(sym_table, (op).attribute.vid_offset) == 'F' ? \
						  sym_table.pstvr[(op).attribute.vid_offset].i_value.fpl_val : \
						  (op).code == AVID_T && st_get_type(sym_table, (op).attribute.vid_offset) == 'I' ? \
						  sym_table.pstvr[(op).attribute.vid_offset].i_value.int_val : \
						  (op).code == FPL_T ? (op).attribute.flt_value : (op).attribute.int_value)


extern STD sym_table;								/* Symbol Table Descriptor */
extern Buffer * str_LTBL;							/* String literal table */
extern int line;									/* source code line numbers - defined in scanner.c */
extern char * kw_table[];
extern Token mlwpar_next_token(Buffer * sc_buf);

static Buffer* sc_buf;
static Token lookahead;
static Token exp_value;
static Stack* operators;
static List* iterable;
static List* reusable_tkns;

int synerrno;
int asgn_stmt_asys, unry_asys;
int save_tkns, reuse_tkns, execute;

/* static function declarations */
static void match(int, int);
static void syn_eh(int);
static void syn_printe(void);
static void gen_incode(char*);
static char get_exp_type(void);
static Token evaluate_expression(List* iterable);
static int concat_str(Token* op1, Token* op2);

/* grammar productions */
static void program(void);
static void opt_statements(void);
static void statements(void);
static void statements_p(void);
static void statement(void);
static void assignment_statement(void);
static void assignment_expression(void);
static void selection_statement(void);
static void iteration_statement(void);
static void input_statement(void);
static void variable_list(void);
static void variable_list_p(void);
static void variable_identifier(void);
static void output_statement(void);
static void output_list(void);
static void arithmetic_expression(void);
static void unary_arithmetic_expression(void);
static void additive_arithmetic_expression(void);
static void additive_arithmetic_expression_p(void);
static void multiplicative_arithmetic_expression(void);
static void multiplicative_arithmetic_expression_p(void);
static void primary_arithmetic_expression(void);
static void string_expression(void);
static void string_expression_p(void);
static void primary_string_expression(void);
static void conditional_expression(void);
static void logical_or_expression(void);
static void logical_or_expression_p(void);
static void logical_and_expression(void);
static void logical_and_expression_p(void);
static void relational_expression(void);
static void primary_a_relational_expression(void);
static void primary_a_relational_expression_p(void);
static void primary_s_relational_expression(void);
static void primary_s_relational_expression_p(void);

/*
Author: Sv.Ranev
*/
void parser(Buffer * in_buf) {
	execute = 1;
	sc_buf = in_buf;
	asgn_stmt_asys = unry_asys = 0;
	lookahead = mlwpar_next_token(sc_buf);
	iterable = l_create(10, 10, sizeof(Token));
	operators = s_create(10, 10, sizeof(Token), 'a');
	program(); match(SEOF_T, NO_ATTR);
	s_destroy(operators);
	l_destroy(iterable);
}

/*******************************************************************************
Purpose: Matches the lookahead and the token input for the parser
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 11 December 2015
Called functions: syn_eh(), syn_printe(), mlwpar_next_token()
Parameters: int pr_token_code, int pr_token_attribute
Return value: void
*******************************************************************************/
static void match(int pr_token_code, int pr_token_attribute) {
	if (lookahead.code == pr_token_code) {
		if (lookahead.code == SEOF_T) return;
		switch(pr_token_code) {
			case KW_T:
				if (lookahead.attribute.kwt_idx == pr_token_attribute) {
					if (lookahead.attribute.kwt_idx == OUTPUT || lookahead.attribute.kwt_idx == INPUT)
						l_add(iterable, &lookahead);
					break;
				}
				syn_eh(pr_token_code); return;
			case LOG_OP_T: case REL_OP_T:
				if (pr_token_code == LOG_OP_T && lookahead.attribute.log_op == pr_token_attribute 
					|| pr_token_code == REL_OP_T && lookahead.attribute.rel_op == pr_token_attribute) {
					while (!s_isempty(operators)) {
						Token* tkn = (Token*)s_pop(operators);
						if (tkn->code == REL_OP_T || lookahead.code == LOG_OP_T 
						&& (tkn->attribute.log_op == AND || lookahead.attribute.log_op == OR)) l_add(iterable, tkn);
						else {
							s_push(operators, tkn);
							break;
						}
					}
					s_push(operators, &lookahead);
					break;
				}
				syn_eh(pr_token_code);
				return;
			case ART_OP_T:
				if (lookahead.attribute.arr_op == pr_token_attribute) {
					/* grammar only allows unary operators in single expressions so no need to check precedence */
					if (unry_asys) {
						lookahead.attribute.arr_op = lookahead.attribute.arr_op == MINUS ? UMINUS : UPLUS;
						unry_asys = 0;
					} else {
						while (!s_isempty(operators)) {
							Token* tkn = (Token*)s_pop(operators);
							/* MULT and DIV have highest order of precedence(oop) so if top of stack is 
							already at highest oop then add popped token to rpn_exp list */
							if (tkn->attribute.arr_op == MULT || tkn->attribute.arr_op == DIV) l_add(iterable, tkn);
							/* ASS_OP_T and LPR_T are lowest oop and if the lookahead token attribute is 
							MULT or DIV then it is higher oop so continue to push to stack */
							else if (tkn->code == ASS_OP_T || tkn->code == LPR_T 
								|| lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV) {
								s_push(operators, tkn);
								break;
							} else l_add(iterable, tkn);
						}
					}
					s_push(operators, &lookahead);
					break;
				}
				syn_eh(pr_token_code);
				return;
			case RPR_T: /* add operators to the rpn_exp until we reach the previous left parenthesis */
				while (!s_isempty(operators)) {
					Token* tkn = (Token*)s_pop(operators);
					if (tkn->code != LPR_T) l_add(iterable, tkn);
					else break;
				}
				break;
			case SCC_OP_T:
				while (!s_isempty(operators)) {
					Token* tkn = (Token*)s_pop(operators);
					if (tkn->code == SCC_OP_T) l_add(iterable, tkn);
					else {
						s_push(operators, tkn);
						break;
					}
				}
				s_push(operators, &lookahead);
				break;
			case AVID_T: case STR_T: case FPL_T: case INL_T: case SVID_T:
				l_add(iterable, &lookahead);
				break;
			case ASS_OP_T: case LPR_T:
				/* this grammar cannot have nested assignments so it is safe to 
				assume we can push to the stack without meeting a condition */
				s_push(operators, &lookahead);
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
static void syn_eh(int sync_token_code) {
	syn_printe(); ++synerrno;
	while((lookahead = mlwpar_next_token(sc_buf)).code != sync_token_code) if (lookahead.code == SEOF_T) exit(synerrno);
	if (lookahead.code == SEOF_T) return;
	lookahead = mlwpar_next_token(sc_buf);
}

/*
Author: Sv.Ranev
*/
static void syn_printe(void) {
	Token t = lookahead;
	asgn_stmt_asys = execute = 0;
#ifdef DEBUG
printf("execute is %d\n", execute);
#endif
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
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 11 December 2015
Called functions: none
Parameters: char* string
Return value: void
*/
static void gen_incode(char* string) {
	printf("%s\n", string);
}

/*
*	<program>			-> PLATYPUS { <opt statements> }
*	FIRST(<program>)	=  { KW_T(PLATYPUS) }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void program(void){
	match(KW_T,PLATYPUS); match(LBR_T,NO_ATTR); 
	opt_statements(); match(RBR_T,NO_ATTR);
}
/*
*	<opt statements> 		-> <statements> | e
*	FIRST(<opt statements)	=  { AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT), e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void opt_statements(void) {
	switch(lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS
		&& lookahead.attribute.get_int != ELSE
		&& lookahead.attribute.get_int != THEN
		&& lookahead.attribute.get_int != REPEAT) { statements(); break; }
	}
}
/*
*	<statements> 		-> <statement> <statements p>
*	FIRST(<statements>) =  { AVID, SVID, IF, USING, INPUT, OUTPUT }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void statements(void) {
	statement(); statements_p();
}
/*
*	<statements p> 			-> <statement> <statements p> | e
*	FIRST(<statements p>)	=  { AVID, SVID, IF, USING, INPUT, OUTPUT, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void statements_p(void) {
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
static void statement(void) {
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
static void assignment_statement(void) {
	asgn_stmt_asys = 1;
	assignment_expression(); match(EOS_T, NO_ATTR);
	asgn_stmt_asys = 0;
}
/*
*	<assignment expression>			-> AVID = <arithmetic expression> |  SVID = <string expression>
*	FIRST(<assignment expression>)	=  { AVID, SVID }
*   Authors: Christopher Elliott, 040 570 022
*/
static void assignment_expression(void) {
	l_reset(iterable);
	s_reset(operators);
	if (lookahead.code == AVID_T) {
		match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); arithmetic_expression();
	} else if (lookahead.code == SVID_T) {
		match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); string_expression();
	} else {
		syn_printe();
	}
	/* unload remaining operators off stack to expression list */
	while (!s_isempty(operators)) l_add(iterable, s_pop(operators));
	if (execute) exp_value = evaluate_expression(iterable);
}
/*
*	<selection statement>			-> IF ( <conditional expression> ) THEN <opt statments>
*									   ELSE { <opt statements> } ;
*	FIRST(<selection statement>)	=  { IF }
*   Authors: Christopher Elliott, 040 570 022
*/
static void selection_statement(void) {
	int execute_chain = execute;
	match(KW_T, IF); match(LPR_T, NO_ATTR); conditional_expression(); 
	match(RPR_T, NO_ATTR); match(KW_T, THEN);
	if (execute_chain) execute = exp_value.attribute.int_value;
	opt_statements(); match(KW_T, ELSE);
	if (execute_chain) execute = !exp_value.attribute.int_value;
	match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	execute = execute_chain;
}
/*
*	<iteration statement> 			-> USING ( <assignment expression> , <conditional expression> , <assignment_expression> )
*									   REPEAT {
*			   							   <opt statements>
*									   };
*	FIRST(<iteration statement>)	=  { USING }
*   Authors: Christopher Elliott, 040 570 022
*/
static void iteration_statement(void) {
	List *cond_exp, *assgn_exp;
	int istrue, execute_chain = execute, retract_mark;
	match(KW_T, USING); match(LPR_T, NO_ATTR);
	assignment_expression(); match(COM_T, NO_ATTR);
	conditional_expression(); cond_exp = l_copy(iterable);
	istrue = exp_value.attribute.int_value;
	match(COM_T, NO_ATTR);
	execute = 0;
	assignment_expression();
	assgn_exp = l_copy(iterable);
	execute = execute_chain;
	match(RPR_T, NO_ATTR); match(KW_T, REPEAT);
	/* set marker before reading first token of opt_statements */
	retract_mark = b_getc_offset(sc_buf);
	match(LBR_T, NO_ATTR);
	if (!istrue) { /* parse but do not execute statement */
		execute = 0;
		opt_statements();
	} else { /* parse while executing statement */
		for (b_setmark(sc_buf, retract_mark); istrue ; b_setmark(sc_buf, retract_mark)) {
			b_retract_to_mark(sc_buf);
			lookahead = mlwpar_next_token(sc_buf);
			opt_statements();
			evaluate_expression(assgn_exp);
			istrue = evaluate_expression(cond_exp).attribute.int_value;
		}
	}
	
	match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	l_destroy(cond_exp);
	l_destroy(assgn_exp);
	execute = execute_chain;
}
/*
*	<input statement>			-> INPUT ( <variable list> ) ;
*	FIRST(<input statement>)	=  { INPUT }
*   Authors: Christopher Elliott, 040 570 022
*/
static void input_statement(void) {
	l_reset(iterable);
	s_reset(operators);
	match(KW_T, INPUT); match(LPR_T, NO_ATTR); variable_list(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	/* evaluate expression */
#ifdef DEBUG
printf("execute is %d\n", execute);
#endif
	if (execute) exp_value = evaluate_expression(iterable);
}
/*
*	<variable list>			-> <variable identifier> <variable list p>
*	FIRST(<variable list>)	=  { AVID_T, SVID_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void variable_list(void) {
	variable_identifier(); variable_list_p();
}
/*
*	<variable list p>			-> , <variable identifier> <variable list p> | e
*	FIRST(<variable list p>)	=  { ,, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void variable_list_p(void) {
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR); variable_identifier(); variable_list_p();
	}
}
/*
*	<variable identifier>			-> AVID_T | SVID_T
*	FIRST(<variable identifier>)	=  { AVID_T, SVID_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void variable_identifier(void) {
	if (lookahead.code == AVID_T) match(AVID_T, NO_ATTR);
	else if (lookahead.code == SVID_T) match(SVID_T, NO_ATTR);
	else syn_printe();
}
/*
*	<output statement>			-> OUTPUT ( <output list> );
*	FIRST(<output statement>)	=  { OUTPUT }
*   Authors: Christopher Elliott
*/
static void output_statement(void) {
	l_reset(iterable);
	s_reset(operators);
	match(KW_T, OUTPUT); match(LPR_T, NO_ATTR); output_list(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	/* evaluate expression */
	if (execute) exp_value = evaluate_expression(iterable);
}
/*
*	<output list>			-> <variable list> | STR_T | e
*	FIRST(<output list>)	=  { STR_T, AVID_T, SVID_T, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void output_list(void) {
	switch(lookahead.code) {
	case AVID_T: case SVID_T: variable_list(); return;
	case STR_T: match(STR_T, NO_ATTR); return;
	}
}
/*
*	<arithmetic expression>			-> <unary arithmetic expression> | <additive arrithmetic expression>
*	FIRST(<arithmetic exression>)	=  { +, -, AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void arithmetic_expression(void) {
	if (lookahead.code == ART_OP_T && (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS)) unary_arithmetic_expression();
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T || lookahead.code == LPR_T) additive_arithmetic_expression();
	else { syn_printe(); return; }
}
/*
*	<unary arithmetic expression>			-> + <primary arithmetic expression> | - <primary arithmetic expression>
*	FIRST(<unary arithmetic expression>)	=  { +, - }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void unary_arithmetic_expression(void) {
	unry_asys = 1;
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == PLUS) {
		match(ART_OP_T, PLUS);
	} else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MINUS) {
		match(ART_OP_T, MINUS);
	} else { 
		syn_printe();
		return;
	}
	primary_arithmetic_expression();
}
/*
*	<additive arithmetic expression>		-> <multiplicative arithmetic expression> <additive arithmetic expression p>
*	FIRST(<additive arithmetic expression>) =  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
}
/*
*	<additive arithmetic expression p>			-> + <multiplicative arithmetic expression> <additive arithmetic expression p>
*												|  - <multiplicative arithmetic expression> <additive arithmetic expression p>
*												|  e
*	FIRST(<additive arithmetic expression p>)	=  { +, -, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void additive_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == PLUS) {
		match(ART_OP_T, PLUS); multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
	} else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MINUS) {
		match(ART_OP_T, MINUS); multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
	}
}
/*
*	<multiplicative arithmetic expression>			-> <primary arithmetic expression> <multiplicative arithmetic expression p>
*	FIRST(<multiplicative arithmetic expression>)	=  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
}
/*
*	<multiplicative arithmetic expression p>		-> * <primary arithmetic expression> <multiplicative arithmetic expression p>
*													|  / <primary arithmetic expression> <multiplicative arithmetic expression p>
*													|  e
*	FIRST(<multiplicative arithmetic expression p>) =  { *, /, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void multiplicative_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MULT) {
		match(ART_OP_T, MULT); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
	} else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == DIV) {
		match(ART_OP_T, DIV); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
	}
}
/*
*	<primary arithmetic expression>			-> AVID_T | FPL_T | INL_T | ( <arithmetic expression> )
*	FIRST(<primary arithmetic expression>)	=  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void primary_arithmetic_expression(void) {
	switch(lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR); break;
	case FPL_T: match(FPL_T, NO_ATTR); break;
	case INL_T: match(INL_T, NO_ATTR); break;
	case LPR_T: match(LPR_T, NO_ATTR); arithmetic_expression(); match(RPR_T, NO_ATTR); break;
	default: syn_printe(); return;
	}
}
/*
*	<string expression>			-> <primary string expression> <string expression p>
*	FIRST(<string expression>)	=  { SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void string_expression(void) {
	primary_string_expression(); string_expression_p();
}
/*
*	<string expression p>			-> # <primary string expression> <string expression p> | e
*	FIRST(<string expression p>)	=  { #, e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void string_expression_p(void) {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR); primary_string_expression(); string_expression_p();
	}
}
/*
*	<primary string expression>			-> SVID_T | STR_T
*	FIRST(<primary string expression>)	=  { SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void primary_string_expression(void) {
	if (lookahead.code == SVID_T) match(SVID_T, NO_ATTR);
	else if (lookahead.code == STR_T) match(STR_T, NO_ATTR);
	else { syn_printe(); return; }
}
/*
*	<conditional expression>		-> <logical OR expression>
*	FIRST(<conditional expression>) =  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void conditional_expression(void) {
	l_reset(iterable);
	s_reset(operators);
	logical_or_expression();
	/* unload remaining operators off stack to expression list */
	while (!s_isempty(operators)) l_add(iterable, s_pop(operators));
	/* evaluate expression */
	if (execute) exp_value = evaluate_expression(iterable);
}
/*
*	<logical OR expression>			-> <logical AND expression> <logical OR expression p>
*	FIRST(<logical OR expression>)	=  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void logical_or_expression(void) {
	logical_and_expression(); logical_or_expression_p();
}
/*
*	<logical OR expression p>			-> .OR. <logical AND expression> <logical OR expression p> | e
*	FIRST(<logical OR expression p>)	=  { .OR., e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void logical_or_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == OR) {
		match(LOG_OP_T, OR); logical_and_expression(); logical_or_expression_p();
	}
}
/*
*	<logical AND expression>		-> <relational expression> <logical AND expression p>
*	FIRST(<logical AND expression>) =  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void logical_and_expression(void) {
	relational_expression(); logical_and_expression_p();
}
/*
*	<logical AND expression p>			-> .AND. <relational expression> <logical AND expression p> | e
*	FIRST(<logical AND expression p>)	=  { .AND., e }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void logical_and_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == AND) {
		match(LOG_OP_T, AND); relational_expression(); logical_and_expression_p();
	}
}
/*
*	<relational expression>			-> <primary a_relational expression> <primary a_relational expression p>
*									|  <primary s_relational expression> <primary s_relational expression p>
*	FIRST(relational expression>)	=  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void relational_expression(void) {
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
}
/*
*	<primary a_relational expression>			-> AVID_T | FPL_T | INL_T
*	FIRST(<primary a_relational expression>)	=  { AVID_T, FPL_T, INL_T }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void primary_a_relational_expression(void) {
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
}
/*
*	<primary a_relational expression p> 		-> == <primary a_relational expression>
*												|  <> <primary a_relational expression>
*												|  >  <primary a_relational expression>
*												|  <  <primary a_relational expression>
*	FIRST(<primary a_relational expression p>)	=  { ==, <>, >, < }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void primary_a_relational_expression_p(void) {
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
static void primary_s_relational_expression(void) {
	primary_string_expression();
}
/*
*	<primary s_relational expression p> 		-> == <primary s_relational expression>
*												|  <> <primary s_relational expression>
*												|  >  <primary s_relational expression>
*												|  <  <primary s_relational expression>
*	FIRST(<primary s_relational expression p>)	=  { ==, <>, >, < }
*   Authors: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
*/
static void primary_s_relational_expression_p(void) {
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
Purpose: Evaluate an expression. Arithmetic expressions are in reverse polish 
		 notation (RPN) at this point. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 30/12/2015
Called functions: [ 
	s_isempty(), q_add(), l_size(), s_create(), q_remove(), s_pop(), printf(),
	s_push(), st_get_type(), st_get_record(), st_update_value()
]
Algorithm: unload the remaining operators off the stack, create a new stack for
		   the evaluation process, iterate over the RPN expression, when an
		   operand is encountered then push it on the stack, if an operator is
		   encountered then pop two operands of the stack, evaluate the
		   expression and push the value on the stack
*******************************************************************************/
static Token evaluate_expression(List* iterable) {

	/* PRE-CONDITIONS */
	int sz;
	Token *tkn;
	Token exp_val;
	Stack* exp_stck;
	sz = l_size(iterable);
	exp_stck = s_create(sz, 4, sizeof(Token), 'a');
	/* END PRE-CONDITIONS */

	/* EXPRESSION EVALUATION LOOP */
	while (l_hasnext(iterable)) {
		tkn = (Token*)l_getnext(iterable);
		/* value tokens are in this range */
		if (tkn->code > 1 && tkn->code < 7) s_push(exp_stck, tkn);
		/* operator tokens are in this range */
		else if (tkn->code > 6 && tkn->code < 12) {
			Token *op1 = NULL;
			/* pop tokens off the stack */
			Token *op2 = (Token*)s_pop(exp_stck);
			if (tkn->code != ART_OP_T || tkn->attribute.arr_op < UPLUS) op1 = (Token*)s_pop(exp_stck);
			switch (tkn->code) {
				case SCC_OP_T:
#ifdef DEBUG
printf("In SCC_OP_T\n");
#endif
					exp_val.code = STR_T;
					exp_val.attribute.str_offset = concat_str(op1, op2);
					break;
				case ASS_OP_T:
#ifdef DEBUG
printf("In ASS_OP_T\n");
#endif
					/* if assignment statement only involves one literal then update type */
					if (asgn_stmt_asys && sz == 3) {
						char type = st_get_type(sym_table, op1->attribute.vid_offset);
						if (type == 'I' && op2->code == FPL_T || type == 'F' && op2->code == INL_T) {
							st_update_type(sym_table, op1->attribute.vid_offset, op2->code == INL_T ? 'I' : 'F');
						}
					}
					{ /* Update value */
						InitialValue rval;
						switch (st_get_type(sym_table, op1->attribute.vid_offset)) {
							case 'I':
#ifdef DEBUG
printf("In case 'I'\n");
#endif
								exp_val.attribute.int_value = rval.int_val = (int)get_num_value(*op2);
								break;
							case 'F':
#ifdef DEBUG
printf("In case 'F'\n");
#endif
								exp_val.attribute.flt_value = rval.fpl_val = (float)get_num_value(*op2);
								break;
							case 'S':
#ifdef DEBUG
printf("In case 'S'\n");
#endif
								exp_val.attribute.str_offset = rval.str_offset = op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : op2->attribute.str_offset;
								break;
						}
						st_update_value(sym_table, op1->attribute.vid_offset, rval);
					}
					exp_val.code = op2->code;
					break;
				case ART_OP_T:
#ifdef DEBUG
printf("In ART_OP_T\n");
#endif
					/* divide by zero will break out of evaluation and print the error to console */
					if (tkn->attribute.arr_op == DIV && (op2->code == AVID_T && !sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val || !op2->attribute.int_value)) {
						printf("***CANNOT DIVIDE BY ZERO***\n");
						l_reset_iterator(iterable);
						exp_val.code = ERR_T;
						strcpy(exp_val.attribute.err_lex, "RUN TIME ERROR");
						return exp_val;
					}
#ifdef DEBUG
printf("op2->code is %d and op1->code is %d\n", op2->code, op1 ? op1->code : -1);
#endif
					exp_val.code = INL_T;
					{
						int int_val1, int_val2;
						float flp_val1, flp_val2;
						if (op2->code == AVID_T && st_get_type(sym_table, op2->attribute.vid_offset) == 'F' || op2->code == FPL_T) {
							flp_val2 = op2->code == AVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.fpl_val : op2->attribute.flt_value;
							exp_val.code = FPL_T;
						} else
							int_val2 = op2->code == AVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.int_val : op2->attribute.int_value;
						if (op1) {
							if (op1->code == AVID_T && st_get_type(sym_table, op1->attribute.vid_offset) == 'F' || op1->code == FPL_T) {
								flp_val1 = op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.fpl_val : op1->attribute.flt_value;
								if (exp_val.code == INL_T) flp_val2 = (float)int_val2;
								exp_val.code = FPL_T;
							} else {
								if (exp_val.code == FPL_T) flp_val1 = (float)(op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.int_val : op1->attribute.int_value);
								else int_val1 = op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.int_val : op1->attribute.int_value;
							}
						}
						switch (tkn->attribute.arr_op) {
							case PLUS:
#ifdef DEBUG
printf("In PLUS\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = flp_val1 + flp_val2;
								else exp_val.attribute.int_value = int_val1 + int_val2;
								break;
							case MINUS:
#ifdef DEBUG
printf("In MINUS\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = flp_val1 - flp_val2;
								else exp_val.attribute.int_value = int_val1 - int_val2;
								break;
							case MULT:
#ifdef DEBUG
printf("In MULT\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = flp_val1 * flp_val2;
								else exp_val.attribute.int_value = int_val1 * int_val2;
								break;
							case DIV:
#ifdef DEBUG
printf("In DIV\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = flp_val1 / flp_val2;
								else exp_val.attribute.int_value = int_val1 / int_val2;
								break;
							case UPLUS:
#ifdef DEBUG
printf("In UPLUS\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = flp_val2;
								else exp_val.attribute.int_value = int_val2;
								break;
							case UMINUS:
#ifdef DEBUG
printf("In UMINUS\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = -flp_val2;
								else exp_val.attribute.int_value = -int_val2;
								break;
						}
#ifdef DEBUG
if (exp_val.code == FPL_T) printf("exp_val.attribute.flt_value = %f\n", exp_val.attribute.flt_value);
else printf("exp_val.attribute.int_value = %d\n", exp_val.attribute.int_value);
#endif
					}			
					break;
				case REL_OP_T:
#ifdef DEBUG
printf("In REL_OP_T\n");
#endif
					exp_val.code = INL_T;
					if (op1->code == AVID_T && st_get_type(sym_table, op1->attribute.vid_offset) == 'F' || op1->code == FPL_T) {
						float flp_val = op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.fpl_val : op1->attribute.flt_value;
#ifdef DEBUG
printf("flp_val1 is %f and flp_val2 is %f\n", flp_val, (float)get_num_value(*op2));
#endif
						switch (tkn->attribute.rel_op) {
							case EQ:
								exp_val.attribute.int_value = flp_val == (float)get_num_value(*op2);
								break;
							case NE:
								exp_val.attribute.int_value = flp_val != (float)get_num_value(*op2);
								break;
							case GT:
								exp_val.attribute.int_value = flp_val >  (float)get_num_value(*op2);
								break;
							case LT:
								exp_val.attribute.int_value = flp_val <  (float)get_num_value(*op2);
								break;
						}
					} else if (op1->code == AVID_T || op1->code == INL_T) {
						int int_val = op1->code == AVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.int_val : op1->attribute.int_value;
#ifdef DEBUG
printf("int_val1 is %d and int_val2 is %d\n", int_val, (int)get_num_value(*op2));
#endif
						switch (tkn->attribute.rel_op) {
							case EQ:
								exp_val.attribute.int_value = int_val == (int)get_num_value(*op2);
								break;
							case NE:
								exp_val.attribute.int_value = int_val != (int)get_num_value(*op2);
								break;
							case GT:
								exp_val.attribute.int_value = int_val >  (int)get_num_value(*op2);
								break;
							case LT:
								exp_val.attribute.int_value = int_val <  (int)get_num_value(*op2);
								break;
						}
					} else {
						int str_offset1 = op1->code == SVID_T ? sym_table.pstvr[op1->attribute.vid_offset].i_value.str_offset : op1->attribute.str_offset;
						int str_offset2 = op2->code == SVID_T ? sym_table.pstvr[op2->attribute.vid_offset].i_value.str_offset : op2->attribute.str_offset;
						char *str1 = str_offset1 < 0 ? "" : b_setmark(str_LTBL, str_offset1);
						char *str2 = str_offset2 < 0 ? "" : b_setmark(str_LTBL, str_offset2);
						int ret = strcmp(str1, str2);
#ifdef DEBUG
printf("str1 is %s and str2 is %s return value is %d\n", b_setmark(str_LTBL, str_offset1), b_setmark(str_LTBL, str_offset2), ret);
#endif
						switch (tkn->attribute.rel_op) {
							case EQ:
								exp_val.attribute.int_value = ret == 0;
								break;
							case NE:
								exp_val.attribute.int_value = ret != 0;
								break;
							case GT:
								exp_val.attribute.int_value = ret > 0;
								break;
							case LT:
								exp_val.attribute.int_value = ret < 0;
								break;
						}
					}
					break;
				case LOG_OP_T:
#ifdef DEBUG
printf("In LOG_OP_T\n");
printf("Tkn->attribute.log_op is %d.\n", tkn->attribute.log_op);
printf("Tkn->attribute.log_op is %d: Op1 int value is %d and Op2 int value is %d.\n", tkn->attribute.log_op, op1->attribute.int_value, op2->attribute.int_value);
#endif
					exp_val.code = INL_T;
					if (tkn->attribute.log_op == AND)
						exp_val.attribute.int_value = (op1->attribute.int_value ? op2->attribute.int_value : op1->attribute.int_value);
					else 
						exp_val.attribute.int_value = (op1->attribute.int_value ? op1->attribute.int_value : op2->attribute.int_value);
					break;
			} /* END SWITCH TKN->CODE */
			s_push(exp_stck, &exp_val);
		} else if (tkn->code == KW_T) {
			if (tkn->attribute.kwt_idx == OUTPUT) {
				if (!l_hasnext(iterable)) printf("\n");
				else {
					/* iterate through remaining tokens */
					while (l_hasnext(iterable)) {
						tkn = (Token*)l_getnext(iterable);
						switch (tkn->code) {
							case AVID_T:
								if (st_get_type(sym_table, tkn->attribute.vid_offset) == 'F')
									printf(l_hasnext(iterable) ? "%f," : "%f", sym_table.pstvr[tkn->attribute.vid_offset].i_value.fpl_val);
								else printf(l_hasnext(iterable) ? "%d," : "%d", sym_table.pstvr[tkn->attribute.vid_offset].i_value.int_val);
								break;
							case SVID_T:
								printf(l_hasnext(iterable) == 1 ? "%s," : "%s", b_setmark(str_LTBL, sym_table.pstvr[tkn->attribute.vid_offset].i_value.str_offset));
								break;
							case STR_T:
								printf("%s", b_setmark(str_LTBL, tkn->attribute.str_offset));
								break;
						}
					}
				}
			} else if (tkn->attribute.kwt_idx == INPUT) {
				char ch;
				Token literal;
				Buffer* var_list = b_create(10, 10, 'a');
				while (((ch = getchar()) != '\n') & (b_addc(var_list, ch) != NULL)) ;
				while (l_hasnext(iterable)) {
					tkn = (Token*)l_getnext(iterable);
					do literal = mlwpar_next_token(var_list);
					while (literal.code == COM_T) ;
#ifdef DEBUG0
printf("literal code is %d\n", literal.code);
#endif
					if (literal.code == SEOF_T) {
						printf("***NOT ENOUGH ARGUMENTS RECEIVED***\n");
						l_reset_iterator(iterable);
						exp_val.code = ERR_T;
						strcpy(exp_val.attribute.err_lex, "RUN TIME ERROR");
						return exp_val;
					}
					if (tkn->code == AVID_T) {
						if (st_get_type(sym_table, tkn->attribute.vid_offset) == 'F') sym_table.pstvr[tkn->attribute.vid_offset].i_value.fpl_val = (float)get_num_value(literal);
						else sym_table.pstvr[tkn->attribute.vid_offset].i_value.int_val = (int)get_num_value(literal);
					} else if (tkn->code == SVID_T) {
						sym_table.pstvr[tkn->attribute.vid_offset].i_value.str_offset = literal.attribute.str_offset;
					}
				}
				b_destroy(var_list);
			}
			exp_val.code = INL_T;
			exp_val.attribute.int_value = 1;
			s_push(exp_stck, &exp_val);
		}
	} /* END EXPRESSION EVALUATION LOOP */
	l_reset_iterator(iterable);
	return *(Token*)s_pop(exp_stck);
}
/*******************************************************************************
Purpose: Concatenate two strings
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 30/12/2015
Called functions: [ b_size(), st_get_ivalue(), b_setmark(), b_addc() ]
Parameters: str1 is the first string, str2 is the second string to concatenate
Return value: the offset of the concatenated string stored on the str_LTBL
Algorithm: set the str_offset to the next available position on the str_LTBL,
		   get the offset to the first operand string value from the str_LTBL,
		   offset value of -1 represents an empty string, add each character to
		   start a new entry in the str_LTBL, repeat for the second operand and
		   return the offset to the new entry
*******************************************************************************/
static int concat_str(Token* str1, Token* str2) {
	char* str;
	int i, mark, str_offset;
	str_offset = b_size(str_LTBL);
	/* string one */
	mark = str1->code == SVID_T ? sym_table.pstvr[str1->attribute.vid_offset].i_value.str_offset : str1->attribute.str_offset;
	if (mark >= 0) {
		str = b_setmark(str_LTBL, mark);
		for (i=0; str[i]; ++i) b_addc(str_LTBL, str[i]);
	}
	/* string two */
	mark = str2->code == SVID_T ? sym_table.pstvr[str2->attribute.vid_offset].i_value.str_offset : str2->attribute.str_offset;
	if (mark >= 0) {
		str = b_setmark(str_LTBL, mark);
		for (i=0; str[i]; ++i) b_addc(str_LTBL, str[i]);
	}
	b_addc(str_LTBL, '\0');
	return str_offset;
}