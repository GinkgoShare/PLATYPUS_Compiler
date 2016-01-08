/*******************************************************************************
File name: parser.c
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022
Course: CST 8152 - Compilers, Lab Section : 012
Assignment: 4
Date: 11 December 2015
Professor: Sv. Ranev
Purpose: Implements a Recursive Descent Predictive Parser for PLATYPUS
Info: This program uses two main states of procedure. The err_states switches on
	  if a syntax error is found. Once this flag is switched on it cannot be
	  switched off again throughout the remainder of the parser. This flag 
	  prevents any expression evaluations or statement executions. Throughout
	  the program you will find an execution flag being turned on and of. This 
	  can be seen in statements that have conditional evaluations of
	  expressions. This stops say an ELSE clause from evaluating within a
	  selection statement if the preceding condition evaluates to true. A third
	  flag specifically for the assignment statement tells an evaluation of the
	  interior assignment expression if there is a chance for an update of the
	  left value's type.
*******************************************************************************/
#include "parser.h"

#define DEBUG
#undef DEBUG
#define DEBUG0
#undef DEBUG0
#define DEBUG1
#undef DEBUG1
#define DBG_PARSER
#undef DBG_PARSER

#define get_num_value(op) ((op).code == AVID_T && st_get_type(sym_table, (op).attribute.vid_offset) == 'F' ? \
						  sym_table.pstvr[(op).attribute.vid_offset].i_value.fpl_val : \
						  (op).code == AVID_T && st_get_type(sym_table, (op).attribute.vid_offset) == 'I' ? \
						  sym_table.pstvr[(op).attribute.vid_offset].i_value.int_val : \
						  (op).code == FPL_T ? (op).attribute.flt_value : (op).attribute.int_value)

#define print_ierr(str,list) {printf(str); \
			 				  while (b_getc(list) && !b_eob(list)) ; \
				 			  break;}


extern STD sym_table;								/* Symbol Table Descriptor */
extern Buffer * str_LTBL;							/* String literal table */
extern int line;									/* source code line numbers - defined in scanner.c */
extern char * kw_table[];							/* Keyword lookup table */
extern Token mlwpar_next_token(Buffer * sc_buf);	

static Buffer* sc_buf;								/* BufferDescriptor contains the source file */
static Token lookahead;								/* token used in matching sequences */
static Token exp_value;								/* token used for expression values */
static Stack* operators;							/* stack used to arrange operators into reverse polish notation */
static List* iterable;								/* a list used for expression evaluations and statement executions */
static List* reusable_tkns;							/* a list to keep a token list to be reused in loop iterations */

/* trackers, flags and states */
int synerrno, err_state;
int asgn_stmt_asys, unry_asys;
int save_tkns, reuse_tkns, execute;

/* static function declarations */
static void match(int, int);
static void syn_eh(int);
static void syn_printe(void);
static void gen_incode(char*);
static char get_exp_type(void);
static void execute_statement(List* iterable);
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
Author: Christopher Elliott
*/
void parser(Buffer * in_buf) {
	execute = 1;
	sc_buf = in_buf;
	err_state = save_tkns = reuse_tkns = 0;
	asgn_stmt_asys = unry_asys = 0;
	lookahead = mlwpar_next_token(sc_buf);
	iterable = l_create(10, 10, sizeof(Token));
	reusable_tkns = l_create(10, 10, sizeof(Token));
	operators = s_create(10, 10, sizeof(Token), 'a');
	program(); match(SEOF_T, NO_ATTR);
#ifdef DBG_PARSER
gen_incode("PLATY: Source file parsed");
#endif
	l_destroy(reusable_tkns);
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
Algorithm: If the save_tkns state is on the add lookahead to the reusable_tkns
		   list. Match the lookahead code to the arg1 code. If lookahead does 
		   not macth then call syn_eh(), otherwise switch on the lookahead code.
		   If we are not in an error state then procede as follows: operator 
		   tokens need to be checked for precedence before adding to the running
		   expression list; if a right parentheses is matched then transfer 
		   operators from stack to the running expression list until the 
		   matching left parentheses is found; others can just be added as they 
		   come. If we are in a reuse token state then get next lookahead from
		   the reusable_tkns list otherwise call scanner for next token.
*******************************************************************************/
static void match(int pr_token_code, int pr_token_attribute) {
	if (save_tkns) l_add(reusable_tkns, &lookahead);
	if (lookahead.code == pr_token_code) {
		if (lookahead.code == SEOF_T) return;
		switch(pr_token_code) {
			case KW_T:
				if (lookahead.attribute.kwt_idx == pr_token_attribute) {
					if (!err_state && (lookahead.attribute.kwt_idx == OUTPUT || lookahead.attribute.kwt_idx == INPUT))
						l_add(iterable, &lookahead);
					break;
				}
				syn_eh(pr_token_code); return;
			case LOG_OP_T: case REL_OP_T:
				if (pr_token_code == LOG_OP_T && lookahead.attribute.log_op == pr_token_attribute 
					|| pr_token_code == REL_OP_T && lookahead.attribute.rel_op == pr_token_attribute) {
					if (!err_state) {
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
					}
					break;
				}
				syn_eh(pr_token_code);
				return;
			case ART_OP_T:
				if (lookahead.attribute.arr_op == pr_token_attribute) {
					if (!err_state) {
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
					}
					break;
				}
				syn_eh(pr_token_code);
				return;
			case RPR_T: /* add operators to the rpn_exp until we reach the previous left parenthesis */
				if (!err_state) {
					while (!s_isempty(operators)) {
						Token* tkn = (Token*)s_pop(operators);
						if (tkn->code != LPR_T) l_add(iterable, tkn);
						else break;
					}
				}
				break;
			case SCC_OP_T:
				if (!err_state) {
					while (!s_isempty(operators)) {
						Token* tkn = (Token*)s_pop(operators);
						if (tkn->code == SCC_OP_T) l_add(iterable, tkn);
						else {
							s_push(operators, tkn);
							break;
						}
					}
					s_push(operators, &lookahead);
				}
				break;
			case AVID_T: case STR_T: case FPL_T: case INL_T: case SVID_T:
				if (!err_state) l_add(iterable, &lookahead);
				break;
			case ASS_OP_T: case LPR_T:
				/* this grammar cannot have nested assignments so it is safe to 
				assume we can push to the stack without meeting a condition of precedence */
				if (!err_state) s_push(operators, &lookahead);
				break;
		}
		if (reuse_tkns) {
			if (l_hasnext(reusable_tkns)) lookahead = *(Token*)l_getnext(reusable_tkns);
		} else if (!(lookahead = mlwpar_next_token(sc_buf)).code) { /* code equals ERR_T */
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
Author: Christopher Elliott, 040 570 022
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
	err_state = 1;
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
*   Authors: Christopher Elliott, 040 570 022
*/
static void program(void){
	match(KW_T,PLATYPUS); match(LBR_T,NO_ATTR); 
	opt_statements(); match(RBR_T,NO_ATTR);
#ifdef DBG_PARSER
gen_incode("PLATY: Program parsed");
#endif
}
/*
*	<opt statements> 		-> <statements> | e
*	FIRST(<opt statements)	=  { AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT), e }
*   Authors: Christopher Elliott, 040 570 022
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
	default:
#ifdef DBG_PARSER
gen_incode("PLATY: Opt_statements parsed");
#endif
		break;
	}
}
/*
*	<statements> 		-> <statement> <statements p>
*	FIRST(<statements>) =  { AVID, SVID, IF, USING, INPUT, OUTPUT }
*   Authors: Christopher Elliott, 040 570 022
*/
static void statements(void) {
	statement(); statements_p();
}
/*
*	<statements p> 			-> <statement> <statements p> | e
*	FIRST(<statements p>)	=  { AVID, SVID, IF, USING, INPUT, OUTPUT, e }
*   Authors: Christopher Elliott, 040 570 022
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
*   Authors: Christopher Elliott, 040 570 022
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
#ifdef DBG_PARSER
gen_incode("PLATY: Assignment statement parsed");
#endif
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
#ifdef DBG_PARSER
gen_incode("PLATY: Assignment expression (arithmetic) parsed");
#endif
	} else if (lookahead.code == SVID_T) {
		match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); string_expression();
#ifdef DBG_PARSER
gen_incode("PLATY: Assignment expression (string) parsed");
#endif
	} else {
		syn_printe();
	}
	/* unload remaining operators off stack to expression list */
	while (!s_isempty(operators)) l_add(iterable, s_pop(operators));
	if (execute && !err_state) exp_value = evaluate_expression(iterable);
}
/*
*	<selection statement>			-> IF ( <conditional expression> ) THEN <opt statments>
*									   ELSE { <opt statements> } ;
*	FIRST(<selection statement>)	=  { IF }
*   Authors: Christopher Elliott, 040 570 022
*/
static void selection_statement(void) {
	/* maintain the preceding global state of execution */
	int execute_chain = execute;
	match(KW_T, IF); match(LPR_T, NO_ATTR); conditional_expression(); 
	match(RPR_T, NO_ATTR); match(KW_T, THEN);
	/* if execute_chain is true then the preceding execute state was 
	true and expression value(exp_value) was set within conditional_expression() */
	if (execute_chain && !err_state) execute = exp_value.attribute.int_value;
	opt_statements(); match(KW_T, ELSE);
	if (execute_chain && !err_state) execute = !exp_value.attribute.int_value;
	match(LBR_T, NO_ATTR); opt_statements(); match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	/* maintain global state of execution */
	execute = execute_chain;
#ifdef DBG_PARSER
gen_incode("PLATY: IF statement parsed");
#endif
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
	/* persist the conditional_expression and 
	assignment_expression for repeated evaluations */
	List *cond_exp, *assgn_exp;
	int istrue, execute_chain = execute;
	match(KW_T, USING); match(LPR_T, NO_ATTR);
	/* evaluate the first assignment expression as normal */
	assignment_expression(); match(COM_T, NO_ATTR);
	/* evaluate the conditional expression as normal */
	conditional_expression();
	/* copy the current expression list to use later */
	cond_exp = l_copy(iterable);
	/* get evaluation of the conditional_expression for use within execution loop */
	istrue = (execute_chain && !err_state) ? exp_value.attribute.int_value : 0;
	match(COM_T, NO_ATTR);
	execute = 0; /* prevent execution of second assignment expression */
	assignment_expression();
	/* copy the current expression list to use later */
	assgn_exp = l_copy(iterable);
	execute = execute_chain; /* revert to global execution state */
	match(RPR_T, NO_ATTR); match(KW_T, REPEAT); match(LBR_T, NO_ATTR);
	if (!istrue || err_state) { /* parse but do not execute statement */
		execute = 0;
		opt_statements();
	} else { /* parse while executing statement */
		Token tkn_chain;
		/* save tokens through first execution of REPEATing statements */
		save_tkns = 1; opt_statements(); save_tkns = 0;
		reuse_tkns = 1; /* set reusing token state for match() */
		/* save current token to reuse after execution loop */
		tkn_chain = lookahead;
		/* HEAD */
		for (evaluate_expression(assgn_exp), istrue = evaluate_expression(cond_exp).attribute.int_value,
			lookahead = *(Token*)l_getnext(reusable_tkns); /* CONDITION */istrue ;
			evaluate_expression(assgn_exp), istrue = evaluate_expression(cond_exp).attribute.int_value,
			l_reset_iterator(reusable_tkns), lookahead = *(Token*)l_getnext(reusable_tkns)) {
			/* BODY */
			opt_statements();
		}
		l_reset(reusable_tkns);
		lookahead = tkn_chain;
		reuse_tkns = 0;
	}
	match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
#ifdef DBG_PARSER
gen_incode("PLATY: USING statement parsed");
#endif
	l_destroy(assgn_exp); l_destroy(cond_exp);
	/* maintain global state of execution */
	execute = execute_chain;
} /* END ITERATION_STATEMENT() */
/*
*	<input statement>			-> INPUT ( <variable list> ) ;
*	FIRST(<input statement>)	=  { INPUT }
*   Authors: Christopher Elliott, 040 570 022
*/
static void input_statement(void) {
	l_reset(iterable);
	s_reset(operators);
	match(KW_T, INPUT); match(LPR_T, NO_ATTR); variable_list(); match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
#ifdef DBG_PARSER
gen_incode("PLATY: INPUT statement parsed");
#endif
	/* evaluate expression */
	if (execute && !err_state) execute_statement(iterable);
}
/*
*	<variable list>			-> <variable identifier> <variable list p>
*	FIRST(<variable list>)	=  { AVID_T, SVID_T }
*   Authors: Christopher Elliott, 040 570 022
*/
static void variable_list(void) {
	variable_identifier(); variable_list_p();
#ifdef DBG_PARSER
gen_incode("PLATY: Variable list parsed");
#endif
}
/*
*	<variable list p>			-> , <variable identifier> <variable list p> | e
*	FIRST(<variable list p>)	=  { ,, e }
*   Authors: Christopher Elliott, 040 570 022
*/
static void variable_list_p(void) {
	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR); variable_identifier(); variable_list_p();
	}
}
/*
*	<variable identifier>			-> AVID_T | SVID_T
*	FIRST(<variable identifier>)	=  { AVID_T, SVID_T }
*   Authors: Christopher Elliott, 040 570 022
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
#ifdef DBG_PARSER
gen_incode("PLATY: OUTPUT statement parsed");
#endif
	/* evaluate expression */
	if (execute && !err_state) execute_statement(iterable);
}
/*
*	<output list>			-> <variable list> | STR_T | e
*	FIRST(<output list>)	=  { STR_T, AVID_T, SVID_T, e }
*   Authors: Christopher Elliott, 040 570 022
*/
static void output_list(void) {
	switch(lookahead.code) {
	case AVID_T: case SVID_T: variable_list(); return;
	case STR_T: match(STR_T, NO_ATTR); return;
	}
#ifdef DBG_PARSER
gen_incode("PLATY: Output list (empty) parsed");
#endif
}
/*
*	<arithmetic expression>			-> <unary arithmetic expression> | <additive arrithmetic expression>
*	FIRST(<arithmetic exression>)	=  { +, -, AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022
*/
static void arithmetic_expression(void) {
	if (lookahead.code == ART_OP_T && (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS)) unary_arithmetic_expression();
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T || lookahead.code == LPR_T) additive_arithmetic_expression();
	else { syn_printe(); return; }
#ifdef DBG_PARSER
gen_incode("PLATY: Arithmetic expression parsed");
#endif
}
/*
*	<unary arithmetic expression>			-> + <primary arithmetic expression> | - <primary arithmetic expression>
*	FIRST(<unary arithmetic expression>)	=  { +, - }
*   Authors: Christopher Elliott, 040 570 022
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
#ifdef DBG_PARSER
gen_incode("PLATY: Unary arithmetic expression parsed");
#endif
}
/*
*	<additive arithmetic expression>		-> <multiplicative arithmetic expression> <additive arithmetic expression p>
*	FIRST(<additive arithmetic expression>) =  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022
*/
static void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
}
/*
*	<additive arithmetic expression p>			-> + <multiplicative arithmetic expression> <additive arithmetic expression p>
*												|  - <multiplicative arithmetic expression> <additive arithmetic expression p>
*												|  e
*	FIRST(<additive arithmetic expression p>)	=  { +, -, e }
*   Authors: Christopher Elliott, 040 570 022
*/
static void additive_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == PLUS) {
		match(ART_OP_T, PLUS); multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
#ifdef DBG_PARSER
gen_incode("PLATY: Additive arithmetic expression parsed");
#endif
	} else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MINUS) {
		match(ART_OP_T, MINUS); multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
#ifdef DBG_PARSER
gen_incode("PLATY: Additive arithmetic expression parsed");
#endif
	}
}
/*
*	<multiplicative arithmetic expression>			-> <primary arithmetic expression> <multiplicative arithmetic expression p>
*	FIRST(<multiplicative arithmetic expression>)	=  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022
*/
static void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
}
/*
*	<multiplicative arithmetic expression p>		-> * <primary arithmetic expression> <multiplicative arithmetic expression p>
*													|  / <primary arithmetic expression> <multiplicative arithmetic expression p>
*													|  e
*	FIRST(<multiplicative arithmetic expression p>) =  { *, /, e }
*   Authors: Christopher Elliott, 040 570 022
*/
static void multiplicative_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MULT) {
		match(ART_OP_T, MULT); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
#ifdef DBG_PARSER
gen_incode("PLATY: Multiplicative arithmetic expression parsed");
#endif
	} else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == DIV) {
		match(ART_OP_T, DIV); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
#ifdef DBG_PARSER
gen_incode("PLATY: Multiplicative arithmetic expression parsed");
#endif
	}
}
/*
*	<primary arithmetic expression>			-> AVID_T | FPL_T | INL_T | ( <arithmetic expression> )
*	FIRST(<primary arithmetic expression>)	=  { AVID_T, FPL_T, INL_T, ( }
*   Authors: Christopher Elliott, 040 570 022
*/
static void primary_arithmetic_expression(void) {
	switch(lookahead.code) {
	case AVID_T: match(AVID_T, NO_ATTR); break;
	case FPL_T: match(FPL_T, NO_ATTR); break;
	case INL_T: match(INL_T, NO_ATTR); break;
	case LPR_T: match(LPR_T, NO_ATTR); arithmetic_expression(); match(RPR_T, NO_ATTR); break;
	default: syn_printe(); return;
	}
#ifdef DBG_PARSER
gen_incode("PLATY: Primary arithmetic expression parsed");
#endif
}
/*
*	<string expression>			-> <primary string expression> <string expression p>
*	FIRST(<string expression>)	=  { SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022
*/
static void string_expression(void) {
	primary_string_expression(); string_expression_p();
#ifdef DBG_PARSER
gen_incode("PLATY: String expression parsed");
#endif
}
/*
*	<string expression p>			-> # <primary string expression> <string expression p> | e
*	FIRST(<string expression p>)	=  { #, e }
*   Authors: Christopher Elliott, 040 570 022
*/
static void string_expression_p(void) {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR); primary_string_expression(); string_expression_p();
	}
}
/*
*	<primary string expression>			-> SVID_T | STR_T
*	FIRST(<primary string expression>)	=  { SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022
*/
static void primary_string_expression(void) {
	if (lookahead.code == SVID_T) match(SVID_T, NO_ATTR);
	else if (lookahead.code == STR_T) match(STR_T, NO_ATTR);
	else { syn_printe(); return; }
#ifdef DBG_PARSER
gen_incode("PLATY: Primary string expression parsed");
#endif
}
/*
*	<conditional expression>		-> <logical OR expression>
*	FIRST(<conditional expression>) =  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022
*/
static void conditional_expression(void) {
	l_reset(iterable);
	s_reset(operators);
	logical_or_expression();
#ifdef DBG_PARSER
gen_incode("PLATY: Conditional expression parsed");
#endif
	/* unload remaining operators off stack to expression list */
	while (!s_isempty(operators)) l_add(iterable, s_pop(operators));
	/* evaluate expression */
	if (execute && !err_state) exp_value = evaluate_expression(iterable);
}
/*
*	<logical OR expression>			-> <logical AND expression> <logical OR expression p>
*	FIRST(<logical OR expression>)	=  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022
*/
static void logical_or_expression(void) {
	logical_and_expression(); logical_or_expression_p();
}
/*
*	<logical OR expression p>			-> .OR. <logical AND expression> <logical OR expression p> | e
*	FIRST(<logical OR expression p>)	=  { .OR., e }
*   Authors: Christopher Elliott, 040 570 022
*/
static void logical_or_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == OR) {
		match(LOG_OP_T, OR); logical_and_expression(); logical_or_expression_p();
#ifdef DBG_PARSER
gen_incode("PLATY: Logical OR expression parsed");
#endif
	}
}
/*
*	<logical AND expression>		-> <relational expression> <logical AND expression p>
*	FIRST(<logical AND expression>) =  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022
*/
static void logical_and_expression(void) {
	relational_expression(); logical_and_expression_p();
}
/*
*	<logical AND expression p>			-> .AND. <relational expression> <logical AND expression p> | e
*	FIRST(<logical AND expression p>)	=  { .AND., e }
*   Authors: Christopher Elliott, 040 570 022
*/
static void logical_and_expression_p(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == AND) {
		match(LOG_OP_T, AND); relational_expression(); logical_and_expression_p();
#ifdef DBG_PARSER
gen_incode("PLATY: Logical AND expression parsed");
#endif
	}
}
/*
*	<relational expression>			-> <primary a_relational expression> <primary a_relational expression p>
*									|  <primary s_relational expression> <primary s_relational expression p>
*	FIRST(relational expression>)	=  { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*   Authors: Christopher Elliott, 040 570 022
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
#ifdef DBG_PARSER
gen_incode("PLATY: Relational expression parsed");
#endif
}
/*
*	<primary a_relational expression>			-> AVID_T | FPL_T | INL_T
*	FIRST(<primary a_relational expression>)	=  { AVID_T, FPL_T, INL_T }
*   Authors: Christopher Elliott, 040 570 022
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
#ifdef DBG_PARSER
gen_incode("PLATY: Primary a_relational expression parsed");
#endif
}
/*
*	<primary a_relational expression p> 		-> == <primary a_relational expression>
*												|  <> <primary a_relational expression>
*												|  >  <primary a_relational expression>
*												|  <  <primary a_relational expression>
*	FIRST(<primary a_relational expression p>)	=  { ==, <>, >, < }
*   Authors: Christopher Elliott, 040 570 022
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
*   Authors: Christopher Elliott, 040 570 022
*/
static void primary_s_relational_expression(void) {
	primary_string_expression();
#ifdef DBG_PARSER
gen_incode("PLATY: Primary s_relational expression parsed");
#endif
}
/*
*	<primary s_relational expression p> 		-> == <primary s_relational expression>
*												|  <> <primary s_relational expression>
*												|  >  <primary s_relational expression>
*												|  <  <primary s_relational expression>
*	FIRST(<primary s_relational expression p>)	=  { ==, <>, >, < }
*   Authors: Christopher Elliott, 040 570 022
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
Purpose: Currently executes the INPUT and OUTPUT statements.
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 07/01/2016
Called functions: [ 
	l_hasnext(), l_getnext(), st_get_type(), printf(), b_setmark(), b_create(),
	getchar(), b_addc(), b_getc(), b_eob(), atof(), b_getc_offset(), isdigit(),
	strlen(), atol(), b_size(),
]
Algorithm: get the first token in statement list, if it's a keyword then match
		   the attribute, iterate through remaining tokens and handle
		   appropriately. If output statement then print the token value
		   formatted to match the type. If it is an input statement, get input
		   from user while formatting the string to follow the format of
		   literal\0literal\0literal\0...Parse each value in order using the
		   entire string value of each literal.
*******************************************************************************/
static void execute_statement(List* iterable) {
	Token *tkn;
	/* STATEMENT EXECUTION */
	tkn = (Token*)l_getnext(iterable);
	if (tkn->code == KW_T) {
		if (tkn->attribute.kwt_idx == OUTPUT) {
			if (!l_hasnext(iterable)) printf("\n"); /* OUTPUT(); no args == new line */
			else {
				/* iterate through arguments */
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
			char ch, str_type = 0;
			Buffer* var_list = b_create(10, 10, 'a');
			do {
				ch = getchar();
				if (!str_type) {
					/* skip spaces outside of strings */
					if (ch == ' ') continue;
					/* marks beginning of string */
					if (!isdigit(ch)) str_type = 1;
				}
				if (ch == ',' || ch == '\n') {
					/* marks end of string */
					if (str_type) str_type = 0;
					b_addc(var_list, '\0');
				} else b_addc(var_list, ch);
			} while (ch != '\n');
			while (l_hasnext(iterable)) {
				tkn = (Token*)l_getnext(iterable);
				/* trim leading '\0' */
				while (!(ch = b_getc(var_list)) && !b_eob(var_list)) ;
				if (b_eob(var_list))
					print_ierr("***NOT ENOUGH ARGUMENTS PROVIDED***\n", var_list);
				if (tkn->code == AVID_T) {
					if (st_get_type(sym_table, tkn->attribute.vid_offset) == 'F') {
						/* -1 necessary because of previous call to b_getc() line 4 lines previous */
						float value = atof(b_setmark(var_list, b_getc_offset(var_list)-1));
						if ((value < MIN_FSZ || value > MAX_FSZ) && value != 0.0)
							print_ierr("***ARGUMENT VALUE IS NOT IN RANGE***\n", var_list);
						sym_table.pstvr[tkn->attribute.vid_offset].i_value.fpl_val = value;
					} else {
						int value;
						char* str = b_setmark(var_list, b_getc_offset(var_list)-1);
						if (!isdigit(str[0]) || strlen(str) > INL_LEN)
							print_ierr("***ARGUMENT MUST BE AN INTEGER***\n", var_list);
						value = atol(str);
						if (value < MIN_ISZ || value > MAX_ISZ) 
							print_ierr("***ARGUMENT VALUE IS NOT IN RANGE***\n", var_list);
						sym_table.pstvr[tkn->attribute.vid_offset].i_value.int_val = value;
					}
					while (b_getc(var_list)) ;
				} else if (tkn->code == SVID_T) {
					sym_table.pstvr[tkn->attribute.vid_offset].i_value.str_offset = b_size(str_LTBL);
					b_addc(str_LTBL, ch);
					while ((ch = b_getc(var_list)) != '\0' && !b_eob(var_list)) b_addc(str_LTBL, ch);
					if (!ch) b_addc(str_LTBL, ch);
				}
			}
			b_destroy(var_list);
		}
	} /* END STATEMENT EXECUTION */
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
Algorithm: Create a new stack for the evaluation process. Iterate over the 
		   iterable. If an operand is encountered push it on to the stack. If an
		   operator is encountered then pop two operands of the stack, evaluate 
		   the expression and push the value back on to the stack.
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
								exp_val.attribute.int_value = rval.int_val = (short)get_num_value(*op2);
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
								else exp_val.attribute.int_value = (short)(int_val1 + int_val2);
								break;
							case MINUS:
#ifdef DEBUG
printf("In MINUS\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = flp_val1 - flp_val2;
								else exp_val.attribute.int_value = (short)(int_val1 - int_val2);
								break;
							case MULT:
#ifdef DEBUG
printf("In MULT\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = flp_val1 * flp_val2;
								else exp_val.attribute.int_value = (short)(int_val1 * int_val2);
								break;
							case DIV:
#ifdef DEBUG
printf("In DIV\n");
#endif
								if (exp_val.code == FPL_T) exp_val.attribute.flt_value = flp_val1 / flp_val2;
								else exp_val.attribute.int_value = (short)(int_val1 / int_val2);
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