/*******************************************************************************
File name: scanner.c
Compiler: MS Visual Studio 2012
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
Course: CST 8152 - Compilers, Lab Section : 012
Assignment: 2
Date: 27 October 2015
Professor: Sv. Ranev
Purpose: Implement the lexical analyzer (scanner) that reads a source program and 
         produces a stream of tokens.
Function list: scanner_init(), mlwpar_next_token(), get_next_state(), char_class(),
			   aa_func02(), aa_func03(), aa_func05(), aa_func08(), aa_func10(),
			   aa_func12(), atool(), iskeyword(), run_time_err(), set_err_t()
*******************************************************************************/

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

#define HT 9	/* ASCII code for horizontal tab */
#define LF 10	/* ASCII code for line feed */
#define VT 11	/* ASCII code for vertical tab */
#define FF 12	/* ASCII code for form feed */
#define CR 13	/* ASCII code for carriage return */
#define SP 32	/* ASCII code for space */

/* counts new line char */
#define n_line(c, buffer) if (c == CR && b_getc(buffer) != LF) { b_retract(buffer); } ++line;

/* runs on a symbol table error */
#define sym_tbl_err(lexeme) printf("\nError: The Symbol Table is full - install failed.\n"); \
							if (lexeme) free(lexeme); \
							st_store(sym_table); \
							exit(EXIT_FAILURE);

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL;	/*String literal table */
extern int scerrnum;		/* defined in platy_st.c - run-time error number */
extern STD sym_table;		/* Symbol Table Descriptor */

int line;					/* current line number of the source code */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

/* function declarations */
static Token run_time_err(void);
static Token set_err_t(unsigned int, Buffer*);

int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*******************************************************************************
Purpose: Performs token recognition by reading the lexeme from the input buffer and
		 returning a token structure for the pattern that it finds
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
History/Versions: 1.0 / 27 October 2015
Called functions:
Parameters: sc_buf - type: Buffer *
Return value: returns the Token
Algorithm: 
*******************************************************************************/
Token mlwpar_next_token(Buffer * sc_buf) {
	Token t;			/* token to return after recognition */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	short lexstart;		/* start offset of a lexeme in the input buffer */
	short lexend;		/* end   offset of a lexeme in the input buffer */
	int accept = NOAS;	/* type of state - initially not accepting */

	for (;;) {
		b_setmark(sc_buf, b_getc_offset(sc_buf));
		c = b_getc(sc_buf);
		switch (c) {
		case '\0': case 0xFF:
			t.code = SEOF_T;
			return t;
		case LF: case CR:
			n_line(c, sc_buf);
			continue;
		case HT: case VT: case FF: case SP:
			continue;
		case '(':
			t.code = LPR_T;
			return t;
		case ')':
			t.code = RPR_T;
			return t;
		case '{':
			t.code = LBR_T;
			return t;
		case '}':
			t.code = RBR_T;
			return t;
		case ',':
			t.code = COM_T;
			return t;
		case ';':
			t.code = EOS_T;
			return t;
		case '#':
			t.code = SCC_OP_T;
			return t;
		case '=':
			if (b_getc(sc_buf) == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		case '<':
			t.code = REL_OP_T;
			if (b_getc(sc_buf) == '>') {
				t.attribute.rel_op = NE;
				return t;
			}
			b_retract(sc_buf);
			t.attribute.rel_op = LT;
			return t;
		case '.':
			t.code = LOG_OP_T;
			if (b_getc(sc_buf) == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.') {
				t.attribute.log_op = AND;
				return t;
			}
			b_retract_to_mark(sc_buf);
			if (b_getc(sc_buf) == '.' && b_getc(sc_buf) == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.') {
				t.attribute.log_op = OR;
				return t;
			}
			return set_err_t(1, sc_buf);
		case '!':
			/* check next input character */
			if ((c = b_getc(sc_buf)) == '<') {
				while ((c = b_getc(sc_buf)) != CR && c != LF && c != '\0' && c != 0xFF) ;
				if (c == CR || c == LF) { /* if new line is found then this is legal comment */
					n_line(c, sc_buf);
					continue; 
				}
			}
			/* else set error token for illegal comment */
			t = set_err_t(2, sc_buf);
			while ((c = b_getc(sc_buf)) != CR && c != LF && c != '\0' && c != 0xFF) ; /* skip remaining characters */
			if (c == CR || c == LF) { n_line(c, sc_buf); }							  /* while counting lines */
			return t;
		case '"':
			/* loop through input characters until closing '"' or SEOF is found */
			for (c = b_getc(sc_buf); c != '"' && c != '\0' && c != 0xFF; c = b_getc(sc_buf))
				if (c == CR || c == LF) { n_line(c, sc_buf); } /* count new lines */
			if (c == '"') { /* if paring '"' is found then successful string is found */
				t.code = STR_T;
				b_retract_to_mark(sc_buf);
				t.attribute.str_offset = b_size(str_LTBL); /* mark where this string can be found in str_LTBL */
				b_getc(sc_buf); /* skip past the quotation mark */
				for (c = b_getc(sc_buf); c != '"'; b_addc(str_LTBL, c), c = b_getc(sc_buf)) ;
				if (!b_addc(str_LTBL, '\0')) { return run_time_err(); } /* terminate string with '\0' */
				return t;
			}
			/* if no closing '"' is found then it is an error */
			t = set_err_t(ERR_LEN, sc_buf);
			for (c = b_getc(sc_buf); c != '\0' && c != 0xFF ; c = b_getc(sc_buf)) ; /* skip chars to SEOF */
			return t;
		default:
			if (c >= '0' && c <= '9' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z') {
				/* cycle states until we find an accepting state */
				for (state = get_next_state(state, c, &accept); !accept; state = get_next_state(state, b_getc(sc_buf), &accept)) ;
				if (accept == ASWR) { b_retract(sc_buf); }
				lexstart = b_mark(sc_buf); /* get length of vid */
				lexend = b_getc_offset(sc_buf);
				lex_buf = b_create((lexend - lexstart), 0, 'f'); /* get buffer to store lexeme */
				if (!lex_buf) { return run_time_err(); }
				b_retract_to_mark(sc_buf);
				for ( ; b_getc_offset(sc_buf) < lexend; b_addc(lex_buf, b_getc(sc_buf))) ; /* add characters to lexeme buffer */
				/* trim lexeme, terminate with '\0' and check for NULL before sending to function */
				if (!b_pack(lex_buf) || !b_addc(lex_buf, '\0')) { return run_time_err(); }
				t = aa_table[state](b_setmark(lex_buf, 0)); /* set token attributes */
				b_destroy(lex_buf);
				return t; 
			}
			return set_err_t( 1, sc_buf);
		}
	}
}


static int get_next_state(int state, char c, int *accept) {
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*******************************************************************************
Purpose: Takes an input character and returns the column index for the 
		 corresponding column in the transition table for that character
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 27 October 2015
Parameters: c - type: char
Return value: int - column index in the range of 0 to 6
*******************************************************************************/
static int char_class (char c) {
	if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) return 0;
	if (c == '0') return 1;
	if (c >= '1' && c <= '7') return 2;
	if (c == '8' || c == '9') return 3;
	if (c == '.') return 4;
	if (c == '%') return 5;
	return 6;
}

/*******************************************************************************
Purpose: Accepting function for the arithmetic variable identifier
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 27 October 2015
Called functions: iskeyword()
Parameters: lexeme array of characters
Return value: returns the Token
Algorithm: If lexeme is a keyword, then return token with the corresponding 
		   attribute, else set AVID token, store chars into vid_lex array, and 
		   append '\0'. 
*******************************************************************************/
Token aa_func02(char lexeme[]) {
	int i;		/* utility */
	Token t;    /* token to return */
	char vid_lex[VID_LEN+1];
	i = iskeyword(lexeme);
	if (i == KWT_SIZE) {
		t.code = AVID_T;
		for (i = 0; i < VID_LEN && lexeme[i]; i++) vid_lex[i] = lexeme[i];
		vid_lex[i] = '\0';
		switch(lexeme[0]) {
		case 'i': case 'o': case 'd': case 'w':
			if ((t.attribute.vid_offset = st_install(sym_table, vid_lex, 'I', line)) < 0) { sym_tbl_err(lexeme) }
			return t;
		default:
			if ((t.attribute.vid_offset = st_install(sym_table, vid_lex, 'F', line)) < 0) { sym_tbl_err(lexeme) }
			return t;
		}
	}
	t.code = KW_T;
	t.attribute.kwt_idx = i;
	return t;
}

/*******************************************************************************
Purpose: Accepting function for the string variable identifier
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 27 October 2015
Called functions:
Parameters: lexeme array of characters
Return value: returns the Token
Algorithm: Set SVID token. Store only the first VID_LEN - 1 chars into vid_lex 
		   array. Add % to the name, add \0 to the end and then return token.
*******************************************************************************/
Token aa_func03(char lexeme[]) {
	int i;		/* counter */
	Token t;	/* token to return */
	char vid_lex[VID_LEN+1];
	t.code = SVID_T;
	/* store lexeme in vid_lex array */
	for (i = 0; i < VID_LEN && lexeme[i]; i++) vid_lex[i] = (i < (VID_LEN-1)) ? lexeme[i] : '%';
	vid_lex[i] = '\0';
	if ((t.attribute.vid_offset = st_install(sym_table, vid_lex, 'S', line)) < 0) { sym_tbl_err(lexeme) }
	return t;
}

/*******************************************************************************
Purpose: Accepting function for the integer literal decimal constant (DIL) and 
		 zero (0)
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 27 October 2015
Called functions: atol(), aa_func12(), strlen()
Parameters: lexeme array of characters
Return value: returns the Token (error token if value out of range)
Algorithm: Convert the lexeme into a decimal integer value. If the integer is 
		   out of range of a 2-byte int, set an error token. Return the token.
*******************************************************************************/
Token aa_func05(char lexeme[]) {
	Token t;	/* token to return */
	long val;	/* integer value */
	if (strlen(lexeme) > INL_LEN) { return aa_func12(lexeme); }
	val = atol(lexeme);
	/* if int out of range, set error token, otherwise store value in the attribute */
	if (val < SHRT_MIN || val > SHRT_MAX) { return aa_func12(lexeme); }
	t.code = INL_T;
	t.attribute.int_value = (int)val;
	return t;
}

/*******************************************************************************
Purpose: Accepting function for the floating-point literal (FPL)
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 27 October 2015
Called functions: atof(), aa_func12()
Parameters: lexeme array of characters
Return value: returns the Token (error token if value out of range)
Algorithm: Convert the lexeme into a floating point value. If the value is out 
		   of range of a 4-byte float, set an error token. Return the token.
*******************************************************************************/
Token aa_func08(char lexeme[]) {
	Token t; /* token to return */
	double val; /* floating-point value */
	val = atof(lexeme);
	/* if float out of range, set error token, otherwise store value in the attribute */
	if ((val < FLT_MIN || val > FLT_MAX) && val != 0.0) { return aa_func12(lexeme); }
	t.code = FPL_T;
	t.attribute.flt_value = (float)val;
	return t;
}

/*******************************************************************************
Purpose: Accepting function for the integer literal(IL) - octal constant (OIL)
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 27 October 2015
Called functions: atool(), aa_func12()
Parameters: lexeme array of characters
Return value: returns the Token (error token if value out of range)
Algorithm: Convert the lexeme into a decimal integer value for an octal 
		   constant. If the value is out of range of a 2-byte int, set an error 
		   token. Return the token.
*******************************************************************************/
Token aa_func10(char lexeme[]) {
	Token t; 	/* token to return */
	long val;	/* decimal integer value*/
	if (strlen(lexeme) > (INL_LEN+1)) { return aa_func12(lexeme); }
	val = atool(lexeme);
	/* if int out of range, set error token, otherwise store value in the attribute */
	if (val < SHRT_MIN || val > SHRT_MAX) { return aa_func12(lexeme); }
	t.code = INL_T;
	t.attribute.int_value = (int)val;
	return t;
}

/*******************************************************************************
Purpose: Accepting function for the error token
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 27 October 2015
Called functions:
Parameters: lexeme array of characters
Return value: returns the Token
Algorithm: Set the error token. Store only the first ERR_LEN chars into err_lex 
		   array. Return the token.
*******************************************************************************/
Token aa_func12(char lexeme[]){
	int i;   /* utility */
	Token t; /* token to return */
	t.code = ERR_T; 
	for (i = 0; i < ERR_LEN && lexeme[i]; t.attribute.err_lex[i] = lexeme[i++]) ;
	t.attribute.err_lex[i] = '\0';
	return t;
}
 
/*******************************************************************************
Purpose: Converts the ASCII string that represents an octal integer constant into an
         decimal integer value.
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 27 October 2015
Called functions: strtol()
Parameters: char pointer to a lexeme string
Return value: long
*******************************************************************************/
static long atool(char * lexeme) {
	return strtol(lexeme, NULL, 8); /* convert string to int value of base 8 */
}
 
/*******************************************************************************
Purpose: Performs token recognition by reading the lexeme from the input buffer and
returning a token structure for the pattern that it finds
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 27 October 2015
Called functions: strcmp()
Parameters: kw_lexeme - type: char *
Return value: int - index of the keyword if found. Otherwise returns KWT_SIZE.
*******************************************************************************/
static int iskeyword(char * kw_lexeme){
	int i; /* utility */
	for (i = 0; i < KWT_SIZE && strcmp(kw_table[i], kw_lexeme); ++i) ;
	return i;
}


/*******************************************************************************
Purpose: Returns the run-time error token 
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 27 October 2015
Called functions: strcpy()
Return value: run-time error token
*******************************************************************************/
static Token run_time_err(void) {
	Token t;  /* token to return */
	t.code = ERR_T;
	strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
	scerrnum = scerrnum == 0 ? 1 : scerrnum << 1;
	return t;
}


/*******************************************************************************
Purpose: Sets the error token for the given lexeme
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 27 October 2015
Called functions: run_time_err(), b_retract_to_mark(), b_getc(), b_retract()
Parameters:lex_len - length of the lexeme, 
Return value: error Token 
*******************************************************************************/
static Token set_err_t(unsigned int lex_len, Buffer* buffer) {
	unsigned char c;
	Token t;
	unsigned int i;
	if (buffer == NULL) { return run_time_err(); }
	t.code = ERR_T;
	b_retract_to_mark(buffer);
	for (i = 0, c = (unsigned char)b_getc(buffer); i < lex_len && c != '\0' && c != 0xFF;
		++i, c = (unsigned char)b_getc(buffer))
		t.attribute.err_lex[i] = (i < ERR_LEN-3) ? c : '.';
	if (i == lex_len) { b_retract(buffer); }
	t.attribute.err_lex[i] = '\0';
	return t;
}