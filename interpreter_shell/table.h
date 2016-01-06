/*******************************************************************************
File name: table.h
Compiler: MS Visual Studio 2012
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
Course: CST 8152 - Compilers, Lab Section : 012
Assignment: 2
Date: 27 October 2015
Professor: Sv. Ranev
Purpose: Declares and initializes the transition tables for the scanner. 
Also contains the function declarations for the accepting functions.
Function list: 
*******************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or only one of the folowing constants: 255, 0xFF , EOF
 */

/*  Single-lexeme tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
 *       space
 *  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', # ,
 *  .AND., .OR. , SEOF, 'wrong symbol',
 */
 

#define ES 12	/* Error state */
#define IS -1   /* Inavalid state */

/* State transition table definition */


#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
   /* a-Z  0  1-7 8-9  .  % other */
	{  1,  6,  4,  4, IS, IS, IS },/* 0 */
	{  1,  1,  1,  1,  2,  3,  2 },/* 1 */
	{ IS, IS, IS, IS, IS, IS, IS },/* 2 */
	{ IS, IS, IS, IS, IS, IS, IS },/* 3 */
	{ ES,  4,  4,  4,  7,  5,  5 },/* 4 */
	{ IS, IS, IS, IS, IS, IS, IS },/* 5 */
	{ ES, ES,  9, 12,  7, ES,  5 },/* 6 */
	{  8,  7,  7,  7,  8,  8,  8 },/* 7 */
	{ IS, IS, IS, IS, IS, IS, IS },/* 8 */
	{ ES,  9,  9, ES, ES, ES, 10 },/* 9 */
	{ IS, IS, IS, IS, IS, IS, IS },/* 10 */
	{ IS, IS, IS, IS, IS, IS, IS },/* 11 */
	{ IS, IS, IS, IS, IS, IS, IS },/* 12 */
	{ IS, IS, IS, IS, IS, IS, IS },/* 13 */
};
 
/* Accepting state table definition */
#define ASWR -1 /* accepting state with retract */
#define ASNR 1  /* accepting state with no retract */
#define NOAS 0  /* not accepting state */

int as_table[ ] = {
	NOAS, /* 0 */
	NOAS, /* 1 */
	ASWR, /* 2 */
	ASNR, /* 3 */
	NOAS, /* 4 */
	ASWR, /* 5 */
	NOAS, /* 6 */
	NOAS, /* 7 */
	ASWR, /* 8 */
	NOAS, /* 9 */
	ASWR, /* 10 */
	NOAS, /* 11 */
	ASNR, /* 12 */
	ASWR  /* 13 */
};

/* Accepting action function declarations */
Token aa_func02(char *lexeme); /* AVID/KW */
Token aa_func03(char *lexeme); /* SVID */
Token aa_func05(char *lexeme); /* DIL */
Token aa_func08(char *lexeme); /* FPL */
Token aa_func10(char *lexeme); /* OIL */
Token aa_func12(char *lexeme); /* ES */

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[ ] ={
	NULL,      /* 0 */
	NULL,      /* 1 */
	aa_func02, /* 2 */
	aa_func03, /* 3 */
	NULL,      /* 4 */
	aa_func05, /* 5 */
	NULL,      /* 6 */
	NULL,      /* 7 */
	aa_func08, /* 8 */
	NULL,      /* 9 */
	aa_func10, /* 10 */
	NULL,      /* 11 */
	aa_func12, /* 12 */
	NULL  /* 13 */
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table []= {
                      "ELSE",
                      "IF",
                      "INPUT",
                      "OUTPUT",
                      "PLATYPUS",
                      "REPEAT",
                      "THEN",
                      "USING"   
                     };

#endif
                     