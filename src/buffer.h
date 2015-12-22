/*******************************************************************************
File name: buffer.h
Compiler: MS Visual Studio 2013
Author: Christopher Elliott, 040 570 022
Course: CST 8152 - Compilers, Lab Section : 012
Assignment: 1
Date: 15/09/2015 (DD/MM/YYYY)
Professor: Sv. Ranev
Purpose: Implement the buffer data structure which will be used to store
		 collections of char.
Function list: [ 
	b_create(), b_addc(), b_reset(), b_destroy(), b_isfull(), 
	b_size(), b_capacity(), b_setmark(), b_mark(), b_mode(), 
	b_inc_factor(), b_load(), b_isempty(), b_eob(), b_getc(), 
	b_print(), b_pack(), b_rflag(), b_retract(), 
	b_retract_to_mark(), b_getc_offset() 
]
 ******************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_

/*to enforce C89 type comments  - to make //comments a warning */
/*#pragma warning(1:4001) */

/* to enforce C89 comments - to make // comments an error */
/*#pragma warning(error:4001)*/

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
/* You may add your own constant definitions here */
#define R_FAIL_1 -1         /* fail return value */
#define R_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2		/* load fail error */
#define SET_R_FLAG 1		/* realloc flag set value */
#define SET_EOB_1 1			/* eob flag set value */
#define I_FCTR_FAIL 256		/* failure value for b_inc_factor() */
#define MLT_RANGE_MIN 1		/* minimum range for multiplicative mode */
#define MLT_RANGE_MAX 100	/* maximum range for multiplicative mode */
#define MULTIPLICATIVE -1	/* multiplicative mode set value */
#define FIXED 0				/* fixed mode set value */
#define ADDITIVE 1			/* additive mode set value */

/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;		/* pointer to the beginning of character array (character buffer) */
	short capacity;		/* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short mark_offset;	/* the offset (in chars) to the mark location */
	char  inc_factor;	/* character array increment factor */
	char  r_flag;		/* character array reallocation flag */
	char  mode;			/* operational mode indicator */
	int   eob;			/* end-of-buffer reached flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer * b_create(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_reset(Buffer * const pBD);
void b_destroy(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_size(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
char * b_setmark(Buffer * const pBD, short mark);
short b_mark(Buffer * const pBD);
int b_mode(Buffer * const pBD);
size_t  b_inc_factor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer  * const pBD);
Buffer *b_pack(Buffer * const pBD);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_retract_to_mark(Buffer * const pBD);
short b_getc_offset(Buffer * const pBD);


#define INLINE
/*#undef INLINE*/

#ifdef INLINE
#define b_isfull(pBD) (((Buffer *)pBD) == NULL) ? R_FAIL_1 : (((short)(((Buffer *)pBD)->addc_offset * sizeof(char))) == ((Buffer *)pBD)->capacity)
#define b_size(pBD) ((((Buffer *)pBD) == NULL) ? (short)R_FAIL_1 : ((Buffer *)pBD)->addc_offset)
#define b_capacity(pBD) ((((Buffer *)pBD) == NULL) ? R_FAIL_1 : ((Buffer *)pBD)->capacity)
#define b_mark(pBD) ((((Buffer *)pBD) == NULL) ? R_FAIL_1 : ((Buffer *)pBD)->mark_offset)
#define b_mode(pBD) ((((Buffer *)pBD) == NULL) ? R_FAIL_2 : ((Buffer *)pBD)->mode)
#define b_inc_factor(pBD) ((((Buffer *)pBD) == NULL) ? I_FCTR_FAIL : (unsigned char)((Buffer *)pBD)->inc_factor)
#define b_isempty(pBD) ((((Buffer *)pBD) == NULL) ? R_FAIL_1 : (((Buffer *)pBD)->addc_offset == 0))
#define b_eob(pBD) ((((Buffer *)pBD) == NULL) ? R_FAIL_1 : ((Buffer *)pBD)->eob)
#define b_rflag(pBD) ((((Buffer *)pBD) == NULL) ? R_FAIL_1 : ((Buffer *)pBD)->r_flag)
#define b_retract(pBD) ((((Buffer *)pBD) == NULL || ((Buffer *)pBD)->getc_offset == 0) ? R_FAIL_1 : --(((Buffer *)pBD)->getc_offset))
#define b_getc_offset(pBD) ((((Buffer *)pBD) == NULL) ? R_FAIL_1 : ((Buffer *)pBD)->getc_offset)
#endif

#endif
