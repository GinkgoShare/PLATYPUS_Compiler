#include <stdio.h>
#include <stdlib.h> /* Constants for calls to exit()*/

#include <string.h>
#include <stdarg.h>

#include "buffer.h"
#include "token.h"
#include "stable.h"

/* Input buffer parameters */
#define INIT_CAPACITY 200 /* initial buffer capacity */
#define INC_FACTOR 15       /* increment factor */
/* String Literal Table parameters */
#define STR_INIT_CAPACITY 100 /* initial string literal table capacity */
#define STR_CAPACITY_INC  50   /* initial string literal table capacity inc */

/* Symbol Table default size */
#define ST_DEF_SIZE 100 

/*check for ANSI C compliancy */
#define ANSI_C 0
#if defined(__STDC__)
#undef ANSI_C
#define ANSI_C 1
#endif

/* Global objects - variables */
static Buffer *sc_buf; /* pointer to input (source) buffer */
Buffer * str_LTBL; /* this buffer implements String Literal Table */
                  /* it is used as a repository for string literals */
int scerrnum;     /* run-time error number = 0 by default (ANSI) */
STD sym_table;    /* Symbol Table Descriptor */