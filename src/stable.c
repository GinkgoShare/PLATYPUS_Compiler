/*******************************************************************************
File name: stable.c
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
Course: CST 8152 - Compilers, Lab Section : 012
Assignment: 3
Date: 20 November 2015
Professor: Sv. Ranev
Purpose:  Implements a symbol table which consists of a Symbol Table Manager
          and a Symbol Table Database.
Function list: st_create(), st_install(), st_lookup(), st_update_type(),
               st_update_type(), st_update_value(), st_get_type(), st_destroy(),
               st_print(), st_setsize(void), st_incoffset(void), st_store(),
               st_sort(), cmp_asc(), cmp_dsc(), buffer_sort()
*******************************************************************************/
#include <stdlib.h>
#include <string.h>
#include "stable.h"

#define DEBUG
/*#undef DEBUG*/

extern STD sym_table; /* global sym_table variable */

static void st_setsize(void);
static void st_incoffset(void);
static int cmp_asc(const void *a, const void *b);
static int cmp_dsc(const void *a, const void *b);
static int buffer_sort(void);

/*******************************************************************************
Purpose: Creates a new empty symbol table
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: malloc()
Parameters: int st_size - number of elements
Return value: STD structure
Algorithm: create and allocate memory for a new symbol table
           create and initialize self-incrementing buffer
           if operation successful, update st_size and offset, return STD
*******************************************************************************/
STD st_create(int st_size) {
	STD sym_table; /* new empty symbol table */
	sym_table.st_size = 0;
	if (st_size <= NVLD_SZ) return sym_table;

	sym_table.pstvr = (STVR *)malloc(sizeof(STVR) * st_size);
	if (!sym_table.pstvr) return sym_table;

	sym_table.plsBD = b_create(INIT_SZ, INC_FCTR, 'a'); /* initialize buffer */
	if (!sym_table.plsBD) { free(sym_table.pstvr); return sym_table; }

	sym_table.st_offset = 0;
	sym_table.st_size = st_size;	
	return sym_table;
}

/*******************************************************************************
Purpose: Installs a new VID record into the symbol table
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 20 November 2015
Called functions: st_lookup(), b_setmark(), strlen(), b_addc(), b_size(),
                  b_rflag(), b_mark(), st_incoffset()
Parameters: STD sym_table, char *lexeme, char type, int line
Return value: int offset of the entry or -1 if sym_table is full
Algorithm: lookup the lexeme in the symbol table,
           if no lexeme found:
				install new entry at st_offset,
				set the data type indicator and initial value,
				add the lexeme to the sym_table buffer descriptor,
				while adding to the symbol table buffer descriptor check for
				reallocation and realign STVR plex members when needed,
				increment offset of the global sym_table,
		   if lexeme found:
				return the current offset of the record
*******************************************************************************/
int st_install(STD sym_table, char *lexeme, char type, int line) {
	STVR record;
	int i, j, len;
	if (!sym_table.st_size || sym_table.st_offset == sym_table.st_size) return R_FAIL_1;
	if ((i = st_lookup(sym_table, lexeme)) == R_FAIL_1) { /* make sure entry does not exist */
		record.o_line = line; record.status_field = DEFAULT; record.plex = b_setmark(sym_table.plsBD, b_size(sym_table.plsBD)); /* init */
		switch (type) {
		case 'I':
			record.i_value.int_val = 0;
			record.status_field = record.status_field | ITYPE;
			break;
		case 'F':
			record.i_value.fpl_val = 0.0F;
			record.status_field = record.status_field | FTYPE;
			break;
		case 'S':
			record.i_value.str_offset = EPSILON;
			record.status_field = record.status_field | STYPE;
			break;
		default:
			return R_FAIL_1;
		}
		for (i = 0, len = (int)strlen(lexeme); i <= len && b_addc(sym_table.plsBD, lexeme[i]); i++) { /* add lexeme to buffer descriptor member */
			if (b_rflag(sym_table.plsBD)) {
				/* use b_setmark() to set the mark of the buffer and return the char * to be assigned to plex and for strlen() */
				for (b_setmark(sym_table.plsBD, 0), j = 0; j < sym_table.st_offset; sym_table.pstvr[j].plex = b_setmark(sym_table.plsBD, b_mark(sym_table.plsBD)),
					b_setmark(sym_table.plsBD, (short)(b_mark(sym_table.plsBD) + strlen(b_setmark(sym_table.plsBD, b_mark(sym_table.plsBD))) + 1)), ++j) ;
				record.plex = b_setmark(sym_table.plsBD, b_mark(sym_table.plsBD)); /* change plex of the current entry after realigning previous */
			}
		}
		if (i <= len) return R_FAIL_1; /* this means that previous loop broke because b_addc() failed */
		sym_table.pstvr[sym_table.st_offset] = record;
		st_incoffset();
		return sym_table.st_offset;
	}
	return i;
}

/*******************************************************************************
Purpose: Looks up a lexeme in the symbol table
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 20 November 2015
Called functions: strcmp()
Parameters: STD sym_table, char *lexeme
Return value: int offset of the entry of the STVR array or -1 if fail
Algorithm: perform the search backwards from last entry in array and return index
*******************************************************************************/
int st_lookup(STD sym_table, char *lexeme) {
	if (!sym_table.st_size || !lexeme) return R_FAIL_1;
	while (--(sym_table.st_offset) >= 0 && strcmp(lexeme, sym_table.pstvr[sym_table.st_offset].plex)) ;
	return sym_table.st_offset;
}

/*******************************************************************************
Purpose: Updates the data type indicator in the STVR specified by vid_offset
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 20 November 2015
Called functions: none
Parameters: STD sym_table, int vid_offset, char v_type
Return value: int offset of the entry of the STVR array or -1 if fail
Algorithm: check the update flag of the status_field of the entry,
           if type is already updated, return -1
		   otherwise set operations on the status field
*******************************************************************************/
int st_update_type(STD sym_table, int vid_offset, char v_type) {
	short shft_val;
	if (!sym_table.st_size || vid_offset < 0) return R_FAIL_1;
	switch (v_type) {
	case 'I': case 'F':
		shft_val = sym_table.pstvr[vid_offset].status_field << BIT_SHFT;
		if (shft_val) return R_FAIL_1;
		sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field ^ XOR_MASK;
		return vid_offset;
	default:
		return R_FAIL_1;
	}
}

/*******************************************************************************
Purpose: Updates the i_value of the variable specified by vid_offset
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 20 November 2015
Called functions: none
Parameters: STD sym_table, int vid_offset, InitialValue i_value
Return value: int vid_offset or -1 if fail
*******************************************************************************/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value) {
	if ((!sym_table.st_size || vid_offset < 0) && vid_offset >= sym_table.st_size) return R_FAIL_1;
#ifdef DEBUG
printf("Before update value int = %d;\n", sym_table.pstvr[vid_offset].i_value.int_val);
printf("Before update value flt = %f;\n", sym_table.pstvr[vid_offset].i_value.fpl_val);
printf("Before update value str = %d;\n", sym_table.pstvr[vid_offset].i_value.str_offset);
#endif
	sym_table.pstvr[vid_offset].i_value = i_value;
#ifdef DEBUG
printf("After update value int = %d;\n", sym_table.pstvr[vid_offset].i_value.int_val);
printf("After update value flt = %f;\n", sym_table.pstvr[vid_offset].i_value.fpl_val);
printf("After update value str = %d;\n", sym_table.pstvr[vid_offset].i_value.str_offset);
#endif
	return vid_offset;
}

/*******************************************************************************
Purpose: Returns type of the variable specified by vid_offset
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 20 November 2015
Called functions: none
Parameters: STD sym_table, int vid_offset
Return value: F for floating point, I for integer, or S for string, or -1 if fail
*******************************************************************************/
char st_get_type(STD sym_table, int vid_offset) {
	if ((!sym_table.st_size || vid_offset < 0) && vid_offset >= sym_table.st_size) return R_FAIL_1;
	switch (sym_table.pstvr[vid_offset].status_field & STYPE) {
		case STYPE:
			return 'S';
		case FTYPE:
			return 'F';
		case ITYPE:
			return 'I';
		default:
			return R_FAIL_1;
	}
}

/*******************************************************************************
Purpose: Frees the memory allocated for the symbol table dynamic elements
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: free(), st_setsize(), b_destroy()
Parameters: STD sym_table
Return value: void
*******************************************************************************/
void st_destroy(STD sym_table) {
	st_setsize();
	if (sym_table.pstvr) free(sym_table.pstvr);
	b_destroy(sym_table.plsBD);
}

/*******************************************************************************
Purpose: Prints the contents of the symbol table to standard output
Author: Christopher Elliott, 040 570 022
History/Versions: 1.0 / 20 November 2015
Called functions: printf()
Parameters: STD sym_table
Return value: int - number of the entry printed or -1 if fail
*******************************************************************************/
int st_print(STD sym_table) {
	int i;
	if (!sym_table.st_size) return R_FAIL_1;
	printf("\nSymbol Table\n");
	printf("____________\n\n");
	printf("Line Number Variable Identifier\n");
	for (i = 0; i < sym_table.st_offset; ++i) {
		if (sym_table.pstvr[i].o_line < 10) printf(" ");
		printf("%d          %s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);
	}
	return i;
}

/*******************************************************************************
Purpose: Sets st_size to 0. Used in functions that do not have access to the 
global sym_table
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: none
Parameters: void
Return value: void
*******************************************************************************/
static void st_setsize(void) {
	sym_table.st_size = 0;
}

/*******************************************************************************
Purpose: Increments st_offset by 1. Used in functions that do not have access to 
the global sym_table
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: none
Parameters: void
Return value: void
*******************************************************************************/
static void st_incoffset(void) {
	++sym_table.st_offset;
}

/*******************************************************************************
Purpose: Stores symbol table into $stable.ste
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: fopen(), fprintf(), strlen(), st_get_type(), printf(), fclose()
Parameters: STD sym_table
Return value: int - number of records stored or -1 on failure
Algorithm: write st_size to the file,
           for each entry write the status_field, lexeme length, line number,
           write the appropriate initial value for the corresponding type
           print success message and return the number of records stored
*******************************************************************************/
int st_store(STD sym_table) {
	int i; /* counter */
	FILE * f; /* file to write to */
	/* line 243 generates a variable may be unsafe warning caused by the use of the fopen() function. */
	if ((f = fopen("$stable.ste", "w")) == NULL || !sym_table.st_size) return R_FAIL_1;
	fprintf(f, "%d", sym_table.st_size);
	for (i = 0; i < sym_table.st_offset; ++i) { /* for each entry, output the data items */
		fprintf(f, " %4X %d %s %d", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), 
			sym_table.pstvr[i].plex, sym_table.pstvr[i].o_line);
		switch (st_get_type(sym_table, i)) { /* output the appropriate initial value */
		case 'I':
			fprintf(f, " %d", sym_table.pstvr[i].i_value.int_val);
			break;
		case 'F':
			fprintf(f, " %.2f", sym_table.pstvr[i].i_value.fpl_val);
			break;
		case 'S':
			fprintf(f, " %d", sym_table.pstvr[i].i_value.str_offset);
			break;
		default:
			return R_FAIL_1;
		}
	}
	fclose(f);
	printf("\nSymbol Table stored.\n");
	return i;
}

/*******************************************************************************
Purpose: Sorts the symbol table entries by ascending or descending order
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: qsort(), buffer_sort()
Parameters: STD sym_table, char s_order
Return value: int - 1 if success or -1 if fail
*******************************************************************************/
int st_sort(STD sym_table, char s_order) {
	if (!sym_table.st_size) return R_FAIL_1;
	switch (s_order) {
	case 'A': case 'D':
		qsort(sym_table.pstvr, sym_table.st_offset, sizeof(STVR), (s_order == 'A') ? cmp_asc : cmp_dsc);
		buffer_sort();
		return 1;
	default:
		return R_FAIL_1;
	}
}

/*******************************************************************************
Purpose: Used by qsort to compare two elements for ascending order
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: strcmp()
Parameters: const void *a, const void *b
Return value: int - positive if a > b, negative if b > a, 0 if strings are equal
*******************************************************************************/
static int cmp_asc(const void *a, const void *b) {
	return strcmp(((STVR *)a)->plex,((STVR *)b)->plex);
}

/*******************************************************************************
Purpose: Used by qsort to compare two elements for descending order
Author: Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: strcmp()
Parameters: const void *a, const void *b
Return value: int - positive if b > a, negative if a > b, 0 if strings are equal
*******************************************************************************/
static int cmp_dsc(const void *a, const void *b) {
	return strcmp(((STVR *)b)->plex,((STVR *)a)->plex);
}

/*******************************************************************************
Purpose: Organizes the lexemes in the symbol table lexeme buffer to be in the 
         same order as the entries in the sorted table
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
History/Versions: 1.0 / 20 November 2015
Called functions: b_create(), b_size(), b_inc_factor(), b_mode(), strlen(),
				  b_addc(), b_rflag(), b_setmark(), free(), b_mark()
Parameters: void
Return value: int
Algorithm: create a new Buffer Descriptor using values from the existing Buffer
		   descriptor; loop through the sorted plex variables and add each char
		   to the new Buffer Descriptor; check for reallocation of the Buffer
		   and realign the plex members when needed; set the plex of current
		   STVR; and increment the new buffer's mark_offset for next iteration;
		   outside of loop...delete existing sym_table buffer and assign the new
		   buffer;
*******************************************************************************/
static int buffer_sort(void) {
	int i, k;
	unsigned int j, len;
	pBuffer sort_buf;
	if (!sym_table.st_size) return R_FAIL_1;
	sort_buf = b_create(b_size(sym_table.plsBD), (char)b_inc_factor(sym_table.plsBD),
		(b_mode(sym_table.plsBD) == ADDITIVE) ? 'a' : (b_mode(sym_table.plsBD) == MULTIPLICATIVE) ? 'm' : 'f'); /* create new buffer */
	if (!sort_buf) return R_FAIL_1;
	for (i = 0; i < sym_table.st_offset; ++i) { /* loop through sorted entries */
		for (j = 0; j <= (len = strlen(sym_table.pstvr[i].plex)) && b_addc(sort_buf, sym_table.pstvr[i].plex[j]); ++j) { /* transfer each lexeme to new sorted buffer */
			if (b_rflag(sort_buf)) { /* b_setmark() is used to set mark and to return a char * to assign to plex and as an inout parameter for strlen() */
				for (b_setmark(sort_buf, 0), k = 0; k < i; sym_table.pstvr[k].plex = b_setmark(sort_buf, b_mark(sort_buf)),
					b_setmark(sort_buf, (short)(b_mark(sort_buf) + strlen(b_setmark(sort_buf, b_mark(sort_buf))) + 1)), ++k) ;
			}
		}
		if (j <= len) { /* this means that b_addc() failed which broke the loop */
			free(sort_buf);
			return R_FAIL_1;
		}
		sym_table.pstvr[i].plex = b_setmark(sort_buf, b_mark(sort_buf));
		b_setmark(sort_buf, (short)(b_mark(sort_buf) + len + 1)); /* increment the buffer's mark_offset for next entry */
	}
	free(sym_table.plsBD);
	sym_table.plsBD = sort_buf;
	return 1;
}