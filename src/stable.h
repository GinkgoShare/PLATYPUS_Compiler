/*******************************************************************************
File name: stable.h
Compiler: MS Visual Studio 2012
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

#ifndef STABLE_H_
#define STABLE_H_

#define DEFAULT 0xFFF8		/* 1111 1111 1111 1000 */
#define ITYPE 0x0004		/* 0000 0000 0000 0100 */
#define FTYPE 0x0002		/* 0000 0000 0000 0010 */
#define STYPE 0x0007		/* 0000 0000 0000 0111 */
#define XOR_MASK 0x0007		/* 0000 0000 0000 0111 */

#define NVLD_SZ 0			/* invalid size of a symbol table descriptor */
#define INIT_SZ 10			/* initial size of the symbold table buffer */
#define INC_FCTR 10			/* increment factor of the symbold table buffer */
#define BIT_SHFT 15			/* shifting value for bitwise operation */
#define EPSILON (-1)		/* empty string value for string offset */

typedef union InitialValue {
	int int_val;							/* integer variable initial value */
	float fpl_val;							/* floating-point variable initial value */
	int str_offset;							/* string variable initial value (offset) */
} InitialValue;

typedef struct SymbolTableVidRecord {
	unsigned short status_field;			/* variable record status field */
	char * plex;							/* pointer to lexeme (VID name) in CA */
	int o_line;								/* line of first occurrence */
	InitialValue i_value;					/* variable initial value */
	size_t reserved;						/* reserved for future use */
} STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr;							/* pointer to array of STVR */
	int st_size;							/* size in number of STVR elements */
	int st_offset;							/* offset in number of STVR elements */
	Buffer *plsBD;							/* pointer to the lexeme storage buffer descriptor */
} STD;

/* function declarations */
STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_update_type(STD sym_table, int vid_offset, char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type(STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
static void st_setsize(void);
static void st_incoffset(void);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);

#endif
