/*******************************************************************************
File name: stack.h
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022
Course: CST 8152 - Compilers, Lab Section : 012
Date: 06/12/2015 (DD/MM/YYYY)
Professor: Sv. Ranev
Purpose: Implement the stack data structure.
Function list: [
	s_create(), s_push(), s_pop(), s_isempty(), s_reset(), s_destroy()
]
 ******************************************************************************/
#ifndef STACK_H_
#define STACK_H_

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <limits.h>

#define FIXED 0
#define ADDITIVE 1

typedef struct StackDescriptor {
	void* elmnts;
	unsigned short elmnt_sz;
	unsigned short capacity;
	unsigned short elmnt_offset;
	unsigned short inc_factor;
	char mode;
} Stack, *pStack;

Stack * s_create(short init_capacity, short inc_factor, short elmnt_sz, char mode);
pStack s_push(pStack const pStck, const void* elmnt);
void* s_pop(pStack const pStck);
int s_isempty(Stack * const pStck);
int s_reset(Stack * const pStck);
void s_destroy(Stack * const pStck);

#endif
