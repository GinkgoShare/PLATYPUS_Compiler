/*******************************************************************************
File name: queue.h
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022
Course: CST 8152 - Compilers, Lab Section : 012
Date: 02/01/2015 (DD/MM/YYYY)
Professor: Sv. Ranev
Purpose: Implement the queue data structure.
Function list: [
	q_create(), q_add(), q_remove(), q_isempty(), q_reset(), q_destroy()
]
*******************************************************************************/
#ifndef STACK_H_
#define STACK_H_

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <limits.h>

#define FIXED 0
#define ADDITIVE 1
#define R_FAIL_1 -1

typedef struct QueueDescriptor {
	void* elmnts;
	unsigned short elmnt_sz;
	unsigned short capacity;
	unsigned short elmnt_offset;
	unsigned short inc_factor;
	char mode;
} Queue;

Queue* q_create(short init_capacity, short inc_factor, short elmnt_sz, char mode);
Queue* q_add(Queue* const pQD, const void* elmnt);
void* q_remove(Queue* const pQD, void* elmnt);
int q_isempty(Queue* const pQD);
int q_reset(Queue* const pQD);
void q_destroy(Queue* const pQD);

#endif
