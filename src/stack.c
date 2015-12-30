/*******************************************************************************
File name: stack.c
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022
Course: CST 8152 - Compilers, Lab Section : 012
Date: 06/12/2015 (DD/MM/YYYY)
Professor: Sv. Ranev
Purpose: Implement the stack data structure.
Function list: [ 
	create(), stackpush(), stackpop, isempty(), reset(), destroy()
]
*******************************************************************************/

#include "stack.h"

#define DEBUG
#undef DEBUG

#ifdef DEBUG
#include "token.h"
#endif

/*******************************************************************************
Purpose: Initialize a StackDescriptor structure on the program heap. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 07/12/2015
Called functions: [ calloc(), malloc(), free() ]
Parameters: init_capacity is a short type with an inclusive range of 0 to
			SHRT_MAX, inc_factor is a char type with an inclusive range of 0 to
			255, o_mode is a char type with an inclusive range of 0 to 1
Return value: returns the address of the defined StackDescriptor
Algorithm: initialize the StackDescriptor structure, initialize embedded array
		   to a capacity defined by the argument init_capacity, set the buffer
		   operational mode and increment factor
*******************************************************************************/
Stack * s_create(short init_capacity, short inc_factor, short elmnt_sz, char mode) {
	Stack* pStack;
	if (init_capacity < 0 || elmnt_sz < 0) { return NULL; }

	pStack = (Stack*)calloc(1, sizeof(Stack));
	if (!pStack) return NULL;

	/* set stack mode and stack increment factor */
	if ((mode == 'f' || inc_factor == 0) && init_capacity > 0) { inc_factor = 0; } 
	else if (mode == 'a') { pStack->mode = ADDITIVE; }
	else { /* stack must meet one of the above modes or it is invalid */
		free(pStack);
		return NULL;
	}

	/* set capacity and allocate memory */
	pStack->capacity = (short)(elmnt_sz * init_capacity);
	pStack->elmnts = (char *)malloc(pStack->capacity);
	if (!pStack->elmnts) {
		free(pStack);
		return NULL;
	}

	pStack->inc_factor = inc_factor;
	pStack->elmnt_sz = elmnt_sz;
	return pStack;
}
/*******************************************************************************
Purpose: Add next element to the stack
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Called functions: [ realloc(), memcpy() ]
Parameters: the address of the stack and a type void* to be stored in the
			buffer
Return value: returns the address of the StackDescriptor type
Algorithm: check if stack is at capacity and allocate more space if necessary,
		   add element to stack and increment the stack's elmnt_offset
*******************************************************************************/
pStack s_push(pStack const pSD, const void* elmnt) {
	void* pLoc; 
	void *dest_addr;
	short new_capacity;
	if (!pSD && pSD->capacity == USHRT_MAX) return NULL;
	#ifdef DEBUG
	printf("Token code in s_push is %d\n", ((Token*)elmnt)->code);
	#endif

	if((pSD->elmnt_offset * pSD->elmnt_sz) == pSD->capacity) {
		if (!pSD->mode) return NULL;
		new_capacity = pSD->capacity + (pSD->inc_factor * pSD->elmnt_sz);
		if (new_capacity < 0) { return NULL; }
		pLoc = realloc(pSD->elmnts, new_capacity);
		if (pLoc == NULL) return NULL;
		pSD->capacity = new_capacity;
		pSD->elmnts = pLoc;
	}

	dest_addr = (char *)pSD->elmnts + pSD->elmnt_offset * pSD->elmnt_sz;
	memcpy(dest_addr, elmnt, pSD->elmnt_sz);
	pSD->elmnt_offset++;
	return pSD;
}
/*******************************************************************************
Purpose: Get the next element on the stack
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Parameters: the address of the stack
Return value: returns the address of the element
Algorithm: decrement elmnt_offset and return the pointed to element
*******************************************************************************/
void* s_pop(pStack const pSD) {
	if (!pSD || !pSD->elmnt_offset) return NULL;
	pSD->elmnt_offset--;
	#ifdef DEBUG
	printf("Token code in s_pop is %d\n", ((Token*)pSD->elmnts + pSD->elmnt_offset * pSD->elmnt_sz)->code);
	#endif
	return (char*)pSD->elmnts + pSD->elmnt_offset * pSD->elmnt_sz;
}
/*******************************************************************************
Purpose: Returns the elmnt_offset, if it is 0 then the stack is empty
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Parameters: the address of the stack
Return value: returns the value of elmnt_offset
*******************************************************************************/
int s_isempty(Stack * const pSD) {
	return (pSD == NULL) ? R_FAIL_1 : (pSD->elmnt_offset == 0);
}
/*******************************************************************************
Purpose: Resets the stack to its created form.
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Parameters: the address of the stack
Return value: returns an integral value representing a succesful operation
*******************************************************************************/
int s_reset(Stack * const pSD) {
	if (!pSD) return -1;
	pSD->elmnt_offset = 0;
	return 1;
}
/*******************************************************************************
Purpose: Free the stack's allocated memory.
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Parameters: the address of the stack
Return value: void
*******************************************************************************/
void s_destroy(Stack * const pSD) {
	if (pSD) {
		if (pSD->elmnts) { free(pSD->elmnts); }
		free(pSD); 
	}
}