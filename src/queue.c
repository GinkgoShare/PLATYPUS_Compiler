/*******************************************************************************
File name: queue.c
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

#include "queue.h"

#define DEBUG
#undef DEBUG

#ifdef DEBUG
#include "token.h"
#endif

/*******************************************************************************
Purpose: Initialize a QueueDescriptor structure on the program heap. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 02/01/2015
Called functions: [ calloc(), malloc(), free() ]
Parameters: init_capacity is a short type with an inclusive range of 0 to
			SHRT_MAX, inc_factor is a char type with an inclusive range of 0 to
			255, o_mode is a char type with an inclusive range of 0 to 1
Return value: returns the address of the defined QueueDescriptor
Algorithm: initialize the QueueDescriptor structure, initialize embedded array
		   to a capacity defined by the argument init_capacity, set the buffer
		   operational mode and increment factor
*******************************************************************************/
Queue* q_create(short init_capacity, short inc_factor, short elmnt_sz, char mode) {
	Queue* pQueue;
	if (init_capacity < 0 || elmnt_sz < 0) { return NULL; }

	pQueue = (Queue*)calloc(1, sizeof(Queue));
	if (!pQueue) return NULL;

	/* set queue mode and queue increment factor */
	if ((mode == 'f' || inc_factor == 0) && init_capacity > 0) { inc_factor = 0; } 
	else if (mode == 'a') { pQueue->mode = ADDITIVE; }
	else { /* queue must meet one of the above modes or it is invalid */
		free(pQueue);
		return NULL;
	}

	/* set capacity and allocate memory */
	pQueue->capacity = (short)(elmnt_sz * init_capacity);
	pQueue->elmnts = (char *)malloc(pQueue->capacity);
	if (!pQueue->elmnts) {
		free(pQueue);
		return NULL;
	}

	pQueue->inc_factor = inc_factor;
	pQueue->elmnt_sz = elmnt_sz;
	return pQueue;
}
/*******************************************************************************
Purpose: Add next element to the queue
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Called functions: [ realloc(), memcpy() ]
Parameters: the address of the queue and a type void* to be stored in the
			buffer
Return value: returns the address of the QueueDescriptor type
Algorithm: check if queue is at capacity and allocate more space if necessary,
		   add element to queue and increment the queue's elmnt_offset
*******************************************************************************/
Queue* q_add(Queue* const pQD, const void* elmnt) {
	void* pLoc; 
	void *dest_addr;
	short new_capacity;
	if (!pQD && pQD->capacity == USHRT_MAX) return NULL;
	#ifdef DEBUG
	printf("Token code in s_push is %d\n", ((Token*)elmnt)->code);
	#endif

	if((pQD->elmnt_offset * pQD->elmnt_sz) == pQD->capacity) {
		if (!pQD->mode) return NULL;
		new_capacity = pQD->capacity + (pQD->inc_factor * pQD->elmnt_sz);
		if (new_capacity < 0) { return NULL; }
		pLoc = realloc(pQD->elmnts, new_capacity);
		if (pLoc == NULL) return NULL;
		pQD->capacity = new_capacity;
		pQD->elmnts = pLoc;
	}

	dest_addr = (char *)pQD->elmnts + pQD->elmnt_offset * pQD->elmnt_sz;
	memcpy(dest_addr, elmnt, pQD->elmnt_sz);
	pQD->elmnt_offset++;
	return pQD;
}
/*******************************************************************************
Purpose: Get the next element on the queue
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Parameters: the address of the queue
Return value: returns the address of the element
Algorithm: decrement elmnt_offset and return the pointed to element
*******************************************************************************/
void* q_remove(Queue* const pQD, void* elmnt) {
	if (!pQD) return NULL;
	pQD->elmnt_offset--;
	/* copy head to */
	memcpy(elmnt, pQD->elmnts, pQD->elmnt_sz);
	/* shift remaining elements */
	memcpy(pQD->elmnts, ((char *)pQD->elmnts) + pQD->elmnt_sz, pQD->elmnt_sz * pQD->elmnt_offset);
	return elmnt;
}
/*******************************************************************************
Purpose: Returns the current size of the queue. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 29/12/2015
Parameters: pQD is the QueueDescriptor to find the size of
Return value: the current size of the queue
*******************************************************************************/
short q_size(Queue* const pQD) {
	return (pQD == NULL) ? R_FAIL_1 : pQD->elmnt_offset;
}
/*******************************************************************************
Purpose: Returns the elmnt_offset, if it is 0 then the queue is empty
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Parameters: the address of the queue
Return value: returns the value of elmnt_offset
*******************************************************************************/
int q_isempty(Queue * const pQD) {
	return (pQD == NULL) ? R_FAIL_1 : (pQD->elmnt_offset == 0);
}
/*******************************************************************************
Purpose: Resets the queue to its created form.
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Parameters: the address of the queue
Return value: returns an integral value representing a succesful operation
*******************************************************************************/
int q_reset(Queue * const pQD) {
	if (!pQD) return -1;
	pQD->elmnt_offset = 0;
	return 1;
}
/*******************************************************************************
Purpose: Free the queue's allocated memory.
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 07/12/2015
Parameters: the address of the queue
Return value: void
*******************************************************************************/
void q_destroy(Queue * const pQD) {
	if (pQD) {
		if (pQD->elmnts) { free(pQD->elmnts); }
		free(pQD); 
	}
}