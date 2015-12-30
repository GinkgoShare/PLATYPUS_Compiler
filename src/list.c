/*******************************************************************************
File name: list.h
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022
Course: CST 8152 - Compilers, Lab Section : 012
Date: 29/12/2015 (DD/MM/YYYY)
Professor: Sv. Ranev
Purpose: Implement the list data structure.
Function list: [
	l_create(), l_add(), l_get(), l_remove(), l_set(), l_size(), 
	l_isEmpty(), l_pack(), l_contains(), l_destroy()
]
*******************************************************************************/
#include "list.h"
/*******************************************************************************
Purpose: Initialize a ListDescriptor structure on the program heap. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 29/12/2015
Called functions: [ calloc(), malloc(), free() ]
Parameters: init_capacity is a short type with an inclusive range of 0 to 
			SHRT_MAX, inc_factor is a short type with an inclusive range of 0 
			to SHRT_MAX
Return value: returns the address of the defined ListDescriptor
Algorithm: initialize the ListDescriptor structure, initialize embedded array
		   to a capacity defined by the argument init_capacity, and set the list 
		   increment factor
*******************************************************************************/
List* l_create(short init_capacity, short inc_factor, short elmnt_sz) {
	List* pList;
	if (init_capacity < 0 || elmnt_sz <= 0 || inc_factor <= 0) { return NULL; }

	pList = (List*)calloc( 1, sizeof(List));
	if (!pList) return NULL;

	/* set capacity and allocate memory */
	pList->capacity = (int)(elmnt_sz * init_capacity);
	pList->elmnts = malloc(pList->capacity);
	if (!pList->elmnts) {
		free(pList);
		return NULL;
	}

	pList->inc_factor = inc_factor;
	pList->elmnt_sz = elmnt_sz;
	return pList;
}
/*******************************************************************************
Purpose: Append a new item to the list. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 29/12/2015
Called functions: [ realloc(), memcpy() ]
Parameters: pLD is the ListDescriptor that is being appended to, elmnt is the 
			new element to append to the list.
Return value: returns the address of the ListDescriptor
Algorithm: Check if the list is at its maximum capacity, check if the list is at
		   its current capacity, if so, reallocate with additional memory
		   storage, add element to the end of the list
*******************************************************************************/
void* l_add(List* const pLD, const void* elmnt) {
	void* pLoc; void* dest_addr;
	short new_capacity;
	if (!pLD && pLD->capacity == INT_MAX) return NULL;

	if((pLD->elmnt_offset * pLD->elmnt_sz) == pLD->capacity) {
		new_capacity = pLD->capacity + (pLD->inc_factor * pLD->elmnt_sz);
		if (new_capacity < 0) { new_capacity = INT_MAX; }
		pLoc = realloc(pLD->elmnts, new_capacity);
		if (pLoc == NULL) return NULL;
		pLD->capacity = new_capacity;
		pLD->elmnts = pLoc;
	}

	dest_addr = (char*)pLD->elmnts + (pLD->elmnt_offset * pLD->elmnt_sz);
	dest_addr = (char*)memcpy(dest_addr, elmnt, pLD->elmnt_sz);
	pLD->elmnt_offset++;
	return pLD;
}
/*******************************************************************************
Purpose: Get the address of an element. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 29/12/2015
Called functions: [ realloc(), memcpy() ]
Parameters: pLD is the ListDescriptor that is being appended to, index is the 
			index of the element to return.
Return value: returns the address at the specified index.
*******************************************************************************/
void* l_get(List* const pLD, const int index) {
	if (!pLD || index >= pLD->elmnt_offset) return NULL;
	return (char*)pLD->elmnts + (--(pLD->elmnt_offset) * pLD->elmnt_sz);
}
/*******************************************************************************
Purpose: Set's an already specified index with a new element. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 29/12/2015
Called functions: [ memcpy() ]
Parameters: pLD is the ListDescriptor that is being appended to, index is the 
			index to set the element to, elmnt is the new item to store.
Return value: returns the address of the list descriptor.
Algorithm: Make sure the specified index is within a valid range, store element
*******************************************************************************/
void* l_set(List* const pLD, const int index, const void* elmnt) {
	void *dest_addr;
	if (!pLD || index < 0 || index >= pLD->elmnt_offset) return NULL;
	dest_addr = (char*)pLD->elmnts + (index * pLD->elmnt_sz);
	dest_addr = (char*)memcpy(dest_addr, elmnt, pLD->elmnt_sz);
	return pLD;
}
/*******************************************************************************
Purpose: Removes an element from the list and shifts remaining elements. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 29/12/2015
Called functions: [ memcpy() ]
Parameters: pLD is the ListDescriptor that is being appended to, index is the 
			index to set the element to, elmnt is the new item to store.
Return value: the adjusted size of the list
Algorithm: Make sure the specified index is within a valid range, shift the
		   following elements to the removed index position
*******************************************************************************/
int l_remove(List* const pLD, const int index) {
	void* dest_addr; void* fllwng_elmnts;
	if (!pLD || index < 0 || index >= pLD->elmnt_offset) return NULL;
	/* shift remaining elements */
	dest_addr = (char*)pLD->elmnts + (index * pLD->elmnt_sz);
	fllwng_elmnts = (char*)pLD->elmnts + ((index+1) * (pLD->elmnt_sz * (pLD->elmnt_offset - (index-1))));
	dest_addr = (char*)memcpy(dest_addr, fllwng_elmnts, pLD->elmnt_sz);
	return --(pLD->elmnt_offset);
}
/*******************************************************************************
Purpose: Returns the current size of the list. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 29/12/2015
Parameters: pLD is the ListDescriptor to find the size of
Return value: the current size of the list
*******************************************************************************/
int l_size(List* const pLD) {
	return (pLD == NULL) ? R_FAIL_1 : pLD->elmnt_offset;
}
/*******************************************************************************
Purpose: Returns true if the list is currently empty. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 29/12/2015
Parameters: pLD is the ListDescriptor to query
Return value: true if the list is currently empty or null, false if otherwise
*******************************************************************************/
int l_isEmpty(List* const pLD) {
	return (pLD == NULL) ? R_FAIL_1 : (pLD->elmnt_offset == 0);
}
/*******************************************************************************
Purpose: resize the list to its current size plus one
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 29/12/2015
Called functions: [ realloc() ]
Parameters: the address of the list
Return value: returns the constant NULL for a failure or the address of the
			  address of the list when successful
Algorithm: set a temporary variable to reallocate new memory, try to reallocate
		   to a new capacity, if reallocation fails then return NULL, otherwise
		   set capacity member and end of list to 0, return
*******************************************************************************/
void* l_pack(List* const pLD) {
	void* pLoc;
	short new_capacity;
	if (pLD == NULL || pLD->capacity == INT_MAX) return NULL;

	if (pLD->capacity == ((pLD->elmnt_offset+1) * pLD->elmnt_sz)) return pLD; /* list is already packed */
	new_capacity = (pLD->elmnt_offset+1) * pLD->elmnt_sz;
	if (new_capacity < 0) new_capacity = INT_MAX;
	pLoc = realloc(pLD->elmnts, new_capacity);
	if (pLoc == NULL) { return NULL; }
	pLD->capacity = new_capacity;
	pLD->elmnts = pLoc;
	return pLD;
}
/*******************************************************************************
Purpose: Free the list's allocated memory
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 29/12/2015
Called functions: [ free() ]
Parameters: the list to free
*******************************************************************************/
void l_destroy(List* const pLD) {
	if (pLD) {
		if (pLD->elmnts != NULL) { free(pLD->elmnts); }
		free(pLD); 
	}
}