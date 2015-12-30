/*******************************************************************************
File name: buffer.c
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022
Course: CST 8152 - Compilers, Lab Section : 012
Assignment: 1
Date: 15/09/2015 (DD/MM/YYYY)
Professor: Sv. Ranev
Purpose: Implement the buffer data structure which will be used to store
		 collections of type char.
Function list: [ 
	b_create(), b_addc(), b_reset(), b_destroy(), b_isfull(),
	b_size(), b_capacity(), b_setmark(), b_mark(), b_mode(),
	b_inc_factor(), b_load(), b_isempty(), b_eob(), b_getc(),
	b_print(), b_pack(), b_rflag(), b_retract(),
	b_retract_to_mark(), b_getc_offset() 
]
*******************************************************************************/
#include "buffer.h"
/*******************************************************************************
Purpose: Initialize a Buffer structure on the program heap. 
Author: Christopher JW Elliott, 040 570 022
History/Versions: Version 0.0.1 15/09/2015
Called functions: [ calloc(), malloc(), free() ]
Parameters: init_capacity is a short type with an inclusive range of 0 to
			SHRT_MAX, inc_factor is a char type with an inclusive range of 0 to
			255, o_mode is a char type with an inclusive range of -1 to 1
Return value: returns the address of the defined BufferDescriptor
Algorithm: initialize the BufferDescriptor structure, initialize embedded
		   character array to a capacity defined by the argument init_capacity,
		   set the buffer operational mode and increment factor
*******************************************************************************/
Buffer * b_create(short init_capacity, char inc_factor, char o_mode) {
	Buffer* pBuffer;
	if (init_capacity < 0) { return NULL; }

	/* allocate memory and check for errors */
	pBuffer = (Buffer*)calloc(1, sizeof(Buffer));
	if (pBuffer == NULL) { return NULL; }

	/* set buffer mode and buffer increment factor */
	if ((o_mode == 'f' || inc_factor == 0) && init_capacity > 0) { inc_factor = 0; } 
	else if (o_mode == 'a') { pBuffer->mode = ADDITIVE; }
	else if (o_mode == 'm' && (inc_factor >= MLT_RANGE_MIN && inc_factor <= MLT_RANGE_MAX)) { pBuffer->mode = MULTIPLICATIVE; }
	else { /* buffer must meet one of the above modes or it is invalid */
		free(pBuffer);
		return NULL;
	}

	/* set capacity and allocate memory */
	pBuffer->capacity = (short)(sizeof(char) * init_capacity);
	pBuffer->cb_head = (char *)malloc(pBuffer->capacity);
	if (pBuffer->cb_head == NULL) {
		free(pBuffer);
		return NULL;
	}

	pBuffer->inc_factor = inc_factor;
	return pBuffer;
}
/*******************************************************************************
Purpose: Add next character to the buffer
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Called functions: [ realloc() ]
Parameters: the address of the buffer and a type char to be stored in the
			buffer
Return value: returns the address of the BufferDescriptor type
Algorithm: set the buffers r_flag to 0, check if buffer is at capacity and
		   allocate more space if necessary, add character to buffer and
		   increment the next available index defined by the buffer member
		   addc_offset
*******************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	char* pLoc; 
	short new_increment, new_capacity;
	if (pBD == NULL) { return NULL; }
	pBD->r_flag = 0;
	if (pBD->addc_offset == SHRT_MAX) { return NULL; }
	if (((short int)(pBD->addc_offset * sizeof(char))) == pBD->capacity) {
		switch (pBD->mode) {
			case ADDITIVE:
				new_capacity = (short int)(pBD->capacity + ((unsigned char)pBD->inc_factor * sizeof(char)));
				if (new_capacity < 0) { return NULL; }
				break;
			case MULTIPLICATIVE:
				new_increment = (short int)(((short int)SHRT_MAX - pBD->capacity) * ((float)pBD->inc_factor / MLT_RANGE_MAX));
				new_capacity = (new_increment == 0) ? (short int)SHRT_MAX : (short int)(pBD->capacity + (new_increment * sizeof(char)));
				break;
			default:
				return NULL;
		}
		pLoc = (char *)realloc(pBD->cb_head, new_capacity);
		if (pLoc == NULL) { return NULL; }
		if (pBD->cb_head != pLoc) { pBD->r_flag = SET_R_FLAG; }
		pBD->capacity = new_capacity;
		pBD->cb_head = pLoc;
	}
	pBD->cb_head[pBD->addc_offset++] = symbol;
	return pBD;
}
/*******************************************************************************
Purpose: Reset buffer to its starting positions so data can be overridden
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer to be reset
Return value: returns an int with a value of -1 on failure and 1 for success
*******************************************************************************/
int b_reset(Buffer * const pBD) {
	if (pBD == NULL) { return R_FAIL_1; }
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->mark_offset = 0;
	pBD->r_flag = 0;
	pBD->eob = 0;
	return 1;
}
/*******************************************************************************
Purpose: Free the buffers allocated memory on the heap
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Called functions: [ free() ]
Parameters: the address of the buffer to free
Return value: void
*******************************************************************************/
void b_destroy(Buffer * const pBD) {
	if (pBD != NULL) {
		if (pBD->cb_head != NULL) { free(pBD->cb_head); }
		free(pBD); 
	}
}
/*******************************************************************************
Purpose: Tells the caller if the buffer is at its capacity.
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: Returns an int with a value of -1 for a NULL pointer failure, 0
			  when the buffer has not reached its capacity and 1 when it has.
*******************************************************************************/
#ifndef INLINE
int b_isfull(Buffer * const pBD) {
	return (pBD == NULL) ? R_FAIL_1 : (((short)(pBD->addc_offset * sizeof(char))) == pBD->capacity);
}
#endif
/*******************************************************************************
Purpose: Returns the current size of the buffer.
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type short value of -1 on a failure or the value of the
			  buffers next available index which is used here as the buffers
			  size
*******************************************************************************/
#ifndef INLINE
short b_size(Buffer * const pBD) {
	return (pBD == NULL) ? (short)R_FAIL_1 : pBD->addc_offset;
}
#endif
/*******************************************************************************
Purpose: returns the current buffer capacity
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type short value of -1 for a failure or the value of the
			  buffer's capacity
*******************************************************************************/
#ifndef INLINE
short b_capacity(Buffer * const pBD) {
	return (pBD == NULL) ? (short)R_FAIL_1 : pBD->capacity;
}
#endif
/*******************************************************************************
Purpose: set the buffers marked index
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer and the value to set the marking index
Return value: the address of the character pointed to by the mark_offset index
*******************************************************************************/
char * b_setmark(Buffer * const pBD, short mark) {
	if (pBD == NULL || mark < 0 || mark > pBD->addc_offset) { return NULL; }
	pBD->mark_offset = mark;
	return (pBD->cb_head + pBD->mark_offset);
}
/*******************************************************************************
Purpose: return the buffers marked index
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type short value of -1 for a failure or the value of the
			  buffers mark_offset member
*******************************************************************************/
#ifndef INLINE
short b_mark(Buffer * const pBD) {
	return (pBD == NULL) ? (short)R_FAIL_1 : pBD->mark_offset;
}
#endif
/*******************************************************************************
Purpose: return the buffers mode
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type short value of -2 for a failure or the value of the
			  the buffers mode member
*******************************************************************************/
#ifndef INLINE
int b_mode(Buffer * const pBD) {
	return (pBD == NULL) ? R_FAIL_2 : pBD->mode;
}
#endif
/*******************************************************************************
Purpose: return the buffers incrementing factor
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type unsigned int value of 256 for a failure or the
			  value of the the buffers incrementing factor member
*******************************************************************************/
#ifndef INLINE
size_t  b_inc_factor(Buffer * const pBD) {
	return (pBD == NULL) ? I_FCTR_FAIL : (unsigned char)pBD->inc_factor;
}
#endif
/*******************************************************************************
Purpose: load the buffer from a source file
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Called functions: [ fgetc(), feof(), b_addc() ]
Parameters: the address of the file structure, the address of the buffer
Return value: returns a type signed int of -1 for a failure or the count of
			  chars that were added to the buffer
Algorithm: get the first character fromthe file structure, if a character is
		   EOF character the the file structure will set its flag, check if
		   the flag was set and if it wasn't then add character to the buffer,
		   repeat until end of file has been reached
*******************************************************************************/
int b_load(FILE * const fi, Buffer * const pBD) {
	char symbol;
	int char_count = 0;	
	if (fi == NULL || pBD == NULL) { return R_FAIL_1; }
	for (symbol = (char)fgetc(fi); !feof(fi); symbol = (char)fgetc(fi), ++char_count)
		if (b_addc(pBD, symbol) == NULL) { return LOAD_FAIL; }
	return char_count;
}
/*******************************************************************************
Purpose: check if the buffer is empty
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type signed int of -1 for a failure or either 0 if the 
			  buffer's addc_offset member, which is used to mark the size of the
			  buffer, is equal to 0 or returns 1 otherwise		
*******************************************************************************/
#ifndef INLINE
int b_isempty(Buffer * const pBD) {
	return (pBD == NULL) ? R_FAIL_1 : (pBD->addc_offset == 0);
}
#endif
/*******************************************************************************
Purpose: return the value of the buffer's eob member
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type signed int of -1 for a failure or the value of the
			  buffer's eob member
*******************************************************************************/
#ifndef INLINE
int b_eob(Buffer * const pBD) {
	return (pBD == NULL) ? R_FAIL_1 : pBD->eob;
}
#endif
/*******************************************************************************
Purpose: return the character pointed to by the getc_offset index
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type signed char of -2 on a failure or the char pointed
			  to by the buffer's getc_offset index
Algorithm: if the buffer's getc_offset index is equal to it's addc_offset index
		   then set the end of buffer flag to 1 and return a failure value,
		   otherwise reset the end of buffer to 0 and return the character
		   pointed to by the buffer's getc_offset index
*******************************************************************************/
char b_getc(Buffer * const pBD) {
	if (pBD == NULL) { return (char)R_FAIL_2; }
	pBD->eob = 0;
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->eob = SET_EOB_1;
		return (char)R_FAIL_1;
	}
	return pBD->cb_head[pBD->getc_offset++];
}
/*******************************************************************************
Purpose: Print contents of the buffer to the console window.
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Called functions: [ printf(), b_getc() ]
Parameters: the address of the buffer
Return value: returns a type signed int of -1 for a failure or the value of the
			  buffer's addc_offset member
Algorithm: set the buffer's getc_offset member to point to the start of the
		   character array, check if the buffer is empty, if it's not then
		   continue to print each character of the buffer until we reach the
		   end, append a newline character, set the getc_offset back to the
		   start of the buffer and return
*******************************************************************************/
int b_print(Buffer * const pBD) {
	char ch;
	if (pBD == NULL) { return R_FAIL_1; }
	pBD->getc_offset = 0;
	if (pBD->addc_offset == 0) { printf("The buffer is empty."); }
	for (ch = b_getc(pBD); !b_eob(pBD); ch = b_getc(pBD)) { printf("%c", ch); }
	printf("\n");
	pBD->getc_offset = 0;
	return (int)pBD->addc_offset;
}
/*******************************************************************************
Purpose: resize the buffer to its current size plus one
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns the constant NULL for a failure or the address of the
			  address of the buffer when successful
Algorithm: set a temporary variable to point to the buffer, try to reallocate
		   to a new capacity, if reallocation fails then return the address of
		   the buffer back to its original address and return NULL, otherwise
		   check if reallocation moved to a new address and set the r_flag
		   member to 1 if it has, set capacity member and end of buffer to 0,
		   return
*******************************************************************************/
Buffer *b_pack(Buffer * const pBD) {
	char* pLoc;
	short new_capacity;
	if (pBD == NULL || pBD->addc_offset == SHRT_MAX) { return NULL; }

	pBD->eob = 0;
	pBD->r_flag = 0;
	if (pBD->capacity == (pBD->addc_offset + 1)) { return pBD; } /* if buffer is already packed */	
	new_capacity = (short)((pBD->addc_offset + 1) * sizeof(char));
	if (new_capacity < 0) { return NULL; }
	pLoc = (char *)realloc(pBD->cb_head, new_capacity);
	if (pLoc == NULL) { return NULL; }
	if (pLoc != pBD->cb_head) { pBD->r_flag = SET_R_FLAG; }
	pBD->capacity = new_capacity;
	pBD->cb_head = pLoc;
	return pBD;
}
/*******************************************************************************
Purpose: return the value of the buffer's r_flag member
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type signed char of -1 for a failure or the value of the
			  buffer's r_flag member
*******************************************************************************/
#ifndef INLINE
char b_rflag(Buffer * const pBD) {
	return (pBD == NULL) ? (char)R_FAIL_1 : pBD->r_flag;
}
#endif
/*******************************************************************************
Purpose: decrement the buffer's getc_offset member
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type signed short of -1 for a failure or the decremented
			  value of the buffer's getc_offset member
*******************************************************************************/
#ifndef INLINE
short b_retract(Buffer * const pBD) {
	return (pBD == NULL || pBD->getc_offset == 0) ? (short)R_FAIL_1 : --(pBD->getc_offset);
}
#endif
/*******************************************************************************
Purpose: set the buffer's getc_offset member to its mark_offset value
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type signed short of -1 for a failure or the value of
			  the buffer's getc_offset member after it has been retracted
*******************************************************************************/
short b_retract_to_mark(Buffer * const pBD) {
	if (pBD == NULL) { return (short)R_FAIL_1; }
	pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
}
/*******************************************************************************
Purpose: return the value of the buffer's getc_offset member
Author: Christopher JW Elliott
History/Versions: Version 0.0.1 15/09/2015
Parameters: the address of the buffer
Return value: returns a type signed short of -1 for a failure or the value of
			  the buffers getc_offset member
*******************************************************************************/
#ifndef INLINE
short b_getc_offset(Buffer * const pBD) {
	return (pBD == NULL) ? (short)R_FAIL_1 : pBD->getc_offset;
}
#endif
