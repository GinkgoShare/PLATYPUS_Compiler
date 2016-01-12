/*******************************************************************************
File name: parser.h
Compiler: Borland 5.5
Author: Christopher Elliott, 040 570 022 and Jeremy Chen, 040 742 822
Course: CST 8152 - Compilers, Lab Section : 012
Assignment: 4
Date: 11 December 2015
Professor: Sv. Ranev
Purpose:  Implements a Recursive Descent Predictive Parser for PLATYPUS
*******************************************************************************/
#ifndef PARSER_H_
#define PARSER_H_

#include <stdlib.h>
#include <ctype.h>
#include "buffer.h"
#include "stable.h"
#include "token.h"
#include "stack.h"
#include "list.h"

#define NO_ATTR -1
#define MAX_ISZ 32767		/* max range for a PLATYPUS int */
#define MIN_ISZ (-32768)	/* min range for a PLATYPUS int */
#define MAX_FSZ 3.4E+38		/* max range for a PLATYPUS float */
#define MIN_FSZ 1.2E-38		/* min range for a PLATYPUS float */
#define INL_MSK 0xFF

typedef enum KeywordIndexes { ELSE, IF, INPUT, OUTPUT, PLATYPUS, REPEAT, THEN, USING } KW;

void parser(Buffer * in_buf);

#endif
