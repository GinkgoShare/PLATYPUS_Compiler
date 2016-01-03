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
#include "buffer.h"
#include "token.h"
#include "stable.h"
#include "stack.h"
#include "queue.h"

#define NO_ATTR -1

typedef enum KeywordIndexes { ELSE, IF, INPUT, OUTPUT, PLATYPUS, REPEAT, THEN, USING } KW;

void parser(Buffer * in_buf);

#endif
