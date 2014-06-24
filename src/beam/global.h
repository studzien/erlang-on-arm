/*
 * global.h
 *
 *  Created on: Oct 26, 2013
 *      Author: Studnicki
 */

#ifndef GLOBAL_H_
#define GLOBAL_H_


#include "FreeRTOS.h"
#include "task.h"
#include "sys.h"
#include "config.h"
#include "erl_term.h"

typedef Eterm BeamInstr;

typedef struct {
	Eterm name; // atom with the module name
	uint16_t size; // number of BeamInstr instructions in the code
	BeamInstr* code;
} BeamModule;

#define MAX(x, y) (((x) > (y)) ? (x) : (y))

#endif /* GLOBAL_H_ */
