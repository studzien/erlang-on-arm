/*
 * erl_fun.h
 *
 *  Created on: Aug 19, 2014
 *      Author: Studnicki
 */

#ifndef ERL_FUN_H_
#define ERL_FUN_H_

typedef struct {
	Eterm header;
	BeamInstr* addr;
} ErlFunEntry;


#endif /* ERL_FUN_H_ */
