/*
 * erl_process.h
 *
 *  Created on: Oct 26, 2013
 *      Author: Studnicki
 */

#ifndef ERL_PROCESS_H_
#define ERL_PROCESS_H_

#define CONTEXT_REDS 3

void ErlProcessTask(void* args);

typedef struct {
	byte* code;
	uint16_t reds;
} Process;

#endif /* ERL_PROCESS_H_ */
