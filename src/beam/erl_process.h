/*
 * erl_process.h
 *
 *  Created on: Oct 26, 2013
 *      Author: Studnicki
 */

#ifndef ERL_PROCESS_H_
#define ERL_PROCESS_H_

#include "global.h"
#include "basic_io.h"
#include "export.h"

void ErlProcessTask(void* args);


typedef struct {

} ErlSpawnOpts;

struct ErlProcess {
	Eterm id;

	struct ErlProcess* parent; //parent process
	BeamInstr* i; // program counter

	xTaskHandle* handle;

	uint8_t active; //is taken from pool?
};

typedef struct ErlProcess ErlProcess;

void init_process_table(void);
Eterm erl_create_process(ErlProcess*, Eterm, Eterm, Eterm, ErlSpawnOpts*);

#endif /* ERL_PROCESS_H_ */
