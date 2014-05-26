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
#include "atom.h"

void ErlProcessTask(void* args);


typedef struct {

} ErlSpawnOpts;

struct ErlProcess {
	Eterm id;

	Eterm* htop; // heap top
	Eterm* stop; // stack top
	Eterm* heap; // heap start
	Eterm* hend; // heap end
	uint16_t heap_sz; // size of heaps in words

	struct ErlProcess* parent; //parent process
	BeamInstr* i; // program counter
	BeamInstr* cp; // continuation pointer

	xTaskHandle* handle;

	uint8_t active; //is taken from pool?

	// Saved x registers
	// number of live argument registers
	uint8_t arity;
	// argument register (when context switch happens during call to function of arity >= 7)
	Eterm* arg_reg;
	// maxmimum number of registers available
	uint8_t max_arg_reg;
	// default array of argument registers (used when arity <= 6)
	Eterm def_arg_reg[6];

	// Number of reductions left to execute
	int16_t fcalls;
};

typedef struct ErlProcess ErlProcess;

void init_process_table(void);
Eterm erl_create_process(ErlProcess*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void erts_do_exit_process(ErlProcess*, Eterm);

#endif /* ERL_PROCESS_H_ */
