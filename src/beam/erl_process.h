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
#include "atom.h"
#include "erl_time.h"
#include "erl_message.h"

#define HEAP_START(p)    (p)->heap
#define HEAP_TOP(p)      (p)->htop
#define HEAP_LIMIT(p)    (p)->stop
#define HEAP_END(p)		 (p)->hend
#define HEAP_SIZE(p)     (p)->heap_sz
#define STACK_START(p)   (p)->hend
#define STACK_TOP(p)	 (p)->stop
#define STACK_END(p)     (p)->htop
#define MBUF(p)          (p)->mbuf

#define ERTS_HEAP_FRAG_SIZE(DATA_WORDS) \
		(sizeof(ErlHeapFragment) - sizeof(Eterm) + (DATA_WORDS)*sizeof(Eterm))

void ErlProcessTask(void* args);

typedef struct {

} ErlSpawnOpts;

// Process flags
#define F_INSLPQUEUE         (1 <<  0) /* Set if in timer queue */
#define F_TIMO               (1 <<  1) /* Set if timeout */

struct ErlProcess {
	Eterm id;

	Eterm* htop; // heap top
	Eterm* stop; // stack top
	Eterm* heap; // heap start
	Eterm* hend; // heap end
	uint16_t heap_sz; // size of heaps in words

	struct ErlProcess* parent; //parent process
	BeamInstr* i; // program counter
	BeamInstr* saved_i; // saved program counter
	BeamInstr* cp; // continuation pointer

	xTaskHandle* handle;

	uint8_t active; //is taken from pool?

	// Saved x registers
	// number of live argument registers
	uint8_t arity;
	// argument register (when context switch happens during call to function of arity >= 4)
	Eterm* arg_reg;
	// maxmimum number of registers available
	uint8_t max_arg_reg;
	// default array of argument registers (used when arity <= 3)
	Eterm def_arg_reg[3];

	// Number of reductions left to execute
	int16_t fcalls;

	// Message queue
	ErlMessageQueue msg;

	// Timer
	ErlTimer timer;

	// Flags
	uint8_t flags;
};

typedef struct ErlProcess ErlProcess;

void init_process_table(void);
Eterm erl_create_process(ErlProcess*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void erts_do_exit_process(ErlProcess*, Eterm);
static void delete_process(ErlProcess*);

#endif /* ERL_PROCESS_H_ */
