/*
 * erl_init.c
 *
 *  Created on: Mar 31, 2014
 *      Author: Studnicki
 */

#include "config.h"
#include "atom.h"
#include "export.h"
#include "beam_emu.h"
#include "erl_arith.h"

#include "modules.h"

extern BeamModule* modules;

extern Eterm x0;

extern ErlProcess* proc_tab;

// heap test
void heap_test(void* p) {
	for(;;) {
		if(uxTaskGetNumberOfTasks() < 3) {
			debug("After processes exited: ");
			debug_32(xPortGetFreeHeapSize());
			break;
		}
		taskYIELD();
	}

	vTaskDelete(NULL);
}


//called when the vm is initialized;
void erl_init() {
	uint8_t i;

	debug_32(xPortGetFreeHeapSize());
	//initialize atom table
	init_atom_table();

	//initialize export table
	init_export_table();

	//initialize BIFs
	erts_init_bif();

	//initialize process table
	init_process_table();

	//init jump table
	process_main(NULL);

	//initialize garbage collector
	erts_init_gc();

	int modules = MODULES_N;

	for(i=0; i<modules; i++) {
		erts_load(code[i]);
	}

	//create the root process
	Export e;
	Export *exported;
	erts_atom_get(ENTRYPOINT_M, ENTRYPOINT_M_LEN, &e.module);
	erts_atom_get(ENTRYPOINT_F, ENTRYPOINT_F_LEN, &e.function);
	e.arity = ENTRYPOINT_ARITY;

	Eterm args = NIL;

	if(ENTRYPOINT_ARITY > 0) {
		Eterm* hp = pvPortMalloc(ENTRYPOINT_ARITY * sizeof(Eterm));
		for(i=0; i<ENTRYPOINT_ARITY; i++) {
			args = CONS(hp, entrypoint_a[i], args);
		}
	}


	debug_32(xPortGetFreeHeapSize());

	erl_create_process(NULL, e.module, e.function, args, NULL);
	//erl_create_process(NULL, e.module, e.function, args, NULL);
	//erl_create_process(NULL, e.module, e.function, args, NULL);
	//erl_create_process(NULL, e.module, e.function, args, NULL);

	//xTaskCreate(heap_test, "heap test",  100, NULL, tskIDLE_PRIORITY, NULL);

	// start the scheduler (cooperative)
	vTaskStartScheduler();

	for( ;; );
}

void erl_exit(char* reason) {
	vTaskSuspendAll();
	char buf[1024];
	vprintf("Erlang VM exited unexpectedly exited with reason: %s", reason);
}
