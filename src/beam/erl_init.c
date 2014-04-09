/*
 * erl_init.c
 *
 *  Created on: Mar 31, 2014
 *      Author: Studnicki
 */

#include "atom.h"
#include "export.h"
#include "beam_emu.h"

extern BeamModule* modules;

extern Eterm x0;

//called when the vm is initialized;
void erl_init() {
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

	byte code[] = FAC2ERL;
	erts_load(code);

	//create the root process
	Export e;
	Export *exported;
	erts_atom_get("fac2", 4, &e.module);
	erts_atom_get("fac", 3, &e.function);
	e.arity = 1;

	debug_32(xPortGetFreeHeapSize());

	erl_create_process(NULL, e.module, e.function, make_small(7), NULL);
	//erl_create_process(NULL, e.module, e.function, make_small(7), NULL);
	//erl_create_process(NULL, e.module, e.function, make_small(4), NULL);

	debug_32(xPortGetFreeHeapSize());


	// start the scheduler (cooperative)
	vTaskStartScheduler();

	for( ;; );
}

void erl_exit(char* reason) {
	vTaskSuspendAll();
	char buf[1024];
	vprintf("Erlang VM exited unexpectedly exited with reason: %s", reason);
}
