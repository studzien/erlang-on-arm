/*
 * erl_init.c
 *
 *  Created on: Mar 31, 2014
 *      Author: Studnicki
 */

#include "atom.h"
#include "export.h"

extern BeamModule* modules;

//called when the vm is initialized;
void erl_init() {
	debug_32(xPortGetFreeHeapSize());
	//initialize atom table
	init_atom_table();

	//initialize export table
	init_export_table();

	byte code[] = FAC2ERL;
	erts_load(code);

	//dump_atoms();
	BeamModule module = modules[0];
	char buf[256];
	int i;
	debug("code\n");
	for(i=0; i<module.size; i++) {
		sprintf(buf, "%d\n", (BeamInstr)module.code[i]);
		debug(buf);
	}

	debug_32(xPortGetFreeHeapSize());
	// start the scheduler
	vTaskStartScheduler();

	for( ;; );
}

void erl_exit(char* reason) {
	vTaskSuspendAll();
	char buf[1024];
	vprintf("Erlang VM exited unexpectedly exited with reason: %s", reason);
}