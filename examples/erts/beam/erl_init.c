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

//called when the vm is initialized;
void erl_init() {
	debug_32(xPortGetFreeHeapSize());
	//initialize atom table
	init_atom_table();

	//initialize export table
	init_export_table();

	//init jump table
	go(NULL);


	byte code[] = FAC2ERL;
	erts_load(code);

	//dump_atoms();
	/*BeamModule module = modules[0];
	char buf[256];
	int i;
	debug("code\n");
	for(i=0; i<module.size; i++) {
		sprintf(buf, "%d\n", (BeamInstr)module.code[i]);
		debug(buf);
	}*/

	Export e;
	Export *exported;
	erts_atom_get("fac2", 4, &e.module);
	erts_atom_get("fac", 3, &e.function);
	e.arity = 1;

	char buf[256];
	sprintf(buf, "%d %d %d\n", e.module, e.function, e.arity);
	debug(buf);

	exported = erts_export_get(&e);
	if(exported == NULL) {
		debug("exported is null!\n");
	}
	else {
		go(exported->address);
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
