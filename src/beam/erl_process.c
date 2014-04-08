/*
 * erl_process.c
 *
 *  Created on: Oct 26, 2013
 *      Author: Studnicki
 */

#include "erl_process.h"
#include "beam_emu.h"

ErlProcess* proc_tab;
uint16_t last_proc;
extern Eterm x0;

void init_process_table(void) {
	int i;
	proc_tab = pvPortMalloc(MAX_PROCESSES * sizeof(ErlProcess));
	last_proc = 0;
	for(i=0; i<MAX_PROCESSES; i++) {
		proc_tab[i].active = 0;
	}
}


//@todo mutex on this
Eterm erl_create_process(ErlProcess* parent, Eterm module, Eterm function, Eterm args, ErlSpawnOpts* opts) {
	int i;
	if(last_proc == MAX_PROCESSES) {
		for(i=0; i<MAX_PROCESSES; i++) {
			if(proc_tab[i].active == 0) {
				last_proc = i;
				break;
			}
		}
	}

	if(last_proc == MAX_PROCESSES) {
		erl_exit("maximum number of processes reached!");
		//@todo return NIL;
	}

	xTaskHandle *handle = pvPortMalloc(sizeof(xTaskHandle));

	Export export, *exported;
	export.module = module;
	export.function = function;
	//@todo fix arity when list term is implemented
	export.arity = 1;
	exported = erts_export_get(&export);
	x0 = args;

	Eterm pid = pix2pid(last_proc);
	proc_tab[last_proc].parent = parent;
	proc_tab[last_proc].handle = handle;
	proc_tab[last_proc].id = pid;
	//@todo check what if exported not found
	proc_tab[last_proc].i = exported->address;
	proc_tab[last_proc].active = 1;

	//start process inside the FreeRTOS scheduler
	xTaskCreate(process_main, "erlang process", 240, (void*)&proc_tab[last_proc], 1, handle);
	last_proc++;

	return pid;
}
