/*
 * erl_init.c
 *
 *  Created on: Mar 31, 2014
 *      Author: Studnicki
 */

#include "atom.h"
#include "export.h"
#include "beam_emu.h"
#include "erl_arith.h"

#include "modules.h"

extern BeamModule* modules;

extern Eterm x0;

extern ErlProcess* proc_tab;

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

	debug_32(xPortGetFreeHeapSize());

	Eterm args = NIL;

	if(ENTRYPOINT_ARITY > 0) {
		Eterm* hp = pvPortMalloc(ENTRYPOINT_ARITY * sizeof(Eterm));
		for(i=0; i<ENTRYPOINT_ARITY; i++) {
			args = CONS(hp, entrypoint_a[i], args);
		}
	}

	erl_create_process(NULL, e.module, e.function, args, NULL);
	//erl_create_process(NULL, e.module, e.function, args, NULL);
	//erl_create_process(NULL, e.module, e.function, args, NULL);
/*
	Eterm small1 = make_small(0x7FFFFFF);
	//debug_term(small1);

	ErlProcess p = proc_tab[pid2pix(pid)];
	//Eterm big = erts_mixed_plus(&p, small1, small1);
	//sprintf(buf, "big: %u %u\n", big, *boxed_val(big));
	//debug(buf);
	//debug_term(big);

	//Eterm big2 = erts_mixed_plus(&p, big, big);
	//debug_term(big2);

	//Eterm big3 = erts_mixed_minus(&p, big2, big);
	//debug_term(big3);

	//Eterm zero = erts_mixed_minus(&p, big2, big2);
	//debug_term(zero);

	Eterm five = make_small(5);
	Eterm big1 = erts_mixed_times(&p, small1, five);
	debug_term(big1);

	Eterm big2 = erts_mixed_times(&p, big1, big1);
	debug_term(big2);

	debug_32(xPortGetFreeHeapSize());

*/


	// start the scheduler (cooperative)
	vTaskStartScheduler();

	for( ;; );
}

void erl_exit(char* reason) {
	vTaskSuspendAll();
	char buf[1024];
	vprintf("Erlang VM exited unexpectedly exited with reason: %s", reason);
}
