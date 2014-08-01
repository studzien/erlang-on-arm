/*
 * erl_process.c
 *
 *  Created on: Oct 26, 2013
 *      Author: Studnicki
 */

#include "erl_process.h"
#include "beam_emu.h"
#include "erl_gc.h"

ErlProcess* proc_tab;
uint16_t last_proc;
extern Eterm x0;

extern void* jump_table[];
//BeamInstr to beam_apply and normal_exit ops, needed for apply/3 when spawning
extern BeamInstr beam_apply[];

void init_process_table(void) {
	int i;
	proc_tab = pvPortMalloc(MAX_PROCESSES * sizeof(ErlProcess));
	last_proc = 0;
	for(i=0; i<MAX_PROCESSES; i++) {
		proc_tab[i].active = 0;
	}
}

void erts_do_exit_process(ErlProcess* p, Eterm reason) {
	delete_process(p);
	p->active = 0;
	xTaskHandle handle = *(p->handle);
	vPortFree(p->handle);
	vTaskDelete(handle);
}


void delete_process(ErlProcess* p) {
	// free heap
	vPortFree(HEAP_START(p));

	// free messages
	ErlMessage *next, *msg = p->msg.first;
	while(msg != NULL) {
		if(msg->data) {
			vPortFree(msg->data);
		}
		next = msg->next;
		vPortFree(msg);
		msg = next;
	}

	// clean argument registers
	if(p->arg_reg != p->def_arg_reg) {
		vPortFree(p->arg_reg);
	}
}

//mutexes are not needed here since we have one scheduler and there will be no context switch
//until a process is created or deleted
Eterm erl_create_process(ErlProcess* parent, Eterm module, Eterm function, Eterm args, ErlSpawnOpts* opts) {
	char buf[40];

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

	ErlProcess* p = &proc_tab[last_proc];
	Eterm pid = pix2pid(last_proc);
	p->parent = parent;
	p->handle = handle;
	p->id = pid;

	//@todo throw an error if exported was not found
	p->arity = 3;
	p->arg_reg = p->def_arg_reg;
	p->max_arg_reg = sizeof(p->def_arg_reg)/sizeof(p->def_arg_reg[0]);
	for(i=0; i<3; i++) {
		p->def_arg_reg[i] = 0;
	}
	p->fcalls = REDUCTIONS;
	p->active = 1;

	p->flags = 0;

	p->msg.len = 0;
	p->msg.first = NULL;
	p->msg.last = &p->msg.first;
	p->msg.save = &p->msg.first;
	p->msg.saved_last = NULL;

	// initialize heap
	unsigned int arg_size = size_object(args);
	unsigned int heap_need = arg_size;
	unsigned int sz = erts_next_heap_size(heap_need);

	p->heap = (Eterm*)pvPortMalloc(sz * sizeof(Eterm));
	p->stop = p->hend = p->heap + sz;
	p->htop = p->heap;
	p->heap_sz = sz;

	p->i = (BeamInstr*)(beam_apply);
	p->cp = (BeamInstr*)(beam_apply + 1);
	p->arg_reg[0] = module;
	p->arg_reg[1] = function;
	p->arg_reg[2] = copy_struct(args, arg_size, &p->htop);

	//start process inside the FreeRTOS scheduler
	xTaskCreate(process_main, "erlang process", TASK_STACK_SIZE, (void*)p, 1, p->handle);
	last_proc++;

	return pid;
}

