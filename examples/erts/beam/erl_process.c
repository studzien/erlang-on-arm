/*
 * erl_process.c
 *
 *  Created on: Oct 26, 2013
 *      Author: Studnicki
 */

#include "basic_io.h"
#include "global.h"
#include "erl_process.h"

void ErlProcessTask(void* args) {
	/*static void* op[] = {&&label, &&func_info, &&int_code_end};

	Process *p = (Process*)args;
	p->reds = CONTEXT_REDS;
	byte* cp = p->code;

	//main emulator loop
	emulator_loop:
	if(p->reds <= 0) {
			vPrintString("context switch\n");
			p->reds = CONTEXT_REDS;
			taskYIELD();
	}
	goto *( *(op+*cp++) );

	label:
	vPrintString("label\n");
	goto emulator_loop;

	func_info:
	vPrintString("func_info\n");
	goto emulator_loop;

	int_code_end:
	vPrintString("int_code_end\n");
	goto emulator_loop;*/
}
