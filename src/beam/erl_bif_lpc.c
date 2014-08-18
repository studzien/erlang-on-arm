/*
 * erl_bif_lpc.c
 *
 *  Created on: Jul 16, 2014
 *      Author: Studnicki
 */

#include "erl_bif_lpc.h"
#include "io.h"
#include "erl_interrupt.h"
#include "FreeRTOS.h"

Eterm output_2(ErlProcess* p, Eterm* arg, UInt live) {
	LPC_GPIO_TypeDef* port = gpio_port(unsigned_val(arg[0]));
	port->FIODIR |= (1 << unsigned_val(arg[1]));
	return atom_ok;
}

Eterm high_2(ErlProcess* p, Eterm* arg, UInt live) {
	LPC_GPIO_TypeDef* port = gpio_port(unsigned_val(arg[0]));
	port->FIOSET |= (1 << unsigned_val(arg[1]));
	return atom_ok;
}

Eterm input_2(ErlProcess* p, Eterm* arg, UInt live) {
	LPC_GPIO_TypeDef* port = gpio_port(unsigned_val(arg[0]));
	port->FIODIR &= ~(1 << unsigned_val(arg[1]));
	return atom_ok;
}

Eterm low_2(ErlProcess* p, Eterm* arg, UInt live) {
	LPC_GPIO_TypeDef* port = gpio_port(unsigned_val(arg[0]));
	port->FIOCLR |= (1 << unsigned_val(arg[1]));
	return atom_ok;
}

Eterm interrupt_3(ErlProcess* p, Eterm* arg, UInt live) {
	add_interrupt(p->id, unsigned_val(arg[0]), unsigned_val(arg[1]), arg[2]);
	return atom_ok;
}

Eterm print_term1(ErlProcess* p, Eterm* arg, UInt live) {
	debug_term(arg[0]);
	debug("\n");
	return atom_ok;
}

Eterm print_info0(ErlProcess* p, Eterm* arg, UInt live) {
	char buf[30];
	sprintf(buf, "Process %d info\n", pid2pix(p->id)); debug(buf);
	sprintf(buf, "Heap start:\t%d\n", p->heap);        debug(buf);
	sprintf(buf, "Heap top:\t%d\n", p->htop);		   debug(buf);
	sprintf(buf, "Stack top:\t%d\n", p->stop);		   debug(buf);
	sprintf(buf, "Heap end:\t%d\n", p->hend);		   debug(buf);
	return atom_ok;
}

Eterm dump_stack0(ErlProcess* p, Eterm* arg, UInt live) {
	dump_stack(p, STACK_TOP(p));
	return atom_ok;
}

Eterm dump_regs0(ErlProcess* p, Eterm *arg, UInt live) {
	int i;
	char buf[30];
	for(i=0; i<5; i++) {
		sprintf(buf, "%d\t%d:\n", i, arg[i]);
		debug(buf);
		debug_term(arg[i]);
		debug("\n");
	}
	return atom_ok;
}

Eterm dump_heap0(ErlProcess* p, Eterm* arg, UInt live) {
	dump_heap(p, HEAP_TOP(p));
	return atom_ok;
}

Eterm print_heap_size0(ErlProcess* p, Eterm* arg, UInt live) {
	char buf[30];
	sprintf(buf, "Free heap size: %d\n", xPortGetFreeHeapSize());
	debug(buf);
	return atom_ok;
}

static inline LPC_GPIO_TypeDef* gpio_port(UInt port) {
	return (LPC_GPIO_TypeDef*)(LPC_GPIO_BASE + 0x20*port);
}
