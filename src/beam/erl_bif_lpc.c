/*
 * erl_bif_lpc.c
 *
 *  Created on: Jul 16, 2014
 *      Author: Studnicki
 */

#include "erl_bif_lpc.h"
#include "io.h"

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

Eterm low_2(ErlProcess* p, Eterm* arg, UInt live) {
	LPC_GPIO_TypeDef* port = gpio_port(unsigned_val(arg[0]));
	port->FIOCLR |= (1 << unsigned_val(arg[1]));
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
	sprintf(buf, "Heap end:\t%d\n", p->hend);		   debug(buf);
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
