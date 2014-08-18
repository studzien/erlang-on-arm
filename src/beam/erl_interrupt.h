/*
 * erl_interrupt.h
 *
 *  Created on: Aug 17, 2014
 *      Author: Studnicki
 */

#ifndef ERL_INTERRUPT_H_
#define ERL_INTERRUPT_H_

#include "erl_term.h"
#include "FreeRTOS.h"

typedef struct erl_interrupt {
	struct erl_interrupt* next;
	UInt port;
	UInt pin;
	Eterm event;
	Eterm pid;
} ErlInterrupt;

void add_interrupt(Eterm pid, UInt port, UInt pin, Eterm event);
void delete_interrupt(Eterm pid);

#endif /* ERL_INTERRUPT_H_ */
