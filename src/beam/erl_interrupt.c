/*
 * erl_interrupt.c
 *
 *  Created on: Aug 17, 2014
 *      Author: Studnicki
 */


#include "erl_interrupt.h"
#include "io.h"
#include "erl_message.h"

static ErlInterrupt* ints = NULL;
extern ErlProcess* proc_tab;

void add_interrupt(Eterm pid, UInt port, UInt pin, Eterm event) {
	switch(port) {
	case 0:
		switch(event) {
		case atom_falling:
			if(pin < 16) {
				LPC_PINCON->PINSEL0 &= ~(0b11 << 2*pin);
				LPC_PINCON->PINMODE0 &= ~(0b11 << 2*pin);
			}
			else {
				LPC_PINCON->PINSEL1 &= ~(0b11 << 2*(pin-16));
				LPC_PINCON->PINMODE1 &= ~(0b11 << 2*(pin-16));
			}
			LPC_GPIOINT->IO0IntEnF |= (1 << pin);
			ErlInterrupt* interrupt = (ErlInterrupt*)pvPortMalloc(sizeof(ErlInterrupt));
			interrupt->event = event;
			interrupt->pid = pid;
			interrupt->pin = pin;
			interrupt->port = port;
			interrupt->next = ints;
			ints = interrupt;
		}
	}
	NVIC_EnableIRQ(EINT3_IRQn);
}

static void send_interrupts(UInt pin, UInt port, Eterm event) {
	ErlInterrupt* interrupt = ints;
	while(interrupt) {
		if(interrupt->pin == pin && interrupt->port == port && interrupt->event == event) {
			ErlProcess* p = &proc_tab[pid2pix(interrupt->pid)];
			erts_send_message(p, interrupt->pid, atom_interrupt, 1);
		}
		interrupt = interrupt->next;
	}
}

void delete_interrupt(Eterm pid) {
	ErlInterrupt* interrupt = ints;
	ErlInterrupt* prev = NULL;

	while(interrupt) {
		if(interrupt->pid == pid) {
			if(!prev) {
				ints = interrupt->next;
			}
			else {
				prev->next = interrupt->next;
			}
			vPortFree(interrupt);
		}
		else {
			prev = interrupt;
		}
		interrupt = interrupt->next;
	}
}

void EINT3_IRQHandler(void) {
	uint32_t falling = LPC_GPIOINT->IO0IntStatF;
	Eterm event = atom_falling;
	UInt port = 0;
	UInt pin;
	int i;
	for(i=0; i<30; i++) {
		if(i > 11 && i < 15) {
			continue;
		}
		if(falling & (1<<i)) {
			pin = i;
			break;
		}
	}
	send_interrupts(pin, port, event);
	LPC_GPIOINT->IO0IntClr = 0xffff;
	NVIC_ClearPendingIRQ(EINT3_IRQn);
}
