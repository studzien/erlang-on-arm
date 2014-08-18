/*
 * io.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "io.h"

extern ErlProcess* proc_tab;
extern UInt reclaimed;
extern UInt garbage_cols;
extern UInt tiw_nto;
extern int sent;
extern int received;
extern int suspended;
extern int resumed;
extern int continued;
extern int spawns;
extern int exits;

void print_stats() {
	Timeval t;
	erts_get_now(&t);

	UInt heap_total = 0;
	UInt processes = 0;
	int i;
	for(i=0; i<MAX_PROCESSES; i++) {
		if(proc_tab[i].active) {
			processes++;
			heap_total += HEAP_SIZE(&proc_tab[i]);
		}
	}

	char buf[50];
	sprintf(buf, "%d;", t.sec); debug(buf);
	sprintf(buf, "%d;", processes); debug(buf);
	sprintf(buf, "%d;", xPortGetFreeHeapSize()); debug(buf);
	sprintf(buf, "%d;", heap_total); debug(buf);
	sprintf(buf, "%d;", reclaimed); debug(buf);
	sprintf(buf, "%d;", garbage_cols); debug(buf);
	sprintf(buf, "%d;", tiw_nto); debug(buf);
	sprintf(buf, "%d;", sent); debug(buf);
	sprintf(buf, "%d;", received); debug(buf);
	sprintf(buf, "%d\n", spawns); debug(buf);
}

void dump_stack(ErlProcess* p, Eterm* stop) {
	Eterm* hp;
	char buf[50];
	sprintf(buf, "process %u stack dump (starts at %u):\n", p->id, stop);
	debug(buf);

	for(hp = stop; hp < STACK_START(p); hp++) {
		debug_term_buf(*hp, buf);
		debug("\n");
	}

	debug("\n");
}

void dump_heap(ErlProcess* p, Eterm* htop) {
	Eterm* hp;
	char buf[50];
	sprintf(buf, "process %u heap dump (starts at %u):\n", p->id, HEAP_START(p));
	debug(buf);

	for(hp = htop-1; hp >= HEAP_START(p); hp--) {
		debug_32(hp);
		debug_term_buf(*hp, buf);
		debug("\n");
	}

	debug("\n");
}

void dump_registers(Eterm* reg) {
	Eterm* hp;
	char buf[50];
	sprintf(buf, "registers dump:\n");
	debug(buf);
	int i;

	for(i=0; i<3; i++) {
		sprintf(buf, "%d: ", i);
		debug(buf);
		debug_term_buf(reg[i], buf);
		debug("\n");
	}

	debug("\n");
}


void debug_term(Eterm term) {
	char buf[50];
	return debug_term_buf(term, buf);
}

void debug_term_buf(Eterm term, char* buf) {
	int i;
	Eterm* boxed;
	Eterm* elem;

	switch(term & _TAG_PRIMARY_MASK) {

	case TAG_PRIMARY_IMMED1:
		switch(term & _TAG_IMMED1_MASK) {
		case _TAG_IMMED1_SMALL:
			sprintf(buf, "small (%d) ", signed_val(term));
			debug(buf);
			break;
		case _TAG_IMMED1_PID:
			sprintf(buf, "pid(%d)", unsigned_val(term));
			debug(buf);
			break;
		case _TAG_IMMED1_IMMED2:
			switch(term & _TAG_IMMED2_MASK) {
			case _TAG_IMMED2_ATOM:
				sprintf(buf, "atom(%d)", atom_val(term));
				debug(buf);
				break;
			case _TAG_IMMED2_NIL:
				sprintf(buf, "NIL ");
				debug(buf);
				break;
			default:
				sprintf(buf, "(immed2)");
				debug(buf);
			}
			break;
		default:
			sprintf(buf, "(immed1)");
			debug(buf);
		}
		break;


	case TAG_PRIMARY_LIST:
		sprintf(buf, "list @%d[", list_val(term));
		debug(buf);
		while(term!=NIL) {
			elem = list_val(term);
			sprintf(buf, "@%d:", elem);
			debug(buf);
			debug_term_buf(*elem, buf);
			debug("\n");
			term = (*(elem+1));
		}
		sprintf(buf, "]");
		debug(buf);
		break;

	case TAG_PRIMARY_BOXED:
		boxed = boxed_val(term);
		switch(*boxed & _TAG_HEADER_MASK) {

		case _TAG_HEADER_POS_BIG:
			sprintf(buf, "big(+ ", boxed);
			debug(buf);
			for(i=0; i<header_arity(*boxed); i++) {
				sprintf(buf, "%#010x ", *(boxed+i+1));
				debug(buf);
			}
			sprintf(buf, ") ");
			debug(buf);
			break;

		case _TAG_HEADER_NEG_BIG:
			sprintf(buf, "big(- ", boxed);
			debug(buf);
			for(i=0; i<header_arity(*boxed); i++) {
				sprintf(buf, "%#010x ", *(boxed+i+1));
				debug(buf);
			}
			sprintf(buf, ") ");
			debug(buf);
			break;

		case _TAG_HEADER_ARITYVAL:
			sprintf(buf, "tuple[@%d/%d]{", boxed, header_arity(*boxed));
			debug(buf);
			for(i=0; i<header_arity(*boxed); i++) {
				debug_term_buf(*(boxed+i+1), buf);
				sprintf(buf, " ");
				debug(buf);
			}
			sprintf(buf, "} ");
			debug(buf);
			break;

		default:
			sprintf(buf, "(boxed %d)", term);
			debug(buf);
			break;
		}
		break;

	default:
		sprintf(buf, "(not recognized %u) ", term);
		debug(buf);
	}
}
