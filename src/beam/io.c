/*
 * io.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "io.h"

void dump_stack(ErlProcess* p, Eterm* stop) {
	Eterm* hp;
	char buf[50];
	sprintf(buf, "process %u stack dump (starts at %u):\n", p->id, stop);
	debug(buf);

	for(hp = stop; hp < STACK_START(p); hp++) {
		debug_term_buf(*hp, buf);
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
				sprintf(buf, "NIL");
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
