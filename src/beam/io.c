/*
 * io.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "io.h"

void debug_term(Eterm term) {
	char buf[256];
	int i;
	int written;
	Eterm* boxed;


	switch(term & _TAG_PRIMARY_MASK) {

	case TAG_PRIMARY_IMMED1:
		switch(term & _TAG_IMMED1_MASK) {
		case _TAG_IMMED1_SMALL:
			sprintf(buf, "small(%d) ", signed_val(term));
			break;
		default:
			sprintf(buf, "(immed1)");
		}
		break;

	case TAG_PRIMARY_BOXED:
		boxed = boxed_val(term);
		switch(*boxed & _TAG_HEADER_MASK) {

		case _TAG_HEADER_POS_BIG:
			written = sprintf(buf, "big(");
			for(i=0; i<header_arity(*boxed); i++) {
				written += sprintf(buf+written, "%#010x", *(boxed+i+1));
			}
			sprintf(buf+written, ") ");
			break;

		case _TAG_HEADER_NEG_BIG:
			written = sprintf(buf, "big(-");
			for(i=0; i<header_arity(*boxed); i++) {
				written += sprintf(buf+written, "%#010x", *(boxed+i+1));
			}
			sprintf(buf+written, ") ");
			break;


		default:
			sprintf(buf, "(boxed)");
			break;
		}
		break;

	default:
		sprintf(buf, "(not recognized) ");
	}

	debug(buf);
}
