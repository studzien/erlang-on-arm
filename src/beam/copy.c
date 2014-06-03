/*
 * copy.c
 *
 *  Created on: May 25, 2014
 *      Author: Studnicki
 */

#include "copy.h"

unsigned int size_object(Eterm obj) {
	unsigned int sum = 0;

	Eterm *ptr;

	Eterm stack[256];
	Eterm* s_start = stack;
	Eterm* sp = s_start;
	Eterm* s_end = stack+256;

	for(;;) {
		switch primary_tag(obj) {

		case TAG_PRIMARY_LIST:
			sum += 2;
			ptr = list_val(obj);
			obj = *ptr++;
			if(!IS_CONST(obj)) {
				*sp++ = obj;
			}
			obj = *ptr;
			break;

		case TAG_PRIMARY_IMMED1:
			if(sp == s_start) {
				return sum;
			}
			obj = *(--sp);
			break;
		}
	}
}

Eterm copy_struct(Eterm obj, unsigned int sz, Eterm** hpp) {

	//return obj;

	char* hstart;
	unsigned int hsize;
	Eterm *hp;
	Eterm* htop;
	Eterm* hbot;
	Eterm res;
	Eterm *objp;
	Eterm *argp;
	Eterm *tailp;
	Eterm *tp;
	Eterm elem;

	if(IS_CONST(obj)) {
		return obj;
	}

	hp = htop = *hpp;
	hbot = htop + sz;
	hstart = (char*)htop;
	hsize = (char*)hbot-hstart;

	switch(primary_tag(obj)) {

	case TAG_PRIMARY_LIST:
		argp = &res;
		objp = list_val(obj);
		goto L_copy_list;
	}

	L_copy:
	while(hp != htop) {
		obj = *hp;

		switch(primary_tag(obj)) {
		case TAG_PRIMARY_IMMED1:
			hp++;
			break;
		case TAG_PRIMARY_LIST:
			objp = list_val(obj);
			argp = hp++;

			L_copy_list:
			tailp = argp;
			for(;;) {
				tp = tailp;
				elem = CAR(objp);
				if(IS_CONST(elem)) {
					hbot -= 2;
					CAR(hbot) = elem;
					tailp = &CDR(hbot);
				}
				else {
					CAR(htop) = elem;
					tailp = &CDR(htop);
					htop += 2;
				}
				*tp = make_list(tailp-1);
				obj = CDR(objp);
				if(!is_list(obj)) {
					break;
				}
				objp = list_val(obj);
			}
			switch(primary_tag(obj)) {
			case TAG_PRIMARY_IMMED1:
				*tailp = obj;
				goto L_copy;
			}
		}
	}
	*hpp = (Eterm*)(hstart+hsize);
	return res;
}
