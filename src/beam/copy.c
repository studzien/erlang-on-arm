/*
 * copy.c
 *
 *  Created on: May 25, 2014
 *      Author: Studnicki
 */

#include "copy.h"
#include "io.h"
#include "erl_gc.h"

unsigned int size_object(Eterm obj) {
	unsigned int sum = 0;

	Eterm *ptr;

	Eterm stack[256];
	Eterm* s_start = stack;
	Eterm* sp = s_start;
	Eterm* s_end = stack+255;
	Eterm hdr;
	int arity;

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

		case TAG_PRIMARY_BOXED:
			 hdr = *boxed_val(obj);
			switch(hdr & _TAG_HEADER_MASK) {
			case ARITYVAL_SUBTAG:
				ptr = tuple_val(obj);
				arity = header_arity(hdr);
				sum += arity+1;
				if(arity == 0) {
					goto pop_next;
				}
				while(arity-- > 1) {
					obj = *++ptr;
					if(!IS_CONST(obj)) {
						*sp++ = obj;
					}
				}
				obj = *ptr++;
				break;
			default:
				sum += arityval(hdr) + 1;
				goto pop_next;
			}

		case TAG_PRIMARY_IMMED1:
		pop_next:
			if(sp == s_start) {
				return sum;
			}
			obj = *(--sp);
			break;
		}
	}
}


Eterm copy_struct(Eterm obj, UInt sz, Eterm** hpp)
{
	char* hstart;
	UInt hsize;
	Eterm* htop;
	Eterm* hbot;
	Eterm* hp;
	Eterm* objp;
	Eterm* tp;
	Eterm  res;
	Eterm  elem;
	Eterm* tailp;
	Eterm* argp;
	Eterm* const_tuple;
	Eterm hdr;
	int i;

	if (IS_CONST(obj))
		return obj;

	hp = htop = *hpp;
	hbot   = htop + sz;
	hstart = (char *)htop;
	hsize = (char*) hbot - hstart;
	const_tuple = 0;

	/* Copy the object onto the heap */
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST:
		argp = &res;
		objp = list_val(obj);
		goto L_copy_list;
	case TAG_PRIMARY_BOXED: argp = &res; goto L_copy_boxed;
	}

	L_copy:
	while (hp != htop) {
		obj = *hp;

		switch (primary_tag(obj)) {
		case TAG_PRIMARY_IMMED1:
			hp++;
			break;
		case TAG_PRIMARY_LIST:
			objp = list_val(obj);
			if (in_area(objp,hstart,hsize)) {
				hp++;
				break;
			}
			argp = hp++;
			/* Fall through */

			L_copy_list:
			tailp = argp;
			for (;;) {
				tp = tailp;
				elem = CAR(objp);
				if (IS_CONST(elem)) {
					hbot -= 2;
					CAR(hbot) = elem;
					tailp = &CDR(hbot);
				}
				else {
					CAR(htop) = elem;
					tailp = &CDR(htop);
					htop += 2;
				}
				*tp = make_list(tailp - 1);
				obj = CDR(objp);
				if (!is_list(obj)) {
					break;
				}
				objp = list_val(obj);
			}
			switch (primary_tag(obj)) {
			case TAG_PRIMARY_IMMED1: *tailp = obj; goto L_copy;
			case TAG_PRIMARY_BOXED: argp = tailp; goto L_copy_boxed;
			}
			case TAG_PRIMARY_BOXED:
				if (in_area(boxed_val(obj),hstart,hsize)) {
					hp++;
					break;
				}
				argp = hp++;

				L_copy_boxed:
				objp = boxed_val(obj);
				hdr = *objp;
				switch (hdr & _TAG_HEADER_MASK) {
				case ARITYVAL_SUBTAG:
				{
					int const_flag = 1; /* assume constant tuple */
					i = arityval(hdr);
					*argp = make_tuple(htop);
					tp = htop;	/* tp is pointer to new arity value */
					*htop++ = *objp++; /* copy arity value */
					while (i--) {
						elem = *objp++;
						if (!IS_CONST(elem)) {
							const_flag = 0;
						}
						*htop++ = elem;
					}
					if (const_flag) {
						const_tuple = tp; /* this is the latest const_tuple */
					}
				}
				}
				break;
				case TAG_PRIMARY_HEADER:
					if (header_is_thing(obj) || hp == const_tuple) {
						hp += header_arity(obj) + 1;
					} else {
						hp++;
					}
					break;
				}
	}

	*hpp = (Eterm *) (hstart+hsize);
	return res;
}
