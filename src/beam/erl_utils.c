/*
 * erl_utils.c
 *
 *  Created on: Aug 2, 2014
 *      Author: Studnicki
 */

#include "erl_utils.h"
#include "io.h"

int eq(Eterm a, Eterm b) {
	switch(primary_tag(a)) {
	case TAG_PRIMARY_LIST:
	{
		if(is_list(b)) {
			Eterm list_a = a;
			Eterm list_b = b;

			while(list_a != NIL && list_b != NIL) {
				if(!eq(*(list_val(list_a)), *(list_val(list_b)))) {
					return 0;
				}
				list_a = *(list_val(list_a)+1);
				list_b = *(list_val(list_b)+1);
			}
			if(list_a == list_b) {
				return 1;
			}
		}
	}
	break;
	case TAG_PRIMARY_BOXED:
	{
		if(is_boxed(b)) {
			Eterm hdr_a = *boxed_val(a);
			Eterm hdr_b = *boxed_val(b);
			if(hdr_a != hdr_b) {
				return 0;
			}

			int arity = arityval(hdr_a);
			int i;
			for(i=0; i<arity; i++) {
				if(!eq(*(boxed_val(a)+1+i), *(boxed_val(b)+1+i))) {
					return 0;
				}
			}
			return 1;
		}
	}
	break;
	default:
		return a == b;
	}
}
