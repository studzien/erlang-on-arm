/*
 * erl_arith.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "erl_arith.h"
#include "erl_vm.h"

Eterm erts_mixed_plus(ErlProcess* p, Eterm arg0, Eterm arg1) {
	SInt ires;
	Eterm* hp;
	Eterm res;

	if(is_small(arg0)) {
		if(is_small(arg1)) {
			ires = signed_val(arg0) + signed_val(arg1);
			if(MY_IS_SSMALL(ires)) {
				return make_small(ires);
			}
			else {
				hp = HAlloc(p, 2);
				res = small_to_big(ires, hp);
				return res;
			}
		}
	}

	return THE_NON_VALUE;

}
