/*
 * erl_arith.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "erl_arith.h"
#include "erl_vm.h"
#include "big.h"

Eterm erts_mixed_plus(ErlProcess* p, Eterm arg0, Eterm arg1) {
	SInt ires;
	Eterm* hp;
	Eterm res;

	Eterm tmp_big1[2];
	Eterm tmp_big2[2];

	if(is_small(arg0)) {
		if(is_small(arg1)) {
			// SMALL+SMALL
			ires = signed_val(arg0) + signed_val(arg1);
			// result is also SMALL
			if(MY_IS_SSMALL(ires)) {
				return make_small(ires);
			}
			// result is BIG
			else {
				hp = HAlloc(p, 2);
				res = small_to_big(ires, hp);
				return res;
			}
		}
		else if(is_big(arg1)) {
			// ZERO + BIG
			if(arg0 == SMALL_ZERO) {
				return arg1;
			}
			// SMALL + BIG
			arg0 = small_to_big(signed_val(arg0), tmp_big1);
			return erts_big_plus(p, arg0, arg1);
		}
	}
	else if(is_big(arg0)) {
		if(is_small(arg1)) {
			// BIG + ZERO
			if(arg1 == SMALL_ZERO) {
				return arg0;
			}
			// BIG + SMALL
			arg1 = small_to_big(signed_val(arg1), tmp_big2);
			return erts_big_plus(p, arg0, arg1);
		}
		//BIG + BIG
		else if(is_big(arg1)) {
			return erts_big_plus(p, arg0, arg1);
		}
	}

	return THE_NON_VALUE;

}

Eterm erts_big_plus(ErlProcess* p, Eterm arg0, Eterm arg1) {
	UInt size = MAX(big_size(arg0), big_size(arg1))+1;
	UInt need_heap = BIG_NEED_SIZE(size);

	Eterm *hp = HAlloc(p, need_heap);
	Eterm res = big_plus(arg0, arg1, hp);

	return res;

}
