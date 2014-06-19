/*
 * erl_arith.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "erl_arith.h"
#include "erl_vm.h"
#include "big.h"

Eterm erts_mixed_times(ErlProcess* p, Eterm arg0, Eterm arg1) {
	SInt ires;
	Eterm* hp;
	Eterm res;
	Eterm hdr;

	Eterm tmp_big1[2];
	Eterm tmp_big2[2];

	if(is_small(arg0)) {
		if(is_small(arg1)) {
			// SMALL*SMALL
			if((arg0 == SMALL_ZERO) || (arg1 == SMALL_ZERO)) {
				return SMALL_ZERO;
			}
			if(arg1 == SMALL_ONE) {
				return arg0;
			}
			if(arg0 == SMALL_ONE) {
				return arg1;
			}
			Eterm big_res[3];
			res = small_times(signed_val(arg0), signed_val(arg1), big_res);
			if(is_small(res)) {
				// result is also SMALL
				return res;
			}
			else {
				// result is BIG
				hdr = big_res[0];
				UInt arity = bignum_header_arity(hdr);
				hp = HAlloc(p, BIG_NEED_SIZE(arity));
				res = make_big(hp);
				*hp++ = hdr;
				*hp++ = big_res[1];
				if(arity > 1) {
					*hp = big_res[2];
				}
				return res;
			}
		}
		else if(is_big(arg1)) {
			if(arg0 == SMALL_ZERO) {
				return SMALL_ZERO;
			}
			if(arg0 == SMALL_ONE) {
				return arg1;
			}
			arg0 = small_to_big(signed_val(arg0), tmp_big1);
			return erts_big_times(p, arg0, arg1);
		}
	}
	else if(is_big(arg0)) {
		if(is_small(arg1)) {
			if(arg1 == SMALL_ZERO) {
				return SMALL_ZERO;
			}
			if(arg1 == SMALL_ONE) {
				return arg0;
			}
			arg1 = small_to_big(signed_val(arg1), tmp_big2);
			return erts_big_times(p, arg0, arg1);
		}
		else if(is_big(arg1)) {
			return erts_big_times(p, arg0, arg1);
		}
	}

	return THE_NON_VALUE;

}

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

Eterm erts_mixed_minus(ErlProcess* p, Eterm arg0, Eterm arg1) {
	SInt ires;
	Eterm* hp;
	Eterm res;

	Eterm tmp_big1[2];
	Eterm tmp_big2[2];

	if(is_small(arg0)) {
		if(is_small(arg1)) {
			// SMALL-SMALL
			ires = signed_val(arg0) - signed_val(arg1);
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
			// SMALL - BIG
			arg0 = small_to_big(signed_val(arg0), tmp_big1);
			return erts_big_minus(p, arg0, arg1);
		}
	}
	else if(is_big(arg0)) {
		if(is_small(arg1)) {
			// BIG - ZERO
			if(arg1 == SMALL_ZERO) {
				return arg0;
			}
			// BIG - SMALL
			arg1 = small_to_big(signed_val(arg1), tmp_big2);
			return erts_big_minus(p, arg0, arg1);
		}
		//BIG + BIG
		else if(is_big(arg1)) {
			return erts_big_minus(p, arg0, arg1);
		}
	}

	return THE_NON_VALUE;

}

static inline void maybe_shrink(ErlProcess* p, Eterm* hp, Eterm res, UInt alloc)
{
	UInt actual;

	if (is_immed(res)) {
		if (p->heap <= hp && hp < p->htop) {
			p->htop = hp;
		}
	} else if ((actual = BIG_NEED_SIZE(bignum_header_arity(*hp))) < alloc) {
		if (p->heap <= hp && hp < p->htop) {
			p->htop = hp+actual;
		}
	}
}

Eterm erts_big_times(ErlProcess* p, Eterm arg0, Eterm arg1) {
	UInt size = big_size(arg0) + big_size(arg1);
	UInt need_heap = BIG_NEED_SIZE(size);

	Eterm *hp = HAlloc(p, need_heap);
	Eterm res = big_times(arg0, arg1, hp);
	maybe_shrink(p, hp, res, need_heap);

	return res;
}

Eterm erts_big_plus(ErlProcess* p, Eterm arg0, Eterm arg1) {
	UInt size = MAX(big_size(arg0), big_size(arg1))+1;
	UInt need_heap = BIG_NEED_SIZE(size);

	Eterm *hp = HAlloc(p, need_heap);
	Eterm res = big_plus(arg0, arg1, hp);
	maybe_shrink(p, hp, res, need_heap);

	return res;
}

Eterm erts_big_minus(ErlProcess* p, Eterm arg0, Eterm arg1) {
	UInt size = MAX(big_size(arg0), big_size(arg1))+1;
	UInt need_heap = BIG_NEED_SIZE(size);

	Eterm *hp = HAlloc(p, need_heap);
	Eterm res = big_minus(arg0, arg1, hp);
	maybe_shrink(p, hp, res, need_heap);

	return res;
}
