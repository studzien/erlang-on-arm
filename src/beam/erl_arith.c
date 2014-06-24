/*
 * erl_arith.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "erl_arith.h"
#include "erl_vm.h"
#include "big.h"


static inline void maybe_shrink(ErlProcess* p, Eterm* hp, Eterm res, UInt alloc)
{
	UInt actual;

	if (is_immed(res)) {
		if (p->heap <= hp && hp < p->htop) {
			p->htop = hp;
		}
		else {
			erts_heap_frag_shrink(p, hp);
		}
	} else if ((actual = BIG_NEED_SIZE(bignum_header_arity(*hp))) < alloc) {
		if (p->heap <= hp && hp < p->htop) {
			p->htop = hp+actual;
		}
		else {
			erts_heap_frag_shrink(p, hp+actual);
		}
	}
}

static inline void trim_heap(ErlProcess* p, Eterm* hp, Eterm res)
{
	UInt actual;

	if (is_immed(res)) {
		HEAP_TOP(p) = hp;
	}
	else {
		Eterm *new_htop;
		new_htop = hp + bignum_header_arity(*hp) + 1;
		HEAP_TOP(p) = new_htop;
	}
}


// gc bifs are not allowed to allocate heap fragments
#define erts_heap_frag_shrink horrible error
#define maybe_shrink horrible error

Eterm erts_gc_mixed_times(ErlProcess* p, Eterm* reg, UInt live) {
	Eterm arg0 = reg[live];
	Eterm arg1 = reg[live+1];

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
				UInt need = BIG_NEED_SIZE(arity);

				if(ERTS_NEED_GC(p, need)) {
					erts_garbage_collect(p, need, reg, live);
				}

				hp = HEAP_TOP(p);
				HEAP_TOP(p) += need;
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
			return erts_gc_big_times(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
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
			return erts_gc_big_times(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
		}
		else if(is_big(arg1)) {
			return erts_gc_big_times(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
		}
	}

	return THE_NON_VALUE;

}

Eterm erts_gc_mixed_plus(ErlProcess* p, Eterm* reg, UInt live) {
	Eterm arg0 = reg[live];
	Eterm arg1 = reg[live+1];

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
				if(ERTS_NEED_GC(p, 2)) {
					erts_garbage_collect(p, 2, reg, live);
				}
				hp = HEAP_TOP(p);
				HEAP_TOP(p) += 2;
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
			return erts_gc_big_plus(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
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
			return erts_gc_big_plus(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
		}
		//BIG + BIG
		else if(is_big(arg1)) {
			return erts_gc_big_plus(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
		}
	}

	return THE_NON_VALUE;

}

Eterm erts_gc_mixed_minus(ErlProcess* p, Eterm* reg, UInt live) {
	Eterm arg0 = reg[live];
	Eterm arg1 = reg[live+1];

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
				if(ERTS_NEED_GC(p, 2)) {
					erts_garbage_collect(p, 2, reg, live);
				}

				hp = HEAP_TOP(p);
				HEAP_TOP(p) += 2;
				res = small_to_big(ires, hp);
				return res;
			}
		}
		else if(is_big(arg1)) {
			// SMALL - BIG
			arg0 = small_to_big(signed_val(arg0), tmp_big1);
			return erts_gc_big_minus(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
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
			return erts_gc_big_minus(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
		}
		//BIG + BIG
		else if(is_big(arg1)) {
			return erts_gc_big_minus(p, arg0, arg1, tmp_big1, tmp_big2, reg, live);
		}
	}

	return THE_NON_VALUE;

}


Eterm erts_gc_big_times(ErlProcess* p, Eterm arg0, Eterm arg1,
						Eterm* tmp_big0, Eterm* tmp_big1, Eterm* reg, UInt live) {
	UInt size = big_size(arg0) + big_size(arg1);
	UInt need_heap = BIG_NEED_SIZE(size);

	if(ERTS_NEED_GC(p, need_heap)) {
		erts_garbage_collect(p, need_heap, reg, live+2);
		if(arg0 != make_big(tmp_big0)) {
			arg0 = reg[live];
		}
		if(arg1 != make_big(tmp_big1)) {
			arg1 = reg[live+1];
		}
	}

	Eterm *hp = HEAP_TOP(p);
	HEAP_TOP(p) += need_heap;

	Eterm res = big_times(arg0, arg1, hp);
	trim_heap(p, hp, res);

	return res;
}

Eterm erts_gc_big_plus(ErlProcess* p, Eterm arg0, Eterm arg1,
					  Eterm* tmp_big0, Eterm* tmp_big1, Eterm* reg, UInt live) {
	UInt size = MAX(big_size(arg0), big_size(arg1))+1;
	UInt need_heap = BIG_NEED_SIZE(size);

	if(ERTS_NEED_GC(p, need_heap)) {
		erts_garbage_collect(p, need_heap, reg, live+2);
		if(arg0 != make_big(tmp_big0)) {
			arg0 = reg[live];
		}
		if(arg1 != make_big(tmp_big1)) {
			arg1 = reg[live+1];
		}
	}

	Eterm *hp = HEAP_TOP(p);
	HEAP_TOP(p) += need_heap;

	Eterm res = big_plus(arg0, arg1, hp);
	trim_heap(p, hp, res);

	return res;
}

Eterm erts_gc_big_minus(ErlProcess* p, Eterm arg0, Eterm arg1,
						Eterm* tmp_big0, Eterm* tmp_big1, Eterm* reg, UInt live) {
	UInt size = MAX(big_size(arg0), big_size(arg1))+1;
	UInt need_heap = BIG_NEED_SIZE(size);

	if(ERTS_NEED_GC(p, need_heap)) {
		erts_garbage_collect(p, need_heap, reg, live+2);
		if(arg0 != make_big(tmp_big0)) {
			arg0 = reg[live];
		}
		if(arg1 != make_big(tmp_big1)) {
			arg1 = reg[live+1];
		}
	}

	Eterm *hp = HEAP_TOP(p);
	HEAP_TOP(p) += need_heap;

	Eterm res = big_minus(arg0, arg1, hp);
	trim_heap(p, hp, res);

	return res;
}
