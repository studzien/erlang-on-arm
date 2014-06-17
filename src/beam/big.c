/*
 * big.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "big.h"
#include "global.h"

Eterm small_to_big(SInt x, Eterm *y) {
	if(x >= 0) {
		*y = make_pos_bignum_header(1);
	}
	else {
		x = -x;
		*y = make_neg_bignum_header(1);
	}

	BIG_DIGIT(y, 0) = x;
	return make_big(y);
}
