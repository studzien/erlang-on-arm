/*
 * big.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "big.h"
#include "global.h"


static Eterm big_norm(Eterm *x, UInt xl, short sign)
{
	if (xl == 1) {
		UInt y = BIG_DIGIT(x, 0);

		if (D_EXP < SMALL_BITS || IS_USMALL(sign, y)) {
			if (sign)
				return make_small(-((SInt)y));
			else
				return make_small(y);
		}
	}


	if (sign) {
		*x = make_neg_bignum_header(xl);
	}
	else {
		*x = make_pos_bignum_header(xl);
	}

	return make_big(x);
}

static int I_comp(ErtsDigit* x, UInt xl, ErtsDigit* y, UInt yl)
{
	if (xl < yl)
		return -1;
	else if (xl > yl)
		return 1;
	else {
		if (x == y)
			return 0;
		x += (xl-1);
		y += (yl-1);
		while((xl > 0) && (*x == *y)) {
			x--;
			y--;
			xl--;
		}
		if (xl == 0)
			return 0;
		return (*x < *y) ? -1 : 1;
	}
}

static UInt I_add(ErtsDigit* x, UInt xl, ErtsDigit* y, UInt yl, ErtsDigit* r)
{
	UInt sz = xl;
	register ErtsDigit yr, xr;
	register ErtsDigit c = 0;

	xl -= yl;
	do {
		xr = *x++ + c;
		yr = *y++;
		c = (xr < c);
		xr = yr + xr;
		c += (xr < yr);
		*r++ = xr;
	} while(--yl);

	while(xl--) {
		xr = *x++ + c;
		c = (xr < c);
		*r++ = xr;
	}
	if (c) {
		*r = 1;
		return sz+1;
	}
	return sz;
}

static UInt I_sub(ErtsDigit* x, UInt xl, ErtsDigit* y, UInt yl, ErtsDigit* r)
{
	ErtsDigit* r0 = r;
	register ErtsDigit yr, xr;
	register ErtsDigit c = 0;

	xl -= yl;
	do {
		yr = *y++ + c;
		xr = *x++;
		c = (yr < c);
		yr = xr - yr;
		c += (yr > xr);
		*r++ = yr;
	} while(--yl);

	while(xl--) {
		xr = *x++;
		yr = xr - c;
		c = (yr > xr);
		*r++ = yr;
	}
	do {
		r--;
	} while(*r == 0 && r != r0);

	return (r - r0) + 1;
}

static Eterm big_plus_minus(ErtsDigit* x, UInt xl, short xsgn,
							ErtsDigit* y, UInt yl, short ysgn,
							Eterm* r) {
    if (xsgn == ysgn) {
    if (xl > yl)
        return big_norm(r, I_add(x,xl,y,yl,BIG_V(r)), xsgn);
    else
        return big_norm(r, I_add(y,yl,x,xl,BIG_V(r)), xsgn);
    }
    else {
    int comp = I_comp(x, xl, y, yl);
    if (comp == 0)
        return make_small(0);
    else if (comp > 0)
        return big_norm(r, I_sub(x,xl,y,yl,BIG_V(r)), xsgn);
    else
        return big_norm(r, I_sub(y,yl,x,xl,BIG_V(r)), ysgn);
    }
}


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

Eterm big_plus(Eterm x, Eterm y, Eterm* r) {
	Eterm *xp = big_val(x);
	Eterm *yp = big_val(y);

    return big_plus_minus(BIG_V(xp),BIG_SIZE(xp),(short) BIG_SIGN(xp),
            BIG_V(yp),BIG_SIZE(yp),(short) BIG_SIGN(yp), r);
}
