/*
 * big.c
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#include "big.h"
#include "global.h"

/*
 ** Multiply smallnums
 */

Eterm small_times(SInt x, SInt y, Eterm *r)
{
	short sign = (x<0) != (y<0);
	ErtsDigit xu = (x > 0) ? x : -x;
	ErtsDigit yu = (y > 0) ? y : -y;
	ErtsDigit d1=0;
	ErtsDigit d0;
	UInt arity;

	DMULc(xu, yu, d1, d0);

	if (!d1 && ((D_EXP < SMALL_BITS) || IS_USMALL(sign, d0))) {
		if (sign)
			return make_small(-((SInt)d0));
		else
			return make_small(d0);
	}

	BIG_DIGIT(r,0) = d0;
	arity = d1 ? 2 : 1;
	if (sign)
		*r = make_neg_bignum_header(arity);
	else
		*r = make_pos_bignum_header(arity);
	if (d1)
		BIG_DIGIT(r,1) = d1;
	return make_big(r);
}


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

/*
 ** Multiply digits d with digits in x and store in r
 */
static UInt D_mul(ErtsDigit* x, UInt xl, ErtsDigit d, ErtsDigit* r)
{
	ErtsDigit c = 0;
	UInt rl = xl;
	ErtsDigit p;

	switch(d) {
	case 0:
		ZERO_DIGITS(r, 1);
		return 1;
	case 1:
		if (x != r)
			MOVE_DIGITS(r, x, xl);
		return xl;
	case 2:
		while(xl--) {
			p = *x;
			DSUMc(p, p, c, p);
			*r++ = p;
			x++;
		}
		break;
	default:
		while(xl--) {
			DMULc(d, *x, c, p);
			*r++ = p;
			x++;
		}
		break;
	}
	if (c == 0)
		return rl;
	*r = c;
	return rl+1;
}


/*
 ** Multiply digits in x with digits in y and store in r
 ** Assumption: digits in r must be 0 (upto the size of x)
 */
static UInt I_mul(ErtsDigit* x, UInt xl, ErtsDigit* y, UInt yl, ErtsDigit* r)
{
	ErtsDigit* r0 = r;
	ErtsDigit* rt = r;

	while(xl--) {
		ErtsDigit cp = 0;
		ErtsDigit c = 0;
		UInt n = yl;
		ErtsDigit* yt = y;
		ErtsDigit d;
		ErtsDigit p;

		d = *x;
		x++;
		rt = r;

		switch(d) {
		case 0:
			rt = rt + n;
			break;
		case 1:
			while(n--) {
				DSUMc(*yt, *rt, c, p);
				*rt++ = p;
				yt++;
			}
			break;
		case 2:
			while(n--) {
				p = *yt;
				DSUMc(p, p, cp, p);
				DSUMc(p, *rt, c, p);
				*rt++ = p;
				yt++;
			}
			break;
		default:
			while(n--) {
				DMULc(d,*yt, cp, p);
				DSUMc(p,*rt, c, p);
				*rt++ = p;
				yt++;
			}
			break;
		}
		*rt = c + cp;
		r++;
	}
	if (*rt == 0)
		return (rt - r0);
	else
		return (rt - r0) + 1;
}


/*
** Square digits in x store in r (x & r may point into a common area)
** Assumption: x is destroyed if common area and digits in r are zero
**             to the size of xl+1
*/

static UInt I_sqr(ErtsDigit* x, UInt xl, ErtsDigit* r)
{
	ErtsDigit d_next = *x;
	ErtsDigit d;
	ErtsDigit* r0 = r;
	ErtsDigit* s = r;

	if ((r + xl) == x)	/* "Inline" operation */
		*x = 0;
	x++;

	while(xl--) {
		ErtsDigit* y = x;
		ErtsDigit y_0 = 0, y_1 = 0, y_2 = 0, y_3 = 0;
		ErtsDigit b0, b1;
		ErtsDigit z0, z1, z2;
		ErtsDigit t;
		UInt y_l = xl;

		s = r;
		d = d_next;
		d_next = *x;
		x++;

		DMUL(d, d, b1, b0);
		DSUMc(*s, b0, y_3, t);
		*s++ = t;
		z1 = b1;
		while(y_l--) {
			DMUL(d, *y, b1, b0);
			y++;
			DSUMc(b0, b0, y_0, z0);
			DSUMc(z0, z1, y_2, z2);
			DSUMc(*s, z2, y_3, t);
			*s++ = t;
			DSUMc(b1, b1, y_1, z1);
		}
		z0 = y_0;
		DSUMc(z0, z1, y_2, z2);
		DSUMc(*s, z2, y_3, t);
		*s = t;
		if (xl != 0) {
			s++;
			t = (y_1+y_2+y_3);
			*s = t;
			r += 2;
		}
		else {
			//ASSERT((y_1+y_2+y_3) == 0);
		}
	}
	if (*s == 0)
		return (s - r0);
	else
		return (s - r0) + 1;
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

Eterm big_minus(Eterm x, Eterm y, Eterm* r) {
	Eterm *xp = big_val(x);
	Eterm *yp = big_val(y);

    return big_plus_minus(BIG_V(xp),BIG_SIZE(xp),(short) BIG_SIGN(xp),
            BIG_V(yp),BIG_SIZE(yp),(short) !BIG_SIGN(yp), r);
}


/*
 ** Multiply bignums
 */

Eterm big_times(Eterm x, Eterm y, Eterm *r)
{
	Eterm* xp = big_val(x);
	Eterm* yp = big_val(y);

	short sign = BIG_SIGN(xp) != BIG_SIGN(yp);
	UInt xsz = BIG_SIZE(xp);
	UInt ysz = BIG_SIZE(yp);
	UInt rsz;

	if (ysz == 1)
		rsz = D_mul(BIG_V(xp), xsz, BIG_DIGIT(yp, 0), BIG_V(r));
	else if (xsz == 1)
		rsz = D_mul(BIG_V(yp), ysz, BIG_DIGIT(xp, 0), BIG_V(r));
	else if (xp == yp) {
		ZERO_DIGITS(BIG_V(r), xsz+1);
		rsz = I_sqr(BIG_V(xp), xsz, BIG_V(r));
	}
	else if (xsz >= ysz) {
		ZERO_DIGITS(BIG_V(r), xsz);
		rsz = I_mul(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(r));
	}
	else {
		ZERO_DIGITS(BIG_V(r), ysz);
		rsz = I_mul(BIG_V(yp), ysz, BIG_V(xp), xsz, BIG_V(r));
	}
	return big_norm(r, rsz, sign);
}
