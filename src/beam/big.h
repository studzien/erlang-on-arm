/*
 * big.h
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#ifndef BIG_H_
#define BIG_H_

#include "erl_term.h"

typedef UInt ErtsDigit;
typedef uint64_t ErtsDoubleDigit;


Eterm small_to_big(SInt, Eterm*);
Eterm small_times(SInt, SInt, Eterm*);
Eterm big_plus(Eterm, Eterm, Eterm*);
Eterm big_minus(Eterm, Eterm, Eterm*);
Eterm big_times(Eterm, Eterm, Eterm*);

#define D_EXP (sizeof(Eterm)*8)

/* macros for bignum objects */
#define big_v(x)       BIG_V(big_val(x))
#define big_sign(x)    BIG_SIGN(big_val(x))
#define big_arity(x)   BIG_ARITY(big_val(x))
#define big_digit(x,i) BIG_DIGIT(big_val(x),i)
#define big_size(x)    BIG_SIZE(big_val(x))

/* macros for thing pointers */
#define BIG_V(xp)        ((ErtsDigit*)((xp)+1))
#define BIG_SIGN(xp)     (!!bignum_header_is_neg(*xp))
#define BIG_ARITY(xp)    ((UInt)bignum_header_arity(*(xp)))
#define BIG_DIGIT(xp,i)  *(BIG_V(xp)+(i))
#define BIG_SIZE(xp)  	 BIG_ARITY(xp)

/* Check for small */
#define IS_USMALL(sgn,x)  ((sgn) ? ((x) <= MAX_SMALL+1) : ((x) <= MAX_SMALL))
#define IS_SSMALL(x)      (((x) >= MIN_SMALL) && ((x) <= MAX_SMALL))

/* The heap size needed for a bignum */
#define BIG_NEED_SIZE(x)  ((x) + 1)


/* ErtsDoubleDigit => ErtsDigit */
#define DLOW(x)        ((ErtsDigit)(x))
#define DHIGH(x)       ((ErtsDigit)(((ErtsDoubleDigit)(x)) >> D_EXP))

#define ZERO_DIGITS(v, sz) do {         \
		UInt _t_sz = sz;         \
		ErtsDigit* _t_v  = v;           \
		while(_t_sz--) *_t_v++ = 0;     \
} while(0)

#define MOVE_DIGITS(dst, src, sz) do {              \
		UInt _t_sz = sz;                 \
		ErtsDigit* _t_dst;                  \
		ErtsDigit* _t_src;                  \
		if (dst < src) {                    \
			_t_dst = dst;                   \
			_t_src = src;                   \
			while(_t_sz--) *_t_dst++ = *_t_src++;       \
		}                           \
		else if (dst > src) {                   \
			_t_dst = (dst)+((sz)-1);                \
			_t_src = (src)+((sz)-1);                \
			while(_t_sz--) *_t_dst-- = *_t_src--;       \
		}                           \
} while(0)

/* add a and b with carry in + out */
#define DSUMc(a,b,c,s) do {                     \
		ErtsDigit ___cr = (c);                      \
		ErtsDigit ___xr = (a)+(___cr);                  \
		ErtsDigit ___yr = (b);                      \
		___cr = (___xr < ___cr);                    \
		___xr = ___yr + ___xr;                      \
		___cr += (___xr < ___yr);                   \
		s = ___xr;                          \
		c = ___cr;                          \
}  while(0)

/* add a and b with carry out */
#define DSUM(a,b,c,s) do {                  \
		ErtsDigit ___xr = (a);                  \
		ErtsDigit ___yr = (b);                  \
		___xr = ___yr + ___xr;                  \
		s = ___xr;                      \
		c = (___xr < ___yr);                    \
}  while(0)

#define DSUBb(a,b,r,d) do {                     \
		ErtsDigit ___cr = (r);                      \
		ErtsDigit ___xr = (a);                      \
		ErtsDigit ___yr = (b)+___cr;                    \
		___cr = (___yr < ___cr);                    \
		___yr = ___xr - ___yr;                      \
		___cr += (___yr > ___xr);                   \
		d = ___yr;                          \
		r = ___cr;                          \
} while(0)

#define DSUB(a,b,r,d) do {          \
		ErtsDigit ___xr = (a);          \
		ErtsDigit ___yr = (b);          \
		___yr = ___xr - ___yr;          \
		r = (___yr > ___xr);            \
		d = ___yr;              \
} while(0)

#define DMULc(a,b,c,p) do {                \
		ErtsDoubleDigit _t = ((ErtsDoubleDigit)(a))*(b) + (c);  \
		p = DLOW(_t);                       \
		c = DHIGH(_t);                      \
} while(0)

#define DMULc(a,b,c,p) do {			       \
		ErtsDoubleDigit _t = ((ErtsDoubleDigit)(a))*(b) + (c);	\
		p = DLOW(_t);						\
		c = DHIGH(_t);						\
} while(0)
#define DMUL(a,b,c1,c0) do { \
		ErtsDoubleDigit _t = ((ErtsDoubleDigit)(a))*(b);	\
		c0 = DLOW(_t);					\
		c1 = DHIGH(_t);					\
} while(0)

#define DDIV(a1,a0,b,q) do {						\
		ErtsDoubleDigit _t = DDIGIT((a1),(a0));				\
		q = _t / (b);							\
} while(0)

#define DDIV2(a1,a0,b1,b0,q) do {					\
		ErtsDoubleDigit _t = DDIGIT((a1),(a0));				\
		q = _t / DDIGIT((b1),(b0));					\
} while(0)

#define DREM(a1,a0,b,r) do { \
		ErtsDoubleDigit _t = DDIGIT((a1),(a0));		\
		r = _t % (b);					\
} while(0)

#endif /* BIG_H_ */
