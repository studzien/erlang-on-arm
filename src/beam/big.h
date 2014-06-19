/*
 * big.h
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#ifndef BIG_H_
#define BIG_H_

#include "erl_term.h"

typedef UInt     ErtsDigit;


Eterm small_to_big(SInt, Eterm*);
Eterm big_plus(Eterm, Eterm, Eterm*);
Eterm big_minus(Eterm, Eterm, Eterm*);

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


#endif /* BIG_H_ */
