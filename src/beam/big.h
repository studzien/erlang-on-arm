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

#define BIG_V(xp)        ((ErtsDigit*)((xp)+1))
#define BIG_SIGN(xp)     (!!bignum_header_is_neg(*xp))
#define BIG_ARITY(xp)    ((Uint)bignum_header_arity(*(xp)))
#define BIG_DIGIT(xp,i)  *(BIG_V(xp)+(i))

Eterm small_to_big(SInt, Eterm*);

#endif /* BIG_H_ */
