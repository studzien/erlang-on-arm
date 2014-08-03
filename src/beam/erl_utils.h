/*
 * erl_utils.h
 *
 *  Created on: Aug 2, 2014
 *      Author: Studnicki
 */

#ifndef ERL_UTILS_H_
#define ERL_UTILS_H_

#include "erl_term.h"

inline int eq(Eterm a, Eterm b);

#define EQ(x,y) (((x) == (y)) || (is_not_both_immed((x),(y)) && eq((x),(y))))

#endif /* ERL_UTILS_H_ */
