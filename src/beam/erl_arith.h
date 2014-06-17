/*
 * erl_arith.h
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#ifndef ERL_ARITH_H_
#define ERL_ARITH_H_

#include "global.h"
#include "erl_process.h"
#include "erl_term.h"

Eterm erts_mixed_plus(ErlProcess* p, Eterm arg0, Eterm arg1);


#endif /* ERL_ARITH_H_ */
