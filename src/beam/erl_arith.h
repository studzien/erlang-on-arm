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

Eterm erts_gc_mixed_plus(ErlProcess* p, Eterm* reg, UInt live);
static inline Eterm erts_gc_big_plus(ErlProcess* p, Eterm arg0, Eterm arg1,
									 Eterm* tmp_big0, Eterm* tmp_big1, Eterm* reg, UInt live);

Eterm erts_gc_mixed_minus(ErlProcess* p, Eterm* reg, UInt live);
static inline Eterm erts_gc_big_minus(ErlProcess* p, Eterm arg0, Eterm arg1,
									  Eterm* tmp_big0, Eterm* tmp_big1, Eterm* reg, UInt live);

Eterm erts_gc_mixed_times(ErlProcess* p, Eterm* reg, UInt live);
static inline Eterm erts_gc_big_times(ErlProcess* p, Eterm arg0, Eterm arg1,
									  Eterm* tmp_big0, Eterm* tmp_big1, Eterm* reg, UInt live);

#define ERTS_NEED_GC(p, need) ((HEAP_LIMIT((p)) - HEAP_TOP((p))) <= (need))

#endif /* ERL_ARITH_H_ */
