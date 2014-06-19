/*
 * erl_bif.h
 *
 *  Created on: Apr 4, 2014
 *      Author: rafalstudnicki
 */

#ifndef ERL_BIF_H_
#define ERL_BIF_H_

#include "erl_process.h"
#include "atom.h"

typedef Eterm (*BifFunction)(ErlProcess*, Eterm*);
typedef struct {
	Eterm module;
	Eterm function;
	uint8_t arity;
	BifFunction f;
} BifEntry;


void erts_init_bif(void);

Eterm plus_2(ErlProcess*, Eterm*);
Eterm minus_2(ErlProcess*, Eterm*);
Eterm times_2(ErlProcess*, Eterm*);

static BifEntry bif_table[] = {
		{atom_erlang, atom_plus, 2, plus_2},
		{atom_erlang, atom_minus, 2, minus_2},
		{atom_erlang, atom_times, 2, times_2}
};

#endif /* ERL_BIF_H_ */
