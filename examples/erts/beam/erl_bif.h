/*
 * erl_bif.h
 *
 *  Created on: Apr 4, 2014
 *      Author: rafalstudnicki
 */

#ifndef ERL_BIF_H_
#define ERL_BIF_H_

#include "global.h"
#include "atom.h"

typedef Eterm (*BifFunction)(Eterm*);
typedef struct {
	Eterm module;
	Eterm function;
	uint8_t arity;
	BifFunction f;
} BifEntry;

void erts_init_bif(void);

Eterm plus_2(Eterm*);
Eterm minus_2(Eterm*);
Eterm multiply_2(Eterm*);

static BifEntry bif_table[] = {
		{atom_erlang, atom_plus, 2, plus_2},
		{atom_erlang, atom_minus, 2, minus_2},
		{atom_erlang, atom_multiply, 2, multiply_2}
};

#endif /* ERL_BIF_H_ */
