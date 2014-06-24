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

typedef Eterm (*BifFunction)(ErlProcess*, Eterm*, UInt);
typedef struct {
	Eterm module;
	Eterm function;
	uint8_t arity;
	BifFunction f;
} BifEntry;


void erts_init_bif(void);

Eterm splus_2(ErlProcess*, Eterm*, UInt);
Eterm sminus_2(ErlProcess*, Eterm*, UInt);
Eterm stimes_2(ErlProcess*, Eterm*, UInt);

static BifEntry bif_table[] = {
		{atom_erlang, atom_splus, 2, splus_2},
		{atom_erlang, atom_sminus, 2, sminus_2},
		{atom_erlang, atom_stimes, 2, stimes_2}
};

#endif /* ERL_BIF_H_ */
