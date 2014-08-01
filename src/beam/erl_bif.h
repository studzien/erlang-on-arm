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

#include "erl_bif_lpc.h"
#include "erl_bif_lists.h"

typedef Eterm (*BifFunction)(ErlProcess*, Eterm*, UInt);
typedef struct {
	Eterm module;
	Eterm function;
	uint8_t arity;
	BifFunction f;
} BifEntry;


void erts_init_bif(void);

Eterm erts_send_message(ErlProcess*, Eterm, Eterm);
static void queue_message(ErlProcess*, ErlProcess*, ErlHeapFragment*, Eterm);

Eterm splus_2(ErlProcess*, Eterm*, UInt);
Eterm sminus_2(ErlProcess*, Eterm*, UInt);
Eterm stimes_2(ErlProcess*, Eterm*, UInt);
Eterm spawn_3(ErlProcess*, Eterm*, UInt);
Eterm setelement_3(ErlProcess*, Eterm*, UInt);
Eterm now_0(ErlProcess*, Eterm*, UInt);
Eterm length_1(ErlProcess*, Eterm*, UInt);

static const BifEntry bif_table[] = {
		{atom_erlang, atom_splus, 2, splus_2},
		{atom_erlang, atom_sminus, 2, sminus_2},
		{atom_erlang, atom_stimes, 2, stimes_2},
		{atom_erlang, atom_spawn, 3, spawn_3},
		{atom_erlang, atom_setelement, 3, setelement_3},
		{atom_erlang, atom_now, 0, now_0},
		{atom_erlang, atom_length, 1, length_1},
		{atom_lists, atom_reverse, 1, reverse_1},
		{atom_lpc_gpio, atom_output, 2, output_2},
		{atom_lpc_gpio, atom_high,   2, high_2},
		{atom_lpc_gpio, atom_low,    2, low_2},
		{atom_lpc_debug, atom_print_term, 1, print_term1},
		{atom_lpc_debug, atom_print_info, 0, print_info0},
		{atom_lpc_debug, atom_print_heap_size, 0, print_heap_size0}
};

#endif /* ERL_BIF_H_ */
