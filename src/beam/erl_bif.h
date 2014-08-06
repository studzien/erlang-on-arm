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

Eterm erts_send_message(ErlProcess*, Eterm, Eterm, int);
static void queue_message(ErlProcess*, ErlProcess*, ErlHeapFragment*, Eterm, int);


Eterm splus_2(ErlProcess*, Eterm*, UInt);
Eterm sminus_2(ErlProcess*, Eterm*, UInt);
Eterm stimes_2(ErlProcess*, Eterm*, UInt);
Eterm spawn_3(ErlProcess*, Eterm*, UInt);
Eterm spawn_link_3(ErlProcess*, Eterm*, UInt);
Eterm setelement_3(ErlProcess*, Eterm*, UInt);
Eterm now_0(ErlProcess*, Eterm*, UInt);
Eterm length_1(ErlProcess*, Eterm*, UInt);
Eterm plusplus_2(ErlProcess*, Eterm*, UInt);
Eterm div_2(ErlProcess*, Eterm*, UInt);
Eterm rem_2(ErlProcess*, Eterm*, UInt);
Eterm element_2(ErlProcess*, Eterm*, UInt);
Eterm exit_1(ErlProcess*, Eterm*, UInt);
Eterm process_flag_2(ErlProcess*, Eterm*, UInt);
Eterm self_0(ErlProcess*, Eterm*, UInt);
Eterm send_after_3(ErlProcess*, Eterm*, UInt);

typedef struct {
	ErlTimer timer;
	Eterm message;
	ErlHeapFragment* bp;
	ErlProcess* sender;
	Eterm receiver;
} ErlBifTimer;

static const BifEntry bif_table[] = {
		{atom_erlang, atom_splus, 2, splus_2},
		{atom_erlang, atom_sminus, 2, sminus_2},
		{atom_erlang, atom_stimes, 2, stimes_2},
		{atom_erlang, atom_spawn, 3, spawn_3},
		{atom_erlang, atom_spawn_link, 3, spawn_link_3},
		{atom_erlang, atom_setelement, 3, setelement_3},
		{atom_erlang, atom_now, 0, now_0},
		{atom_erlang, atom_length, 1, length_1},
		{atom_erlang, atom_plusplus, 2, plusplus_2},
		{atom_erlang, atom_div, 2, div_2},
		{atom_erlang, atom_rem, 2, rem_2},
		{atom_erlang, atom_element, 2, element_2},
		{atom_erlang, atom_exit, 1, exit_1},
		{atom_erlang, atom_process_flag, 2, process_flag_2},
		{atom_erlang, atom_self, 0, self_0},
		{atom_erlang, atom_send_after, 3, send_after_3},
		{atom_lists, atom_reverse, 1, reverse_1},
		{atom_lpc_gpio, atom_output, 2, output_2},
		{atom_lpc_gpio, atom_high,   2, high_2},
		{atom_lpc_gpio, atom_low,    2, low_2},
		{atom_lpc_debug, atom_print_term, 1, print_term1},
		{atom_lpc_debug, atom_print_info, 0, print_info0},
		{atom_lpc_debug, atom_print_heap_size, 0, print_heap_size0},
		{atom_lpc_debug, atom_dump_stack, 0, dump_stack0},
		{atom_lpc_debug, atom_dump_regs, 0, dump_regs0},
		{atom_lpc_debug, atom_dump_heap, 0, dump_heap0}
};

#endif /* ERL_BIF_H_ */
