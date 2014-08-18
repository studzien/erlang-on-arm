/*
 * atom.h
 *
 *  Created on: Mar 31, 2014
 *      Author: Studnicki
 */

#ifndef ATOM_H_
#define ATOM_H_

#include "global.h"
#include "index.h"
#include "hash.h"
#include "erl_term.h"

#define MAX_ATOM_CHARACTERS 255
#define MAX_ATOM_SZ_LIMIT (4*MAX_ATOM_CHARACTERS)

typedef struct {
	IndexSlot slot; // Atom is an element of IndexTable thus this must be at the top
	uint8_t len; // length of atom name
	byte* name;
} Atom;

void init_atom_table(void);
Eterm erts_atom_put(const byte* name, int len);
int erts_atom_get(const char *name, int len, Eterm* ap);
void dump_atoms();

// Hashtable callbacks
static HashValue atom_hash(Atom*);
static int atom_cmp(Atom*, Atom*);
static Atom* atom_alloc(Atom*);
static void atom_free(Atom*);

#define atom_erlang          make_atom(0)
#define atom_splus           make_atom(1)
#define atom_sminus          make_atom(2)
#define atom_stimes          make_atom(3)
#define atom_normal          make_atom(4)
#define atom_lpc_gpio        make_atom(5)
#define atom_output          make_atom(6)
#define atom_low             make_atom(7)
#define atom_high            make_atom(8)
#define atom_ok              make_atom(9)
#define atom_function_clause make_atom(10)
#define atom_spawn			 make_atom(11)
#define atom_setelement		 make_atom(12)
#define atom_lpc_debug		 make_atom(13)
#define atom_print_term		 make_atom(14)
#define atom_print_info		 make_atom(15)
#define atom_now			 make_atom(16)
#define atom_length			 make_atom(17)
#define atom_lists			 make_atom(18)
#define atom_reverse		 make_atom(19)
#define atom_print_heap_size make_atom(20)
#define atom_div			 make_atom(21)
#define atom_plusplus	     make_atom(22)
#define atom_nth			 make_atom(23)
#define atom_delete			 make_atom(24)
#define atom_rem 			 make_atom(25)
#define atom_dump_stack		 make_atom(26)
#define atom_kill			 make_atom(27)
#define atom_EXIT			 make_atom(28)
#define atom_element		 make_atom(29)
#define atom_exit			 make_atom(30)
#define atom_spawn_link		 make_atom(31)
#define atom_process_flag	 make_atom(32)
#define atom_true			 make_atom(33)
#define atom_false			 make_atom(34)
#define atom_trap_exit		 make_atom(35)
#define atom_self			 make_atom(36)
#define atom_send_after		 make_atom(37)
#define atom_dump_regs		 make_atom(38)
#define atom_dump_heap		 make_atom(39)
#define atom_badarg			 make_atom(40)
#define atom_badmatch		 make_atom(41)
#define atom_case_clause	 make_atom(42)
#define atom_if_clause	     make_atom(43)
#define atom_undef		     make_atom(44)
#define atom_interrupt	     make_atom(45)
#define atom_falling	     make_atom(46)
#define atom_input			 make_atom(47)
#define atom_bsr			 make_atom(48)
#define atom_bor			 make_atom(49)
#define atom_lpc_spi		 make_atom(50)
#define atom_init		     make_atom(51)
#define atom_rw				 make_atom(52)
#define atom_band			 make_atom(53)

#endif /* ATOM_H_ */
