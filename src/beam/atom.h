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

#endif /* ATOM_H_ */
