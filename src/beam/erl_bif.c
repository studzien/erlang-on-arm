/*
 * erl_bif.c
 *
 *  Created on: Apr 4, 2014
 *      Author: rafalstudnicki
 */

#include "erl_bif.h"
#include "erl_arith.h"
#include "export.h"

void erts_init_bif(void) {
	int i;
	for(i=0; i<(sizeof(bif_table)/sizeof(bif_table[0])); i++) {
		Export* e;
		Eterm module, function;
		uint8_t arity;
		module = bif_table[i].module;
		function = bif_table[i].function;
		arity = bif_table[i].arity;
		e = erts_export_put(module, function, arity);
		e->bif = bif_table[i].f;
	}
}

Eterm splus_2(ErlProcess* p, Eterm* reg, UInt live) {
	return erts_gc_mixed_plus(p, reg, live);
}

Eterm sminus_2(ErlProcess* p, Eterm* reg, UInt live) {
	return erts_gc_mixed_minus(p, reg, live);
}

Eterm stimes_2(ErlProcess* p, Eterm* reg, UInt live) {
	return erts_gc_mixed_times(p, reg, live);
}
