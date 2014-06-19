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

Eterm plus_2(ErlProcess* p, Eterm* args) {
	return erts_mixed_plus(p, args[0], args[1]);
}

Eterm minus_2(ErlProcess* p, Eterm* args) {
	return erts_mixed_minus(p, args[0], args[1]);
}

Eterm times_2(ErlProcess* p, Eterm* args) {
	Eterm arg0 = unsigned_val(args[0]);
	Eterm arg1 = unsigned_val(args[1]);
	return make_small(arg0*arg1);
}
