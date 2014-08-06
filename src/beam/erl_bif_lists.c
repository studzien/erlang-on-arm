/*
 * erl_bif_lists.c
 *
 *  Created on: Jul 28, 2014
 *      Author: Studnicki
 */

#include "erl_bif_lists.h"
#include "erl_vm.h"
#include "io.h"

Eterm reverse_1(ErlProcess* p, Eterm* reg, UInt live) {
	UInt n = 0;
	Eterm list = reg[0];

	//debug("lists:reverse/1\n");
	//debug_term(list);
	//debug("\n");

	while(is_list(list)) {
		n++;
		list = CDR(list_val(list));
	}

	Eterm* hp = HAlloc(p, 2*n, live);
	Eterm result = NIL;

	list = reg[0];

	while(is_list(list)) {
		Eterm* pair = list_val(list);
		result = CONS(hp, CAR(pair), result);
		list = CDR(pair);
		hp += 2;
	}

	return result;
}
