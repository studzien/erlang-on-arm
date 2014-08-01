/*
 * erl_bif.c
 *
 *  Created on: Apr 4, 2014
 *      Author: rafalstudnicki
 */

#include "erl_bif.h"
#include "erl_arith.h"
#include "export.h"
#include "io.h"
#include "erl_vm.h"
#include "erl_gc.h"
#include "erl_time.h"
#include "erl_message.h"

extern ErlProcess* proc_tab;

Eterm erts_send_message(ErlProcess* sender, Eterm to, Eterm msg) {
	UInt to_ix = pid2pix(to);
	if(to_ix >= MAX_PROCESSES) {
		return atom_ok;
	}
	ErlProcess* receiver = &proc_tab[to_ix];
	if(receiver->id != to) {
		return atom_ok;
	}

	UInt msize = size_object(msg);
	ErlHeapFragment* bp = (ErlHeapFragment*)pvPortMalloc(ERTS_HEAP_FRAG_SIZE(msize));
	Eterm* hp = bp->mem;
	bp->alloc_size = msize;
	bp->used_size = msize;
	msg = copy_struct(msg, msize, &hp);

	queue_message(sender, receiver, bp, msg);

	return atom_ok;
}

static void queue_message(ErlProcess* from, ErlProcess* to, ErlHeapFragment* bp, Eterm message) {
	ErlMessage* mp = (ErlMessage*)pvPortMalloc(sizeof(ErlMessage));
	mp->next = NULL;
	mp->data = bp;
	mp->m = message;
	LINK_MESSAGE(to, mp);
	vTaskResume(*(to->handle));
}

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

Eterm spawn_3(ErlProcess* p, Eterm* reg, UInt live) {
	return erl_create_process(p, reg[0], reg[1], reg[2], NULL);
}

Eterm setelement_3(ErlProcess* p, Eterm* reg, UInt live) {
	Eterm* ptr = tuple_val(reg[1]);
	UInt ix = unsigned_val(reg[0]);
	UInt size = arityval(*ptr)+1;
	Eterm* hp = HAlloc(p, size, 3);

	memcpy(hp, ptr, size*sizeof(Eterm));
	hp[ix] = reg[2];

	return make_tuple(hp);
}

Eterm now_0(ErlProcess* p, Eterm* reg, UInt live) {
	Timeval t;
	erts_get_now(&t);

	Eterm *hp = HAlloc(p, 4, live);
	hp[0] = make_arityval(3);
	hp[1] = make_small(t.msec);
	hp[2] = make_small(t.sec);
	hp[3] = make_small(t.usec);
	return make_tuple(hp);
}

Eterm length_1(ErlProcess* p, Eterm* reg, UInt live) {
	UInt length = 0;
	Eterm list = reg[0];
	while(is_list(list)) {
		length++;
		list = CDR(list_val(list));
	}

	return make_small(length);
}
