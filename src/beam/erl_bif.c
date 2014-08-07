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
#include "erl_utils.h"
#include "semphr.h"

extern ErlProcess* proc_tab;

Eterm erts_send_message(ErlProcess* sender, Eterm to, Eterm msg, int from_isr) {
	UInt to_ix = pid2pix(to);
	if(to_ix >= MAX_PROCESSES) {
		return atom_ok;
	}
	ErlProcess* receiver = &proc_tab[to_ix];
	if(receiver->id != to) {
		return atom_ok;
	}

	UInt msize = size_object(msg);

	if(ERTS_HEAP_FRAG_SIZE(msize) > 1000) {
		debug("erl_bif.c:33\n");
		debug_32(ERTS_HEAP_FRAG_SIZE(msize));
	}
	ErlHeapFragment* bp = (ErlHeapFragment*)pvPortMalloc(ERTS_HEAP_FRAG_SIZE(msize));
	Eterm* hp = bp->mem;
	bp->alloc_size = msize;
	bp->used_size = msize;
	msg = copy_struct(msg, msize, &hp);

	queue_message(sender, receiver, bp, msg, from_isr);

	return atom_ok;
}

static void queue_message(ErlProcess* from, ErlProcess* to, ErlHeapFragment* bp, Eterm message, int from_isr) {
	if(sizeof(ErlMessage) > 1000) {
		debug("erl_bif.c:50\n");
		debug_32(sizeof(ErlMessage));
	}
	ErlMessage* mp = (ErlMessage*)pvPortMalloc(sizeof(ErlMessage));
	mp->next = NULL;
	mp->data = bp;
	mp->m = message;
	LINK_MESSAGE(to, mp);
	xTaskHandle handle = *(to->handle);

	if(from_isr) {
		xTaskResumeFromISR(handle);
	}
	else {
		vTaskResume(handle);
	}
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

Eterm spawn_link_3(ErlProcess* p, Eterm* reg, UInt live) {
	ErlSpawnOpts opts;
	opts.flags |= SPO_LINK;
	return erl_create_process(p, reg[0], reg[1], reg[2], &opts);
}

Eterm setelement_3(ErlProcess* p, Eterm* reg, UInt live) {
	Eterm old = reg[1];
	Eterm* ptr = tuple_val(reg[1]);
	UInt ix = unsigned_val(reg[0]);
	UInt size = arityval(*ptr)+1;

	Eterm* hp = HAlloc(p, size, 3);

	ptr = tuple_val(reg[1]);
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
	Eterm list = reg[live];
	while(is_list(list)) {
		length++;
		list = CDR(list_val(list));
	}

	return make_small(length);
}

Eterm plusplus_2(ErlProcess* p, Eterm* reg, UInt live) {
	int i = list_length(reg[0]);
	if(i == 0) {
		return reg[1];
	}
	else if(is_nil(reg[1])) {
		return reg[0];
	}

	int need = i*2;
	Eterm* hp = HAlloc(p, need, live);
	Eterm list = reg[0];
	Eterm copy, last;
	copy = last = CONS(hp, CAR(list_val(list)), make_list(hp+2));
	list = CDR(list_val(list));
	hp += 2;
	i--;
	while(i--) {
		Eterm* listp = list_val(list);
		last = CONS(hp, CAR(listp), make_list(hp+2));
		list = CDR(listp);
		hp += 2;
	}
	CDR(list_val(last)) = reg[1];

	return copy;
}

Eterm div_2(ErlProcess* p, Eterm* reg, UInt live) {
	SInt a = signed_val(reg[live]);
	SInt b = signed_val(reg[live+1]);
	return make_small(a / b);
}

Eterm rem_2(ErlProcess* p, Eterm* reg, UInt live) {
	//debug("rem_2 inside\n");
	SInt a = signed_val(reg[live]);
	SInt b = signed_val(reg[live+1]);
	return make_small(a % b);
}

Eterm uniform_1(ErlProcess* p, Eterm* reg, UInt live) {
	Timeval seed;
	UInt n = unsigned_val(reg[0]);
	erts_get_now(&seed);
	return make_small((seed.usec % n)+1);
}

Eterm element_2(ErlProcess* p, Eterm* reg, UInt live) {
	int n = signed_val(reg[live]);
	Eterm tuple = reg[live+1];

	Eterm element = *(tuple_val(tuple)+n);

	return element;
}

Eterm exit_1(ErlProcess* p, Eterm* reg, UInt live) {
	Eterm reason = reg[0];
	erts_do_exit_process(p, reason);
	return atom_ok;
}

Eterm process_flag_2(ErlProcess* p, Eterm* reg, UInt live) {
	Eterm flag = reg[0];
	Eterm value = reg[1];
	if(flag == atom_trap_exit) {
		if(value == atom_true) {
			p->flags |= F_TRAP_EXIT;
		}
	}
	return atom_ok;
}

Eterm self_0(ErlProcess* p, Eterm* reg, UInt live) {
	return p->id;
}


static void bif_timer_timeout(void* arg) {
	ErlBifTimer *bt = (ErlBifTimer*)arg;
	erts_send_message(bt->sender, bt->receiver, bt->message, 1);
	vPortFree(bt->bp);
	vPortFree(bt);
}

Eterm send_after_3(ErlProcess* p, Eterm* reg, UInt live) {
	SInt time = signed_val(reg[0]);
	Eterm pid = reg[1];
	Eterm msg = reg[2];

	UInt sz = size_object(msg);
	if(sizeof(ErlBifTimer) > 1000) {
		debug("erl_bif.c:238\n");
		debug_32(sizeof(ErlBifTimer));
	}
	ErlBifTimer* bt = (ErlBifTimer*)pvPortMalloc(sizeof(ErlBifTimer));
	if(ERTS_HEAP_FRAG_SIZE(sz) > 1000) {
		debug("erl_bif.c:240\n");
		debug_32(ERTS_HEAP_FRAG_SIZE(sz));
	}
	bt->bp = (ErlHeapFragment*)pvPortMalloc(ERTS_HEAP_FRAG_SIZE(sz));
	Eterm *hp = bt->bp->mem;

	bt->message = copy_struct(msg, sz, &hp);
	bt->bp->alloc_size = sz;
	bt->bp->used_size = sz;
	bt->sender = p;
	bt->receiver = pid;
	bt->timer.active = 0;
	bt->timer.slot = 0;
	bt->timer.count = 0;

	erts_set_timer(&bt->timer, (ErlTimeoutProc)bif_timer_timeout, NULL, (void*)bt, time);
	return atom_ok;
}
