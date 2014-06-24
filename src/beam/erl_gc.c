/*
 * erl_gc.c
 *
 *  Created on: May 25, 2014
 *      Author: Studnicki
 */

#include "erl_gc.h"

#define MAX_HEAP_SIZES 24

static SInt heap_sizes[MAX_HEAP_SIZES];

UInt reclaimed = 0;
UInt garbage_cols = 0;

//init global garbage collector data
void erts_init_gc(void) {
	//init allowed heap sizes
	heap_sizes[0] = 12;
	heap_sizes[1] = 38;

	int i;
	for(i=2; i<MAX_HEAP_SIZES; i++) {
		heap_sizes[i] = 1.2 * heap_sizes[i-1];
		//heap_sizes[i] = heap_sizes[i-1] + heap_sizes[i-2] + 1;
	}
}

/*
 * Garbage collect a process.
 *
 * p: Pointer to the process structure.
 * need: Number of Eterm words needed on the heap.
 * objv: Array of terms to add to rootset; that is to preserve.
 * nobj: Number of objects in objv.
 */
int erts_garbage_collect(ErlProcess* p, int need, Eterm* objv, int objc) {
	debug("gc start\n");
	UInt ticks1, ticks2;
	ticks1 = LPC_TIM0->TC;

	major_collection(p, need, objv, objc, &reclaimed);
	garbage_cols++;

	int cost = (HEAP_TOP(p) - HEAP_START(p))/10;
	p->reductions += cost;

	ticks2 = LPC_TIM0->TC;
	p->gc_ticks += ticks2-ticks1;
	debug("gc end\n");
	return cost;
}

static int major_collection(ErlProcess* p, int need, Eterm* objv, int objc, UInt *recl) {
	Eterm *n_heap;
	Eterm *n_htop;
	char* src = (char*)HEAP_START(p);
	UInt src_size = (char*)HEAP_TOP(p) - src;

	//@todo do gc on heap fragments and old heap
	UInt new_sz = HEAP_SIZE(p);
	new_sz = erts_next_heap_size(new_sz);

	char buf[30];
	sprintf(buf, "new size is %u\n", new_sz);
	debug(buf);

	UInt size_before = (HEAP_TOP(p) - HEAP_START(p));

	debug_32(xPortGetFreeHeapSize());
	n_heap = n_htop = (Eterm*)pvPortMalloc(new_sz * sizeof(Eterm));
	debug_32(xPortGetFreeHeapSize());

	debug("setting up rootset\n");
	Rootset rootset;
	UInt n = setup_rootset(p, objv, objc, &rootset);
	sprintf(buf, "rootset size is %u\n", n);
	debug(buf);
	Roots* roots = rootset.roots;

	while(n--) {
		Eterm* g_ptr = roots->v;
		Eterm g_sz = roots->sz;
		roots++;

		while(g_sz--) {
			Eterm *ptr;
			Eterm val;
			Eterm g_val = *g_ptr;

			switch(primary_tag(g_val)) {
			case TAG_PRIMARY_BOXED:
				ptr = boxed_val(g_val);
				val = *ptr;
				if (IS_MOVED_BOXED(val)) {
					*g_ptr++ = val;
				} else if (in_area(ptr, src, src_size)) {
					MOVE_BOXED(ptr,val,n_htop,g_ptr++);
				} else {
					g_ptr++;
				}
				continue;

				//@todo add list

			default:
				g_ptr++;
				continue;
			}
		}
	}

	cleanup_rootset(&rootset);

	// Move the stack to the end of the heap
	n = HEAP_END(p) - STACK_TOP(p);
	memcpy(n_heap + new_sz - n, STACK_TOP(p), n * sizeof(Eterm));
	STACK_TOP(p) = n_heap + new_sz - n;

	// Free old heap T
	vPortFree(HEAP_START(p));

	// Rewrite new heap data
	HEAP_START(p) = n_heap;
	HEAP_TOP(p) = n_htop;
	HEAP_SIZE(p) = new_sz;
	HEAP_END(p) = n_heap + new_sz;

	*recl += size_before - (HEAP_TOP(p)-HEAP_START(p));

	UInt stack_size = HEAP_END(p) - STACK_TOP(p);
	UInt size_after = HEAP_TOP(p) - HEAP_START(p);
	UInt need_after = stack_size + size_after + need;

	if(HEAP_SIZE(p) < need_after) {
		UInt new_sz = erts_next_heap_size(need_after);
		grow_new_heap(p, new_sz, objv, objc);
	}

	return 1;
}

static void grow_new_heap(ErlProcess* p, int new_sz, Eterm* objv, int objc) {
	debug_32(xPortGetFreeHeapSize());
	Eterm* new_heap = (Eterm*)pvPortMalloc(new_sz * sizeof(Eterm));
	debug_32(xPortGetFreeHeapSize());
	UInt heap_size = HEAP_TOP(p) - HEAP_START(p);
	memcpy(new_heap, HEAP_START(p), heap_size*sizeof(Eterm));

	UInt stack_size = HEAP_END(p) - STACK_TOP(p);
	SInt offset = new_heap - HEAP_START(p);

	char* area = (char*)HEAP_START(p);
	UInt area_size = (char*)HEAP_TOP(p)-area;
	Eterm *prev_stop = STACK_TOP(p);

	offset_heap_ptr(new_heap, heap_size, offset, area, area_size);

	HEAP_END(p) = new_heap + new_sz;
	prev_stop = STACK_TOP(p);
	STACK_TOP(p) = STACK_START(p) - stack_size;
	memcpy(STACK_TOP(p), prev_stop, stack_size * sizeof(Eterm));

    offset_rootset(p, offset, area, area_size, objv, objc);
    Eterm *old_heap = HEAP_START(p);
    HEAP_TOP(p) = new_heap + heap_size;
    HEAP_START(p) = new_heap;
    HEAP_SIZE(p) = new_sz;

    vPortFree(old_heap);
}

static void offset_heap_ptr(Eterm* hp, UInt sz, SInt offset, char* area, UInt area_size) {
	while(sz--) {
		Eterm val = *hp;
		switch(primary_tag(val)) {
		case TAG_PRIMARY_LIST:
		case TAG_PRIMARY_BOXED:
			if(in_area(ptr_val(val), area, area_size)) {
				*hp = offset_ptr(val, offset);
			}
			hp++;
			break;
		default:
			hp++;
		}
	}
}

static void offset_rootset(ErlProcess *p, SInt offset, char* area, UInt area_size, Eterm* objv, int nobj) {
	offset_heap_ptr(p->stop, (STACK_START(p) - p->stop), offset, area, area_size);
	if (nobj > 0) {
		offset_heap_ptr(objv, nobj, offset, area, area_size);
	}
}

static UInt setup_rootset(ErlProcess* p, Eterm* objv, int nobj, Rootset *rootset) {
	//@todo initialize other structs (like message buffers etc.) to the rootset
	Roots* roots = rootset->def;
	UInt n = 0;

	rootset->size = sizeof(rootset->def)/sizeof(rootset->def[0]);

	// add stack to the rootset
	roots[n].v = STACK_TOP(p);
	roots[n].sz = STACK_START(p) - STACK_TOP(p);
	n++;

	// add live registers to the rootset
	if(nobj > 0) {
		roots[n].v = objv;
		roots[n].sz = nobj;
		n++;
	}

	rootset->roots = roots;
	rootset->num_roots = n;

	return n;
}

static void cleanup_rootset(Rootset* rootset) {
	if(rootset->roots != rootset->def) {
		vPortFree(rootset->roots);
	}
}

// returns the next heap size to use
unsigned int erts_next_heap_size(unsigned int size) {
	int i;
	for(i=0; i<MAX_HEAP_SIZES; i++) {
		if(heap_sizes[i] >= size) {
			return heap_sizes[i];
		}
	}
	return -1;
}
