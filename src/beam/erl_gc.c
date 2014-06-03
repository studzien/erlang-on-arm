/*
 * erl_gc.c
 *
 *  Created on: May 25, 2014
 *      Author: Studnicki
 */

#include "erl_gc.h"

#define MAX_HEAP_SIZES 24

static SInt heap_sizes[MAX_HEAP_SIZES];

//init global garbage collector data
void erts_init_gc(void) {
	//init allowed heap sizes
	heap_sizes[0] = 12;
	heap_sizes[1] = 38;

	int i;
	for(i=2; i<MAX_HEAP_SIZES; i++) {
		heap_sizes[i] = heap_sizes[i-1] + heap_sizes[i-2] + 1;
	}
}

// returns the next heap size to use
unsigned int erts_next_heap_size(unsigned int size) {
	char buf[256];
	sprintf(buf, "needed size: %d", size);
	debug(buf);
	int i;
	for(i=0; i<MAX_HEAP_SIZES; i++) {
		if(heap_sizes[i] > size) {
			return heap_sizes[i];
		}
	}
	return -1;
}
