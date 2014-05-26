/*
 * erl_gc.h
 *
 *  Created on: May 25, 2014
 *      Author: Studnicki
 */


#ifndef ERL_GC_H_
#define ERL_GC_H_

#include "global.h"

void erts_init_gc(void);
unsigned int erts_next_heap_size(unsigned int size);

#endif /* ERL_GC_H_ */
