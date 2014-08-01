/*
 * erl_vm.h
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#ifndef ERL_VM_H_
#define ERL_VM_H_

#define HAllocX(p, sz, xtra,Live) (((HEAP_LIMIT(p) - HEAP_TOP(p)) < (sz))) \
	? erts_heap_alloc((p),(sz),(xtra),(Live)) \
	: (HEAP_TOP(p) = HEAP_TOP(p)+(sz), HEAP_TOP(p) - (sz))
#define HAlloc(P, SZ, Live) HAllocX(P,SZ,0,Live)

#endif /* ERL_VM_H_ */
