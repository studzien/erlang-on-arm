/*
 * erl_vm.h
 *
 *  Created on: Jun 17, 2014
 *      Author: Studnicki
 */

#ifndef ERL_VM_H_
#define ERL_VM_H_

#define HAllocX(p, sz, xtra) (HEAP_TOP(p) = HEAP_TOP(p)+(sz), HEAP_TOP(p) - (sz))
#define HAlloc(P, SZ) HAllocX(P,SZ,0)

#endif /* ERL_VM_H_ */
