/*
 * erl_term.h
 *
 *  Created on: Nov 2, 2013
 *      Author: Studnicki
 */

#ifndef ERL_TERM_H_
#define ERL_TERM_H_

// tagged Erlang term (32 bits)
typedef uint32_t Eterm;

#define _TAG_PRIMARY_SIZE   2
#define _TAG_PRIMARY_MASK   0x3
#define TAG_PRIMARY_HEADER  0x0
#define TAG_PRIMARY_LIST    0x1
#define TAG_PRIMARY_BOXED   0x2
#define TAG_PRIMARY_IMMED1  0x3

#define primary_tag(x)  ((x) & _TAG_PRIMARY_MASK)

#define _TAG_IMMED1_SIZE    4
#define _TAG_IMMED1_MASK    0xF
#define _TAG_IMMED1_PID     ((0x0 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_PORT    ((0x1 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_IMMED2  ((0x2 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_SMALL   ((0x3 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)

#define _TAG_IMMED2_SIZE    6
#define _TAG_IMMED2_MASK    0x3F
#define _TAG_IMMED2_ATOM    ((0x0 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_CATCH   ((0x1 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_NIL     ((0x3 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)

// atom access methods
#define make_atom(x) ((Eterm)(((x) << _TAG_IMMED2_SIZE) + _TAG_IMMED2_ATOM))

#endif /* ERL_TERM_H_ */