/*
 * erl_gc.h
 *
 *  Created on: May 25, 2014
 *      Author: Studnicki
 */


#ifndef ERL_GC_H_
#define ERL_GC_H_

#include "global.h"
#include "erl_process.h"
#include "io.h"

void erts_init_gc(void);

int erts_garbage_collect(ErlProcess*, int, Eterm*, int);
unsigned int erts_next_heap_size(unsigned int size);
unsigned int erts_next_heap_size_bytes(unsigned int size);

static int major_collection(ErlProcess* p, int need, Eterm* objv, int objc, UInt *recl);
static void resize_new_heap(ErlProcess* p, int new_sz, Eterm* objv, int objc);
static void offset_heap_ptr(Eterm* hp, UInt sz, SInt offset, char* area, UInt area_size);
static void offset_rootset(ErlProcess *p, SInt offset, char* area, UInt area_size, Eterm* objv, int nobj);
static Eterm* sweep_one_area(Eterm* n_hp, Eterm* n_htop, char* src, UInt src_size);
static inline UInt combined_message_size(ErlProcess* p);
static inline void move_message_to_heap(Eterm* start, Eterm* end, Eterm **hpp, ErlMessage* msg);

typedef struct {
	Eterm *v;
	UInt sz;
} Roots;

typedef struct {
	Roots *roots;
	Roots def[32];
	UInt size;
	UInt num_roots;
} Rootset;

static UInt setup_rootset(ErlProcess* p, Eterm* objv, int nobj, Rootset *rootset);
static void cleanup_rootset(Rootset *rootset);


#define IS_MOVED_BOXED(x)   (!is_header((x)))
#define IS_MOVED_CONS(x)    (is_non_value((x)))

#define in_area(ptr, start, nbytes) ((UInt)((char*)(ptr)-(char*)(start)) < (nbytes))

#define MOVE_CONS(PTR,CAR,HTOP,ORIG)                    		\
	do {                                  						\
		Eterm gval;                             				\
		HTOP[0] = CAR;      /* copy car */              		\
		HTOP[1] = PTR[1];       /* copy cdr */              	\
		gval = make_list(HTOP); /* new location */          	\
		*ORIG = gval;       /* redirect original reference */   \
		PTR[0] = THE_NON_VALUE; /* store forwarding indicator */\
		PTR[1] = gval;      /* store forwarding address */      \
		HTOP += 2;          /* update tospace htop */       	\
		} while(0)

#define MOVE_BOXED(PTR,HDR,HTOP,ORIG)               \
		do {                                    	\
			Eterm gval;                             \
			SInt nelts;                            	\
			gval = make_boxed(HTOP);                \
			*ORIG = gval;                           \
			*HTOP++ = HDR;                          \
			*PTR++ = gval;                          \
			nelts = header_arity(HDR);             	\
			while (nelts--)   {                     \
				*HTOP++ = *PTR++;                   \
			}\
		} while(0)

#endif /* ERL_GC_H_ */
