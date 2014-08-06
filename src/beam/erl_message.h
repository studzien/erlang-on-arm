/*
 * erl_message.h
 *
 *  Created on: Jul 29, 2014
 *      Author: Studnicki
 */

#ifndef ERL_MESSAGE_H_
#define ERL_MESSAGE_H_

#include "erl_term.h"

#define ERTS_HEAP_FRAG_SIZE(DATA_WORDS) \
   (sizeof(ErlHeapFragment) - sizeof(Eterm) + (DATA_WORDS)*sizeof(Eterm))

typedef struct ErlHeapFragment ErlHeapFragment;

struct ErlHeapFragment {
	ErlHeapFragment* next;
	unsigned alloc_size;
	unsigned used_size;
	Eterm mem[1];
};

typedef struct erl_mesg {
	struct erl_mesg* next;
	ErlHeapFragment* data;
	Eterm m;
} ErlMessage;

typedef struct {
	ErlMessage* first;
	ErlMessage** last;
	ErlMessage** save;
	SInt len;

	ErlMessage** saved_last;
} ErlMessageQueue;

/* Unlink current message */
#define UNLINK_MESSAGE(p,msgp) do { \
	 portENTER_CRITICAL(); \
     ErlMessage* __mp = (msgp)->next; \
     *(p)->msg.save = __mp; \
     (p)->msg.len--; \
     if (__mp == NULL) \
         (p)->msg.last = (p)->msg.save; \
     portEXIT_CRITICAL(); \
} while(0)

/* Reset message save point (after receive match) */
#define JOIN_MESSAGE(p) do { \
	 taskENTER_CRITICAL(); \
     (p)->msg.save = &(p)->msg.first; \
     taskEXIT_CRITICAL(); \
} while(0)

/* Save current message */
#define SAVE_MESSAGE(p) do {\
     taskENTER_CRITICAL(); \
     (p)->msg.save = &(*(p)->msg.save)->next; \
     taskEXIT_CRITICAL(); \
} while(0)

/* Get "current" message */
#define PEEK_MESSAGE(p) (*(p)->msg.save)


/* Add message last in private message queue */
#define LINK_MESSAGE(p, mp) do { \
	taskENTER_CRITICAL(); \
    *(p)->msg.last = (mp); \
    (p)->msg.last = &(mp)->next; \
    (p)->msg.len++; \
    taskEXIT_CRITICAL(); \
} while(0)

#endif /* ERL_MESSAGE_H_ */
