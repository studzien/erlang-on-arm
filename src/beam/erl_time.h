/*
 * erl_time.h
 *
 *  Created on: Jul 15, 2014
 *      Author: Studnicki
 */

#ifndef ERL_TIME_H_
#define ERL_TIME_H_

#include "erl_term.h"

#define TIW_ITIME 1   // one timer-wheel slot is 1 ms
#define TIW_SIZE  128 // should be a power of 2

typedef void (*ErlTimeoutProc)(void*);
typedef void (*ErlCancelProc)(void*);

typedef struct erl_timer {
	struct erl_timer* next;
	struct erl_timer* prev;
	UInt slot;
	UInt count;
	UInt active;

	ErlTimeoutProc timeout;
	ErlCancelProc cancel;
	void* arg;
} ErlTimer;


typedef struct {
	UInt msec;
	UInt sec;
	UInt usec;
} Timeval;

volatile Timeval now;
volatile Timeval then;
volatile Timeval time;
volatile UInt ticks;

void erts_init_time(void);
void erts_get_now(volatile Timeval* time);
void erts_set_timer(ErlTimer*, ErlTimeoutProc, ErlCancelProc, void*, UInt);
void erts_cancel_timer(ErlTimer*);
void erts_bump_timer(UInt dt);

static void init_us_timer(void);
static void init_ms_timer(void);
static void insert_timer(ErlTimer*, UInt);
static void remove_timer(ErlTimer*);

#endif /* ERL_TIME_H_ */
