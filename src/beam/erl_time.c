/*
 * erl_time.c
 *
 *  Created on: Jul 15, 2014
 *      Author: Studnicki
 */

#include "erl_time.h"
#include "FreeRTOS.h"
#include "LPC17xx.h"
#include "erl_process.h"
#include "io.h"
#include "semphr.h"

static ErlTimer* tiw[TIW_SIZE]; // timer wheel
volatile UInt tiw_pos;   // current position in wheel
volatile SInt tiw_nto;   // number of timeouts in the wheel
extern ErlProcess* proc_tab;

void dump_timer_slots(ErlTimer* timer) {
	int i;
	char buf[35];
	sprintf(buf, "timer %d slots:\n", timer);
	debug(buf);


	for(i=0; i<TIW_SIZE; i++) {
		ErlTimer* t = tiw[i];
		while(t!=NULL) {
			if(t == timer) {
				sprintf(buf, "%d\n", i);
				debug(buf);
			}
			t = t->next;
		}
	}
}

void dump_timers(void) {
	char buf[50];
	sprintf(buf, "current position in the wheel is %d out of %d\n", tiw_pos, TIW_SIZE);
	debug(buf);

	int i;
	for(i=0; i<TIW_SIZE; i++) {
		sprintf(buf, "slot %d\n", i);
		debug(buf);
		ErlTimer* t = tiw[i];
		while(t != NULL) {
			sprintf(buf, "%d %d %d %d\n", t, t->active, t->count, t->slot);
			debug(buf);
			t = t->next;
		}
	}
}

void erts_init_time(void) {
	ticks = 0;

	// initialize the timer wheel
	UInt i;
	for(i=0; i<TIW_SIZE; i++) {
		tiw[i] = NULL;
	}
	tiw_pos = tiw_nto = 0;


	init_us_timer();
	init_ms_timer();
}

void erts_get_now(volatile Timeval* time) {
	time->msec = now.msec;
	time->sec = now.sec;
	time->usec = LPC_TIM0->TC;
}

// high resolution (us) timer - hardware timer 0
static void init_us_timer(void) {
	const unsigned long TCR_COUNT_RESET = 2, CTCR_CTM_TIMER = 0x00, TCR_COUNT_ENABLE = 0x01;

	now.msec = 0;
	now.sec = 0;
	now.usec = 0;

	/* Power up and feed the timer. */
	LPC_SC->PCONP |= 0x02UL;
	LPC_SC->PCLKSEL0 = (LPC_SC->PCLKSEL0 & (~(0x3<<2))) | (0x01 << 2);

	/* Reset Timer 0 */
	LPC_TIM0->TCR = TCR_COUNT_RESET;

	/* Just count up. */
	LPC_TIM0->CTCR = CTCR_CTM_TIMER;

	/* Prescale to a frequency that is good enough to get a decent resolution,
		but not too fast so as to overflow all the time. */
	LPC_TIM0->PR =  ( configCPU_CLOCK_HZ / 1000000L ) - 1UL;

	/* Start the counter. */
	LPC_TIM0->TCR = TCR_COUNT_ENABLE;

	/* Match on one second */
	LPC_TIM0->MR0 = 1000000;

	/* Generate interrupt and reset counter when equals to MR0 */
	LPC_TIM0->MCR |= ((1 << 0) | (1 << 1));

	NVIC_EnableIRQ(TIMER0_IRQn);
	NVIC_SetPriority(TIMER0_IRQn, 5);
}

void TIMER0_IRQHandler(void) {
	/* Clear TIM0 interrupt flag */
	LPC_TIM0->IR = (1 << 0);
	now.sec++;
	if(now.sec == 1000000) {
		now.sec = 0;
		now.msec++;
	}

	if(now.sec % 60 == 1) {
		//print_stats();
	}
}

// low resolution (ms) timer - hardware timer 1
static void init_ms_timer(void) {
	const unsigned long TCR_COUNT_RESET = 2, CTCR_CTM_TIMER = 0x00, TCR_COUNT_ENABLE = 0x01;

	/* Power up and feed the timer. */
	LPC_SC->PCONP |= 0x04UL;
	LPC_SC->PCLKSEL0 = (LPC_SC->PCLKSEL0 & (~(0x3<<4))) | (0x01 << 4);

	/* Reset Timer 1 */
	LPC_TIM1->TCR = TCR_COUNT_RESET;

	/* Just count up. */
	LPC_TIM1->CTCR = CTCR_CTM_TIMER;

	/* Prescale to a frequency that is good enough to get a decent resolution,
			but not too fast so as to overflow all the time. */
	LPC_TIM1->PR =  ( configCPU_CLOCK_HZ / 1000000L ) - 1UL;

	/* Start the counter. */
	LPC_TIM1->TCR = TCR_COUNT_ENABLE;

	/* Match on one milisecond */
	LPC_TIM1->MR0 = 1000;

	/* Generate interrupt and reset counter when equals to MR0 */
	LPC_TIM1->MCR |= ((1 << 0) | (1 << 1));

	erts_get_now(&then);

	NVIC_EnableIRQ(TIMER1_IRQn);
	NVIC_SetPriority(TIMER1_IRQn, 6);
}

void TIMER1_IRQHandler(void) {
	erts_get_now(&time);
	LPC_TIM1->IR = (1 << 0);

	SInt dt = ((SInt)(time.usec-then.usec) / 1000) + ((SInt)(time.sec-then.sec) * 1000);
	if(dt > 0) {
		portENTER_CRITICAL();
		erts_bump_timer(dt);
		portEXIT_CRITICAL();
	}

	then.msec = time.msec;
	then.sec = time.sec;
	then.usec = time.usec;

	/* Clear TIM1 interrupt flag */
	NVIC_ClearPendingIRQ(TIMER1_IRQn);
}

void erts_set_timer(ErlTimer* timer, ErlTimeoutProc timeout, ErlCancelProc cancel, void* arg, UInt t) {
	portENTER_CRITICAL();
	if(!timer->active) {
		timer->timeout = timeout;
		timer->cancel = cancel;
		timer->arg = arg;
		insert_timer(timer, t);
		timer->active = 1;
	}
	portEXIT_CRITICAL();
}

void erts_cancel_timer(ErlTimer* timer) {
	portENTER_CRITICAL();
	if(timer->active) {
		remove_timer(timer);
		timer->slot = timer->count = 0;

		if(timer->cancel != NULL) {
			(*timer->cancel)(timer->arg);
		}
	}
	portEXIT_CRITICAL();
}

static void insert_timer(ErlTimer* timer, UInt timeout) {
	UInt ticks = timeout; // timeout is in 1 ms resolution, so is the timer wheel

	/* Calculate slot */
	UInt tm = (ticks+tiw_pos) % TIW_SIZE;
	timer->slot = tm;
	timer->count = (UInt)(ticks / TIW_SIZE);

	/* Insert at the head of the slot list */
	timer->next = tiw[tm];
	timer->prev = NULL;
	if(timer->next != NULL) {
		timer->next->prev = timer;
	}
	tiw[tm] = timer;

	tiw_nto++;
}

static void remove_timer(ErlTimer *p) {
	/* first */
	if (!p->prev) {
		tiw[p->slot] = p->next;
		if(p->next) {
			p->next->prev = NULL;
		}
	}
	else {
		p->prev->next = p->next;
	}

	/* last */
	if (!p->next) {
		if (p->prev) {
			p->prev->next = NULL;
		}
	}
	else {
		p->next->prev = p->prev;
	}

	p->next = NULL;
	p->prev = NULL;
	/* Make sure cancel callback isn't called */
	p->active = 0;
	tiw_nto--;

}

void erts_bump_timer(UInt dt) {
	UInt keep_pos, count;
	ErlTimer *p, **prev, *timeout_head, **timeout_tail;

	if(tiw_nto == 0) {
		return;
	}

	count = (UInt)(dt / TIW_SIZE) + 1;
	keep_pos = (tiw_pos + dt) % TIW_SIZE;
	if(dt > TIW_SIZE) {
		dt = TIW_SIZE;
	}

	timeout_head = NULL;
	timeout_tail = &timeout_head;

	while(dt > 0) {
		if(tiw_pos == keep_pos) {
			count--;
		}

		prev = &tiw[tiw_pos];
		while((p = *prev) != NULL) {
			// timeout
			if(p->count < count) {
				remove_timer(p);
				*timeout_tail = p;
				timeout_tail = &p->next;
			}
			// no timeout
			else {
				p->count -= count;
				prev = &p->next;
			}
		}
		tiw_pos = (tiw_pos + 1) % TIW_SIZE;
		dt--;
	}
	tiw_pos = keep_pos;

	while(timeout_head) {
		p = timeout_head;
		timeout_head = p->next;

		p->next = NULL;
		p->prev = NULL;
		p->slot = 0;
		(*p->timeout)(p->arg);
	}
}

