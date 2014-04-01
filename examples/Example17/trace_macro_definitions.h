/*
    FreeRTOS V6.1.1 - Copyright (C) 2011 Real Time Engineers Ltd.

    This file is part of the FreeRTOS distribution.

    FreeRTOS is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Public License (version 2) as published by the
    Free Software Foundation AND MODIFIED BY the FreeRTOS exception.
    ***NOTE*** The exception to the GPL is included to allow you to distribute
    a combined work that includes FreeRTOS without being obliged to provide the
    source code for proprietary components outside of the FreeRTOS kernel.
    FreeRTOS is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
    more details. You should have received a copy of the GNU General Public 
    License and the FreeRTOS license exception along with FreeRTOS; if not it 
    can be viewed here: http://www.freertos.org/a00114.html and also obtained 
    by writing to Richard Barry, contact details for whom are available on the
    FreeRTOS WEB site.

    1 tab == 4 spaces!

    http://www.FreeRTOS.org - Documentation, latest information, license and
    contact details.

    http://www.SafeRTOS.com - A version that is certified for use in safety
    critical systems.

    http://www.OpenRTOS.com - Commercial support, development, porting,
    licensing and training services.
*/

#ifndef TRACE_MACRO_DEFINITIONS_H
#define TRACE_MACRO_DEFINITIONS_H

/* Include files required by the trace macros. */
#include <stdio.h>
#include "FreeRTOS.h"
#include "Task.h"
#include "basic_io.h"

/* This header file is included from FreeRTOSConfig.h - which ensures these 
trace macro are defined before their default implementations that are contained
in FreeRTOS.h.

Note: This example makes direct use of consoleprint() from within
traceTASK_SWITCHED_OUT() and traceTASK_SWITCHED_IN(), even though these two
macros are called from an interrupt service routine.  This is done to make this
trivial example as easy to follow and as generic as possible.  However,
consoleprint() has the potential to take a long time to execute, might require
a large stack, and could access resources that should not be accessed
simultaneously by tasks and interrupts.  For these reasons, text or file based
output functions (especially printf()) should not be called from an interrupt
service routine in a real application. */


/* xTaskThatWasRunning is defined in main.c for convenience. */
extern xTaskHandle xTaskThatWasRunning;

/* cBuffer[] is defined in basic_io.c and is shared by all the macros that
output text.  The scheduler locking API functions are used in macros that are
called in the context of a task to ensure mutually exclusive access to
cBuffer[] is maintained at all times.  This is a safe mutual exclusion
mechanism in this case, because the macros that are called in the context of an
interrupt are only called in the tick interrupt, and will therefore not be
executed if the scheduler is locked. */
extern char cBuffer[];


/* traceTASK_SWITCHED_OUT() is always called before a reschedule, and
traceTASK_SWITCHED_IN() is always called after a reschedule.  This definition
of traceTASK_SWITCHED_OUT() just records which task was running when the macro 
was called.  The recorded value is later compared to the task in the Running 
state when the traceTASK_SWITCHED_IN() macro is called to determine if the 
Running state task was changed. */
#define traceTASK_SWITCHED_OUT() xTaskThatWasRunning = pxCurrentTCB

/* traceTASK_SWITCHED_OUT() is always called before a reschedule, and
traceTASK_SWITCHED_IN() is always called after a reschedule.  This definition
of traceTASK_SWITCHED_IN() compares the handle of the task that is in the
Running state when it is called to the handle of the task that was in the 
Running state when traceTASK_SWITCHED_OUT() was called.  If the two task 
handles do not match then a context switch has occurred, and a string is 
printed out to say which task left the Running state, and which task entered
the Running state.  The tick count value is also output.

Note how the task handles are cast to tskTCB pointers to allow the macro to
obtain the task names directly from the TCB structures. */
#define traceTASK_SWITCHED_IN()                                                    	\
    if( pxCurrentTCB != xTaskThatWasRunning )                                      	\
    {                                                                              	\
        sprintf( cBuffer, "%s switched out, %s switched in, tick count = %u\n",     \
                                ( ( tskTCB* ) xTaskThatWasRunning )->pcTaskName,   	\
                                ( ( tskTCB* ) pxCurrentTCB )->pcTaskName,          	\
                                xTickCount );                                      	\
        consoleprint( cBuffer );													\
    }

/* Output a string when a queue is created.  The queue handle is also output. */
#define traceQUEUE_CREATE( pxQueue )                                               	\
	vTaskSuspendAll();																\
    sprintf( cBuffer, "Queue 0x%x created\n", pxQueue );                            \
    consoleprint( cBuffer );                                                       	\
    xTaskResumeAll()

/* Output a string when an item is successfully sent to a queue.  The queue 
handle is also output. */
#define traceQUEUE_SEND( pxQUEUE )                                                 	\
	vTaskSuspendAll();																\
    sprintf( cBuffer, "Sent message to 0x%x\n", pxQueue );                          \
    consoleprint( cBuffer );														\
    xTaskResumeAll()

/* Ouput a string when sending to a queue fails.  The queue handle is also 
output. */
#define traceQUEUE_SEND_FAILED( pxQueue )                                          	\
	vTaskSuspendAll();																\
    sprintf( cBuffer, "Write to queue 0x%x failed\n", pxQueue );                    \
    consoleprint( cBuffer );														\
    xTaskResumeAll()

/* Output a string when a task is about to block because it called vTaskDelay().
The name of the calling task is also output. */
#define traceTASK_DELAY()                                                         	\
	vTaskSuspendAll();																\
    sprintf( cBuffer, "Task %s called vTaskDelay()\n", pxCurrentTCB->pcTaskName ); 	\
    consoleprint( cBuffer );														\
    xTaskResumeAll()
    
#endif /* TRACE_MACRO_DEFINITIONS_H */

