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
    FreeRTOS web site.

    1 tab == 4 spaces!

    http://www.FreeRTOS.org - Documentation, latest information, license and
    contact details.

    http://www.SafeRTOS.com - A version that is certified for use in safety
    critical systems.

    http://www.OpenRTOS.com - Commercial support, development, porting,
    licensing and training services.
*/

/* FreeRTOS.org includes. */
#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"

/* Demo includes. */
#include "basic_io.h"

/* The task functions. */
static void prvTestTask( void *pvParameters );

/* Variables used by the trace hook macros. */
xTaskHandle xTaskThatWasRunning = NULL;

/*-----------------------------------------------------------*/

int main( void )
{
	/* Init the semi-hosting. */
	printf( "\n" );

	/* Create the test task that writes to the queue until the queue is full, 
	before blocking for a fixed period, emptying the queue, and then starting 
	again. */
	xTaskCreate(	prvTestTask,/* Pointer to the function that implements the task. */
					"TestTask",	/* Text name for the task. */
					240,		/* Stack depth in words. */
					NULL,		/* The task parameter is not used by this example. */
					1,			/* This task will run at priority 1, above the idle priority. */
					NULL );		/* We are not using the task handle. */

	/* Start the scheduler so our tasks start executing. */
	vTaskStartScheduler();

	/* If all is well we will never reach here as the scheduler will now be
	running.  If we do reach here then it is likely that there was insufficient
	heap available for the idle task to be created. */
	for( ;; );
	return 0;
}
/*-----------------------------------------------------------*/

static void prvTestTask( void *pvParameters )
{
xQueueHandle xQueue; /* The queue used for the example. */
const char cCharValueToSend = 'a'; /* The value sent to the queue. */
char cCharToReceive; /* A variable that receives values from the queue. */
const portTickType xBlockTime = 500 / portTICK_RATE_MS;

	/* Create a queue of length three to be used by this example.  The 
	traceQUEUE_CREATE() macro will output a string as the queue is created. */
	xQueue = xQueueCreate( 3, sizeof( char ) );

	/* As per most tasks, this task is implemented in an infinite loop. */
	for( ;; )
	{
		/* Loop sending items to the queue until the queue is full.  The 
		traceQUEUE_SEND() macro will print out a string each time an item is
		successfully sent to the queue.  As the queue was created with a length of
		three, traceQUEUE_SEND() will print out three strings before calls to
		xQueueSend() start to fail.
		
		After three items have been sent to the queue the queue will be full and
		further calls to xQueueSend() will fail.  The traceQUEUE_SEND_FAILED() macro
		will print out a string each time xQueueSend() is called while the queue is
		full.  The while loop is exited as soon as a call to xQueueSend() fails, so
		traceQUEUE_SEND_FAILED() should only print out one string. */
		while( xQueueSend( xQueue, &cCharValueToSend, 0 ) == pdPASS )
		{
			/* Nothing to do here, the queue is just being filled. */
		}
		
		/* The while() loop has been exited because the queue is full.  Block
		this task for a fixed period.  This is really done to slow down the rate at
		which the trace macros output strings, but in this case it will also cause 
		the traceTASK_DELAY() macro to output its string.
		
		The Idle task is the only other task created by this example, so once this
		task has entered the Blocked state (and is therefore not available to the
		scheduler) the scheduler must select the Idle task as the next task to enter
		the Running state.  This context switch will result in the 
		traceTASK_SWITCHED_IN() macro outputting its string to state which task has 
		exited the Running state and which task has entered the Running state. */
		vTaskDelay( xBlockTime );
		
		/* This task has a higher priority than the Idle task, so when xBlockTime
		ticks have passed the scheduler will select this task to be the Running state
		task again.  As before, this will result in the traceTASK_SWITCHED_IN() macro
		outputting its sting to state which task has exited the Running state and 
		which task has entered the Running state. */
		
		/* Empty the queue again so the sequence can be repeated. */
		while( xQueueReceive( xQueue, &cCharToReceive, 0 ) == pdPASS )
		{
			/* Nothing to do here, the queue is just being emptied. */
		}
		
		/* Just to break up the console output between iterations of this
		task loop. */
		vPrintString( "\n\n" );
	}
}
/*-----------------------------------------------------------*/

void vApplicationMallocFailedHook( void )
{
	/* This function will only be called if an API call to create a task, queue
	or semaphore fails because there is too little heap RAM remaining. */
	for( ;; );
}
/*-----------------------------------------------------------*/

void vApplicationStackOverflowHook( xTaskHandle *pxTask, signed char *pcTaskName )
{
	/* This function will only be called if a task overflows its stack.  Note
	that stack overflow checking does slow down the context switch
	implementation. */
	for( ;; );
}
/*-----------------------------------------------------------*/

void vApplicationIdleHook( void )
{
	/* This example does not use the idle hook to perform any processing. */
}
/*-----------------------------------------------------------*/

void vApplicationTickHook( void )
{
	/* This example does not use the tick hook to perform any processing. */
}


