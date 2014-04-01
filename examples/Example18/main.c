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

/* FreeRTOS.org includes. */
#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"

/* The task functions. */
static void prvDemoTask( void *pvParameters );
static void prvStatsTask( void *pvParameters );

/*-----------------------------------------------------------*/

int main( void )
{
	/* Init the semi-hosting. */
	printf( "\n" );

	/* Two tasks are created above the idle task priority (at tskIDLE_PRIORITY + 
	1) that just use up some processing time before blocking for a very short 
	period. */
	xTaskCreate( prvDemoTask, "Demo1", 240, NULL,	tskIDLE_PRIORITY + 1, NULL );
	xTaskCreate( prvDemoTask, "Demo2", 240, NULL, tskIDLE_PRIORITY + 1, NULL );

	/* The highest priority task (created at tskIDLE_PRIORITY + 2 ) runs every 
	five seconds and prints out the run time stats. */
	xTaskCreate( prvStatsTask, "Stats", 240, NULL, tskIDLE_PRIORITY + 2, NULL );

	/* Start the scheduler so the test task starts executing. */
	vTaskStartScheduler();	
	
	/* If all is well we will never reach here as the scheduler will now be
	running.  If we do reach here then it is likely that there was insufficient
	heap available for the idle task to be created. */
	for( ;; );
	return 0;
}
/*-----------------------------------------------------------*/

static void prvDemoTask( void *pvParameters )
{
volatile unsigned long ulLoopCounter;
const unsigned long ulMaxLoopCount = 0x1fffUL;
portTickType xLastExecutionTime;

/* The task will run every 5 milliseconds. */
const portTickType xBlockPeriod = ( 5 / portTICK_RATE_MS );

	/* Initialise xLastExecutionTime to the current time.  This is the only
	time this variable needs to be written to explicitly.  Afterwards it is
	updated internally within the xTaskDelayUnitl() API function. */
	xLastExecutionTime = xTaskGetTickCount();

	/* As per most tasks, this task is implemented in an infinite loop. */
	for( ;; )
	{
		/* Wait until it is time to run this task again. */
		vTaskDelayUntil( &xLastExecutionTime, xBlockPeriod );
		
		/* This loop is just to ensure the task uses up enough processing time
		to register in the run time statistics. */
		for( ulLoopCounter = 0; ulLoopCounter < ulMaxLoopCount; ulLoopCounter++ )
		{
			/* There is nothing to do here.  Just perform a "no operation" to
			ensure there are some instructions generated. */
			__asm volatile( "NOP " );
		}
	}
}
/*-----------------------------------------------------------*/

static void prvStatsTask( void *pvParameters )
{
portTickType xLastExecutionTime;

/* The buffer used to hold the run time stats text needs to be quite large.  It 
is therefore declared static to ensure it is not allocated on the task stack.  
This makes this function non re-entrant. */
static signed char cStringBuffer[ 512 ]; 

/* The task will run every 5 seconds. */
const portTickType xBlockPeriod = ( 5000 / portTICK_RATE_MS );

	/* Initialise xLastExecutionTime to the current time.  This is the only
	time this variable needs to be written to explicitly.  Afterwards it is
	updated internally within the xTaskDelayUntil() API function. */
	xLastExecutionTime = xTaskGetTickCount();

	/* As per most tasks, this task is implemented in an infinite loop. */
	for( ;; )
	{
		/* Wait until it is time to run this task again. */
		vTaskDelayUntil( &xLastExecutionTime, xBlockPeriod );

		/* Generate a text table from the run time stats.  This must fit into
		the cStringBuffer array. */
		vTaskGetRunTimeStats( cStringBuffer );
		
		/* Print out column headings for the run time stats table. */
		consoleprint( "\nTask\t\tAbs\t\t%\n" );
		consoleprint( "------------------------------------" );
		
		/* Print out the run time stats themselves. */
		consoleprint( cStringBuffer );
	}
}
/*-----------------------------------------------------------*/

/* This function configures a timer to be used as the run time statistics time 
base.  The function implementation requires knowledge of the LPC17xx peripheral 
registers, and uses macros that are available when the LPC17xx.h header file is 
used.  In this example LPC17xx.h is included from FreeRTOSConfig.h. */
void vSetupTimerForRunTimeStats( void )
{
const unsigned long TCR_COUNT_RESET = 2, CTCR_CTM_TIMER = 0x00, TCR_COUNT_ENABLE = 0x01;

	/* Power up and drive the timer 0. */
	LPC_SC->PCONP |= 0x02UL;
	LPC_SC->PCLKSEL0 = ( LPC_SC->PCLKSEL0 & (~(0x3<<2)) ) | ( 0x01 << 2 );

	/* Reset Timer 0 */
	LPC_TIM0->TCR = TCR_COUNT_RESET;

	/* The timer needs to just count up continuously. */
	LPC_TIM0->CTCR = CTCR_CTM_TIMER;

	/* The clock driving the timer is prescaled to a frequency that is good enough 
	to get a decent resolution,	but not so fast that the counter value will
	overflow too quickly. */
	LPC_TIM0->PR =  ( configCPU_CLOCK_HZ / 10000UL ) - 1UL;

	/* Start the counter. */
	LPC_TIM0->TCR = TCR_COUNT_ENABLE;
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


