/*
 * erl_bif_spi.c
 *
 *  Created on: Aug 18, 2014
 *      Author: Studnicki
 */

#include "erl_bif_spi.h"
#include "FreeRTOS.h"
#include "io.h"

#define SSP_SR_BSY      ((uint32_t)(1<<4))

Eterm init_1(ErlProcess* p, Eterm* arg, UInt live) {

	LPC_SC->PCONP |= (1 << 21);
	LPC_PINCON->PINSEL0 &= ~(3UL << 30);
	LPC_PINCON->PINSEL0 |= (2UL << 30);
	LPC_PINCON->PINSEL1 &= ~((3<<2) | (3<<4));
	LPC_PINCON->PINSEL1 |= ((2<<2) | (2<<4));

	LPC_SC->PCLKSEL1 &= ~(3 << 10);
	LPC_SC->PCLKSEL1 |= (1 << 10);

	LPC_SSP0->CR0 = (0x07 << 0) |
					(0x00 << 4) |
					(0x00 << 6) |
					(0x00 << 7) |
					(0x00 << 8);

	LPC_SSP0->CR1 = (0x00 << 0) |
					(0x01 << 1) |
					(0x00 << 2) |
					(0x00 << 3);


	LPC_SSP0->CPSR = 16;
	return atom_ok;
}

Eterm rw_1(ErlProcess* p, Eterm* arg, UInt live) {
	LPC_SSP0->DR = unsigned_val(arg[0]);
	while(LPC_SSP0->SR & SSP_SR_BSY);
	return make_small(LPC_SSP0->DR & 0xff);
}
