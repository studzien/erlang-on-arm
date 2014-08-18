/*
 * erl_bif_spi.h
 *
 *  Created on: Aug 18, 2014
 *      Author: Studnicki
 */

#ifndef ERL_BIF_SPI_H_
#define ERL_BIF_SPI_H_

#include "erl_process.h"
#include "erl_term.h"

Eterm init_1(ErlProcess*, Eterm*, UInt);
Eterm rw_1(ErlProcess*, Eterm*, UInt);

#endif /* ERL_BIF_SPI_H_ */
