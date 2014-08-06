/*
 * erl_bif_lpc.h
 *
 *  Created on: Jul 16, 2014
 *      Author: Studnicki
 */

#ifndef ERL_BIF_LPC_H_
#define ERL_BIF_LPC_H_

#include "erl_process.h"
#include "erl_term.h"

Eterm output_2(ErlProcess*, Eterm*, UInt);
Eterm high_2(ErlProcess*, Eterm*, UInt);
Eterm low_2(ErlProcess*, Eterm*, UInt);

Eterm print_term1(ErlProcess*, Eterm*, UInt);
Eterm print_info0(ErlProcess*, Eterm*, UInt);
Eterm print_heap_size0(ErlProcess*, Eterm*, UInt);
Eterm dump_stack0(ErlProcess*, Eterm*, UInt);
Eterm dump_regs0(ErlProcess*, Eterm*, UInt);
Eterm dump_heap0(ErlProcess*, Eterm*, UInt);

static inline LPC_GPIO_TypeDef* gpio_port(UInt port);

#endif /* ERL_BIF_LPC_H_ */
