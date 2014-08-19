/*
 * config.h
 *
 *  Created on: Apr 1, 2014
 *      Author: Studnicki
 */

#ifndef CONFIG_H_
#define CONFIG_H_

#define SMALL_BITS (28)
#define SMALL_DIGITS (8)

#define THREADED_CODE 1

#define MAX_OPCODE 154
#define SPECIAL_OPCODES 4
#define ALL_OPCODES MAX_OPCODE+SPECIAL_OPCODES
#define REDUCTIONS 100
#define MAX_PROCESSES 2
#define ATOM_TABLE_SIZE 100
#define EXPORT_TABLE_SIZE 100
#define CODE_BUFFER_SIZE 1800
#define MAX_REG 30
#define X_REGS_ALLOCATED (MAX_REG+3)

#define DEBUG 1
#define DEBUG_OP 0

#define TASK_STACK_SIZE 350

#endif /* CONFIG_H_ */
