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


#define MAX_OPCODE 154
#define SPECIAL_OPCODES 1
#define ALL_OPCODES MAX_OPCODE+SPECIAL_OPCODES+1
#define REDUCTIONS 3
#define MAX_PROCESSES 10
#define ATOM_TABLE_SIZE 100
#define EXPORT_TABLE_SIZE 100
#define CODE_BUFFER_SIZE 2048
#define MAX_REG 255
#define X_REGS_ALLOCATED (MAX_REG+3)

#define DEBUG 1
#define DEBUG_OP 0

#define TASK_STACK_SIZE 450

#endif /* CONFIG_H_ */
