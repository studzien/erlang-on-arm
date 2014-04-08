/*
 * config.h
 *
 *  Created on: Apr 1, 2014
 *      Author: Studnicki
 */

#ifndef CONFIG_H_
#define CONFIG_H_

#define MAX_OPCODE 153
#define SPECIAL_OPCODES 1
#define ALL_OPCODES MAX_OPCODE+SPECIAL_OPCODES+1
#define REDUCTIONS 2000
#define MAX_PROCESSES 50
#define ATOM_TABLE_SIZE 128
#define EXPORT_TABLE_SIZE 128
#define CODE_BUFFER_SIZE 1024
#define MAX_REG 255
#define X_REGS_ALLOCATED (MAX_REG+3)

#endif /* CONFIG_H_ */
