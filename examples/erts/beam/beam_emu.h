/*
 * beam_emu.h
 *
 *  Created on: Apr 2, 2014
 *      Author: Studnicki
 */

#ifndef BEAM_EMU_H_
#define BEAM_EMU_H_

#define LABEL 0x01
#define CALL_ONLY 0x06
#define CALL_EXT 0x07
#define CALL_EXT_LAST 0x08
#define BIF0 0x09
#define BIF1 0x0A
#define BIF2 0x0B

#define IS_EQ_EXACT 0x43

#define CALL_EXT_ONLY 0x4E

#define GC_BIF1 0x7C
#define GC_BIF2 0x7D
#define GC_BIF3 0x98

//opcodes that have an external label as a first argument
#define EXTERNAL_OP_1(op) (((op)==BIF0)||((op)==BIF1)||((op)==BIF2))
//and as a second argument
#define EXTERNAL_OP_2(op) (((op)==CALL_EXT)||((op)==CALL_EXT_LAST)||((op)==CALL_EXT_ONLY))
//and as a third argument
#define EXTERNAL_OP_3(op) (((op)==GC_BIF1)||((op)==GC_BIF2)||((op)==GC_BIF3))

//opcodes that have a local label as a first argument
#define LABEL_OP_1(op) ((op)==IS_EQ_EXACT)
#define LABEL_OP_2(op) ((op)==CALL_ONLY)

#endif /* BEAM_EMU_H_ */