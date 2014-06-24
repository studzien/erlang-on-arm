/*
 * beam_emu.h
 *
 *  Created on: Apr 2, 2014
 *      Author: Studnicki
 */

#ifndef BEAM_EMU_H_
#define BEAM_EMU_H_

#include "erl_process.h"
#include "io.h"

void process_main(void* p);
BeamInstr* apply(ErlProcess* p, Eterm module, Eterm function, Eterm args);
inline void restore_registers(ErlProcess* p);
static inline void allocate_heap(ErlProcess* p, UInt stack_need, UInt heap_need, UInt live);

#define OpCase(OpCode) lb_##OpCode
#define OpCode(OpCode) (&&lb_##OpCode)
#define Goto(Rel) goto *(Rel)

#define x(N) reg[N]
#define y(N) E[N]
#define r(N) x##N

#define Arg(N) p->i[(N)+1]

#define Resolve(Arg, Dest) do {     \
	if(is_rreg(Arg))                \
		Dest = x0;                  \
	else if(is_xreg(Arg))           \
		Dest = x(x_reg_index(Arg)); \
	else if(is_yreg(Arg))           \
		Dest = y(y_reg_index(Arg)); \
	else                            \
		Dest = Arg;                 \
} while(0)

#define Move(Arg, Dest) do {        \
	if(is_rreg(Dest))		        \
		x0 = Arg;                   \
	else if(is_xreg(Dest))          \
		x(x_reg_index(Dest)) = Arg; \
	else if(is_yreg(Dest))          \
		y(y_reg_index(Dest)) = Arg; \
	else                            \
		Dest = Arg;                 \
} while(0)                          \

#define LABEL 1
#define FUNC_INFO 2
#define INT_CODE_END 3
#define CALL 4
#define CALL_LAST 5
#define CALL_ONLY 6
#define CALL_EXT 7
#define CALL_EXT_LAST 8
#define BIF0 9
#define BIF1 10
#define BIF2 11
#define ALLOCATE 12
#define ALLOCATE_HEAP 13
#define ALLOCATE_ZERO 14
#define ALLOCATE_HEAP_ZERO 15
#define TEST_HEAP 16
#define INIT 17
#define DEALLOCATE 18
#define RETURN 19
#define SEND 20
#define REMOVE_MESSAGE 21
#define TIMEOUT 22
#define LOOP_REC 23
#define LOOP_REC_END 24
#define WAIT 25
#define WAIT_TIMEOUT 26
#define M_PLUS 27
#define M_MINUS 28
#define M_TIMES 29
#define M_DIV 30
#define INT_DIV 31
#define INT_REM 32
#define INT_BAND 33
#define INT_BOR 34
#define INT_BXOR 35
#define INT_BSL 36
#define INT_BSR 37
#define INT_BNOT 38
#define IS_LT 39
#define IS_GE 40
#define IS_EQ 41
#define IS_NE 42
#define IS_EQ_EXACT 43
#define IS_NE_EXACT 44
#define IS_INTEGER 45
#define IS_FLOAT 46
#define IS_NUMBER 47
#define IS_ATOM 48
#define IS_PID 49
#define IS_REFERENCE 50
#define IS_PORT 51
#define IS_NIL 52
#define IS_BINARY 53
#define IS_CONSTANT 54
#define IS_LIST 55
#define IS_NONEMPTY_LIST 56
#define IS_TUPLE 57
#define TEST_ARITY 58
#define SELECT_VAL 59
#define SELECT_TUPLE_ARITY 60
#define JUMP 61
#define CATCH 62
#define CATCH_END 63
#define MOVE 64
#define GET_LIST 65
#define GET_TUPLE_ELEMENT 66
#define SET_TUPLE_ELEMENT 67
#define PUT_STRING 68
#define PUT_LIST 69
#define PUT_TUPLE 70
#define PUT 71
#define BADMATCH 72
#define IF_END 73
#define CASE_END 74
#define CALL_FUN 75
#define MAKE_FUN 76
#define IS_FUNCTION 77
#define CALL_EXT_ONLY 78
#define BS_START_MATCH 79
#define BS_GET_INTEGER 80
#define BS_GET_FLOAT 81
#define BS_GET_BINARY 82
#define BS_SKIP_BITS 83
#define BS_TEST_TAIL 84
#define BS_SAVE 85
#define BS_RESTORE 86
#define BS_INIT 87
#define BS_FINAL 88
#define BS_PUT_INTEGER 89
#define BS_PUT_BINARY 90
#define BS_PUT_FLOAT 91
#define BS_PUT_STRING 92
#define BS_NEED_BUF 93
#define FCLEARERROR 94
#define FCHECKERROR 95
#define FMOVE 96
#define FCONV 97
#define FADD 98
#define FSUB 99
#define FMUL 100
#define FDIV 101
#define FNEGATE 102
#define MAKE_FUN2 103
#define TRY 104
#define TRY_END 105
#define TRY_CASE 106
#define TRY_CASE_END 107
#define RAISE 108
#define BS_INIT2 109
#define BS_BITS_TO_BYTES 110
#define BS_ADD 111
#define APPLY 112
#define APPLY_LAST 113
#define IS_BOOLEAN 114
#define IS_FUNCTION2 115
#define BS_START_MATCH2 116
#define BS_GET_INTEGER2 117
#define BS_GET_FLOAT2 118
#define BS_GET_BINARY2 119
#define BS_SKIP_BITS2 120
#define BS_TEST_TAIL2 121
#define BS_SAVE2 122
#define BS_RESTORE2 123
#define GC_BIF1 124
#define GC_BIF2 125
#define BS_FINAL2 126
#define BS_BITS_TO_BYTES2 127
#define PUT_LITERAL 128
#define IS_BITSTR 129
#define BS_CONTEXT_TO_BINARY 130
#define BS_TEST_UNIT 131
#define BS_MATCH_STRING 132
#define BS_INIT_WRITABLE 133
#define BS_APPEND 134
#define BS_PRIVATE_APPEND 135
#define TRIM 136
#define BS_INIT_BITS 137
#define BS_GET_UTF8 138
#define BS_SKIP_UTF8 139
#define BS_GET_UTF16 140
#define BS_SKIP_UTF16 141
#define BS_GET_UTF32 142
#define BS_SKIP_UTF32 143
#define BS_UTF8_SIZE 144
#define BS_PUT_UTF8 145
#define BS_UTF16_SIZE 146
#define BS_PUT_UTF16 147
#define BS_PUT_UTF32 148
#define ON_LOAD 149
#define RECV_MARK 150
#define RECV_SET 151
#define GC_BIF3 152
#define LINE 153
#define BEAM_APPLY 154
#define NORMAL_EXIT 155

//opcodes that have an external label as a first argument
#define EXTERNAL_OP_1(op) (((op)==BIF0)||((op)==BIF1)||((op)==BIF2))
//and as a second argument
#define EXTERNAL_OP_2(op) (((op)==CALL_EXT)||((op)==CALL_EXT_LAST)||((op)==CALL_EXT_ONLY))
//and as a third argument
#define EXTERNAL_OP_3(op) (((op)==GC_BIF1)||((op)==GC_BIF2)||((op)==GC_BIF3))

//opcodes that have a local label as a first argument
#define LABEL_OP_1(op) ((op)==IS_EQ_EXACT)
#define LABEL_OP_2(op) ((op)==CALL_ONLY || (op)==CALL)

#define JUMP_TABLE NULL,\
		&&lb_LABEL,\
		&&lb_FUNC_INFO,\
		&&lb_INT_CODE_END,\
		&&lb_CALL,\
		&&lb_CALL_LAST,\
		&&lb_CALL_ONLY,\
		&&lb_CALL_EXT,\
		&&lb_CALL_EXT_LAST,\
		&&lb_BIF0,\
		&&lb_BIF1,\
		&&lb_BIF2,\
		&&lb_ALLOCATE,\
		&&lb_ALLOCATE_HEAP,\
		&&lb_ALLOCATE_ZERO,\
		&&lb_ALLOCATE_HEAP_ZERO,\
		&&lb_TEST_HEAP,\
		&&lb_INIT,\
		&&lb_DEALLOCATE,\
		&&lb_RETURN,\
		&&lb_SEND,\
		&&lb_REMOVE_MESSAGE,\
		&&lb_TIMEOUT,\
		&&lb_LOOP_REC,\
		&&lb_LOOP_REC_END,\
		&&lb_WAIT,\
		&&lb_WAIT_TIMEOUT,\
		&&lb_M_PLUS,\
		&&lb_M_MINUS,\
		&&lb_M_TIMES,\
		&&lb_M_DIV,\
		&&lb_INT_DIV,\
		&&lb_INT_REM,\
		&&lb_INT_BAND,\
		&&lb_INT_BOR,\
		&&lb_INT_BXOR,\
		&&lb_INT_BSL,\
		&&lb_INT_BSR,\
		&&lb_INT_BNOT,\
		&&lb_IS_LT,\
		&&lb_IS_GE,\
		&&lb_IS_EQ,\
		&&lb_IS_NE,\
		&&lb_IS_EQ_EXACT,\
		&&lb_IS_NE_EXACT,\
		&&lb_IS_INTEGER,\
		&&lb_IS_FLOAT,\
		&&lb_IS_NUMBER,\
		&&lb_IS_ATOM,\
		&&lb_IS_PID,\
		&&lb_IS_REFERENCE,\
		&&lb_IS_PORT,\
		&&lb_IS_NIL,\
		&&lb_IS_BINARY,\
		&&lb_IS_CONSTANT,\
		&&lb_IS_LIST,\
		&&lb_IS_NONEMPTY_LIST,\
		&&lb_IS_TUPLE,\
		&&lb_TEST_ARITY,\
		&&lb_SELECT_VAL,\
		&&lb_SELECT_TUPLE_ARITY,\
		&&lb_JUMP,\
		&&lb_CATCH,\
		&&lb_CATCH_END,\
		&&lb_MOVE,\
		&&lb_GET_LIST,\
		&&lb_GET_TUPLE_ELEMENT,\
		&&lb_SET_TUPLE_ELEMENT,\
		&&lb_PUT_STRING,\
		&&lb_PUT_LIST,\
		&&lb_PUT_TUPLE,\
		&&lb_PUT,\
		&&lb_BADMATCH,\
		&&lb_IF_END,\
		&&lb_CASE_END,\
		&&lb_CALL_FUN,\
		&&lb_MAKE_FUN,\
		&&lb_IS_FUNCTION,\
		&&lb_CALL_EXT_ONLY,\
		&&lb_BS_START_MATCH,\
		&&lb_BS_GET_INTEGER,\
		&&lb_BS_GET_FLOAT,\
		&&lb_BS_GET_BINARY,\
		&&lb_BS_SKIP_BITS,\
		&&lb_BS_TEST_TAIL,\
		&&lb_BS_SAVE,\
		&&lb_BS_RESTORE,\
		&&lb_BS_INIT,\
		&&lb_BS_FINAL,\
		&&lb_BS_PUT_INTEGER,\
		&&lb_BS_PUT_BINARY,\
		&&lb_BS_PUT_FLOAT,\
		&&lb_BS_PUT_STRING,\
		&&lb_BS_NEED_BUF,\
		&&lb_FCLEARERROR,\
		&&lb_FCHECKERROR,\
		&&lb_FMOVE,\
		&&lb_FCONV,\
		&&lb_FADD,\
		&&lb_FSUB,\
		&&lb_FMUL,\
		&&lb_FDIV,\
		&&lb_FNEGATE,\
		&&lb_MAKE_FUN2,\
		&&lb_TRY,\
		&&lb_TRY_END,\
		&&lb_TRY_CASE,\
		&&lb_TRY_CASE_END,\
		&&lb_RAISE,\
		&&lb_BS_INIT2,\
		&&lb_BS_BITS_TO_BYTES,\
		&&lb_BS_ADD,\
		&&lb_APPLY,\
		&&lb_APPLY_LAST,\
		&&lb_IS_BOOLEAN,\
		&&lb_IS_FUNCTION2,\
		&&lb_BS_START_MATCH2,\
		&&lb_BS_GET_INTEGER2,\
		&&lb_BS_GET_FLOAT2,\
		&&lb_BS_GET_BINARY2,\
		&&lb_BS_SKIP_BITS2,\
		&&lb_BS_TEST_TAIL2,\
		&&lb_BS_SAVE2,\
		&&lb_BS_RESTORE2,\
		&&lb_GC_BIF1,\
		&&lb_GC_BIF2,\
		&&lb_BS_FINAL2,\
		&&lb_BS_BITS_TO_BYTES2,\
		&&lb_PUT_LITERAL,\
		&&lb_IS_BITSTR,\
		&&lb_BS_CONTEXT_TO_BINARY,\
		&&lb_BS_TEST_UNIT,\
		&&lb_BS_MATCH_STRING,\
		&&lb_BS_INIT_WRITABLE,\
		&&lb_BS_APPEND,\
		&&lb_BS_PRIVATE_APPEND,\
		&&lb_TRIM,\
		&&lb_BS_INIT_BITS,\
		&&lb_BS_GET_UTF8,\
		&&lb_BS_SKIP_UTF8,\
		&&lb_BS_GET_UTF16,\
		&&lb_BS_SKIP_UTF16,\
		&&lb_BS_GET_UTF32,\
		&&lb_BS_SKIP_UTF32,\
		&&lb_BS_UTF8_SIZE,\
		&&lb_BS_PUT_UTF8,\
		&&lb_BS_UTF16_SIZE,\
		&&lb_BS_PUT_UTF16,\
		&&lb_BS_PUT_UTF32,\
		&&lb_ON_LOAD,\
		&&lb_RECV_MARK,\
		&&lb_RECV_SET,\
		&&lb_GC_BIF3,\
		&&lb_LINE,\
		&&lb_BEAM_APPLY,\
		&&lb_NORMAL_EXIT

static uint8_t opcode_arities[] = {0,1,3,0,2,3,2,2,3,2,4,5,2,3,2,3,2,1,1,0,0,0,0,2,1,1,2,4,4,4,
		 4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,
		 3,1,2,1,2,3,3,3,3,3,2,1,1,0,1,1,3,2,2,2,5,5,5,4,2,1,1,2,2,5,
		 5,5,2,1,0,1,2,2,4,4,4,4,3,1,2,1,1,1,2,6,3,5,1,2,2,3,5,7,7,7,
		 5,3,2,2,5,6,2,2,2,2,1,3,4,0,8,6,2,6,5,4,5,4,5,4,3,3,3,3,3,0,
		 1,1,7,1};

#endif /* BEAM_EMU_H_ */
