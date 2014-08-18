/*
 * beam_emu.c
 *
 *  Created on: Oct 26, 2013
 *      Author: Studnicki
 */

#include "global.h"
#include "beam_emu.h"
#include "beam_load.h"
#include "export.h"
#include "erl_init.h"
#include "erl_process.h"
#include "erl_message.h"
#include "erl_utils.h"
#include "task.h"
#include "semphr.h"

int init_done = 0;
int sent = 0;
int received = 0;
int suspended = 0;
int resumed = 0;
int continued = 0;

Eterm x0;
Eterm reg[X_REGS_ALLOCATED];
Eterm* E, *HTOP;
Eterm tmp0, tmp1;
Eterm* tmp_ptr, *ptr;
Export *e;
BeamInstr* next;
UInt StackN, Live, HeapN, need;
ErlMessage *msgp;
BeamInstr** saved_i;
UInt arity, desired_arity;
int i;
Eterm tuple, term;
Export tmp;
BeamInstr* temp_i;
Eterm result;
Eterm bif_args[3];

char buf[50];
int i;

BeamInstr beam_apply[3];
extern void* jump_table[];
extern UInt reclaimed;
extern UInt garbage_cols;

#define RESTORE_I(PROCESS, WHERE, MODIFIER) do { \
	WHERE = (PROCESS)->saved_i + MODIFIER; \
} while(0)

#define SWAPIN             \
    HTOP = HEAP_TOP(p);  \
    E = p->stop;

#define SWAPOUT            \
    HEAP_TOP(p) = HTOP;  \
    p->stop = E; \

static void yield_maybe(ErlProcess* p, uint8_t arity) {
	p->arity = arity;
	if(p->fcalls < 0) {
		#if (DEBUG_OP == 1)
		sprintf(buf, "pid %d context switch\n", pid2pix(p->id));
		debug(buf);
		#endif

		if(p->arity > p->max_arg_reg) {
			if(p->arg_reg != p->def_arg_reg) {
				vPortFree(p->arg_reg);
			}
			p->arg_reg = (Eterm*)pvPortMalloc(p->arity * sizeof(p->arg_reg[0]));
			p->max_arg_reg = p->arity;
		}
		for(i=1; i<p->arity; i++) {
			p->arg_reg[i] = x(i);
		}
		p->arg_reg[0] = r(0);
		p->fcalls = REDUCTIONS;

		SWAPOUT;
		taskYIELD();
		SWAPIN;

		restore_registers(p);
	}
	p->fcalls--;
}

void process_main(void* arg) {
	ErlProcess* p = (ErlProcess*)arg;

	//first time this function is called op labels from here are exported to the loader
	if(!init_done) {
		int i;

#if (THREADED_CODE == 1)
		static const void* temp[] = { JUMP_TABLE };
		for(i=0; i<ALL_OPCODES; i++) {
				jump_table_add(i, temp[i]);
		}
		beam_apply[0] = (BeamInstr)jump_table[BEAM_APPLY];
		beam_apply[1] = (BeamInstr)jump_table[NORMAL_EXIT];
		beam_apply[2] = (BeamInstr)jump_table[NOP];
#else
		beam_apply[0] = (BeamInstr)BEAM_APPLY;
		beam_apply[1] = (BeamInstr)NORMAL_EXIT;
		beam_apply[2] = (BeamInstr)NOP;
#endif

		init_done = 1;
		return;
	}

	SWAPIN;
	Goto(p);

	switch_loop:

	LOOP_BEGIN

	OpCase(LABEL):
		debug_op2(p,"label\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(FUNC_INFO):
		debug("function_clause\n");
		sprintf(buf, "%d %d %d %d\n", Arg(0), Arg(1), Arg(2), p->id);
		debug(buf);
		erts_do_exit_process(p, atom_normal);
	OpCase(INT_CODE_END):
		debug_op2(p,"int_code_end\n");
		RESTORE_I(p, p->i, 1);
	OpCase(CALL):
		debug_op2(p,"call\n");
		yield_maybe(p, (uint8_t)unsigned_val(Arg(0)));
		RESTORE_I(p, p->cp, 3);
		p->i = (BeamInstr*)(Arg(1));
		Goto(p);
	OpCase(CALL_LAST):
		debug_op2(p,"call_last\n");
		yield_maybe(p, (uint8_t)unsigned_val(Arg(0)));
		p->cp = (BeamInstr*)E[0];
		E += unsigned_val(Arg(2)) + 1;
		p->i = (BeamInstr*)(Arg(1));
		Goto(p);
	OpCase(CALL_ONLY):
		debug_op2(p,"call_only\n");
		yield_maybe(p, (uint8_t)unsigned_val(Arg(0)));
		p->i = (BeamInstr*)(Arg(1));
		Goto(p);
	OpCase(CALL_EXT):
		debug_op2(p,"call_ext\n");
		yield_maybe(p, unsigned_val(Arg(0)));
		e = (Export*)(Arg(1));
		RESTORE_I(p, p->cp, 3);
		if(e->bif != NULL) {
			SWAPOUT;
			reg[0] = r(0);
			r(0) = (e->bif)(p, reg, p->arity);
			if(p->saved_i != (BeamInstr*)(beam_apply+2)) {
				p->i = p->cp;
			}
			SWAPIN;
			Goto(p);
		}
		p->i = e->address;
		Goto(p);
	OpCase(CALL_EXT_LAST):
		debug_op2(p,"call_ext_last\n");
		yield_maybe(p, (uint8_t)unsigned_val(Arg(0)));
		e = (Export*)(Arg(1));
		p->cp = (BeamInstr*)E[0];
		E += unsigned_val(Arg(2)) + 1;
		if(e->bif != NULL) {
			SWAPOUT;
			reg[0] = r(0);
			r(0) = (e->bif)(p, reg, p->arity);
			if(p->saved_i != (BeamInstr*)(beam_apply+2)) {
				p->i = p->cp;
			}
			SWAPIN;
			Goto(p);
		}
		p->i = e->address;
		Goto(p);
	OpCase(BIF0):
		debug_op2(p,"bif0\n");
		e = (Export*)Arg(0);
		result = (e->bif)(p, NULL, 0);
		Move(result, Arg(1));
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BIF1):
		debug_op2(p,"bif1\n");
		e = (Export*)Arg(1);
		Resolve(Arg(2), bif_args[0]);
		result = (e->bif)(p, bif_args, 0);
		Move(result, Arg(3));
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(BIF2):
		debug_op2(p,"bif2\n");
		e = (Export*)Arg(1);
		Resolve(Arg(2), bif_args[0]);
		Resolve(Arg(3), bif_args[1]);
		result = (e->bif)(p, bif_args, 0);
		Move(result, Arg(4));
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(ALLOCATE):
		debug_op2(p,"allocate\n");
		StackN = unsigned_val(Arg(0));
		Live = unsigned_val(Arg(1));
		allocate_heap(p, StackN, 0, Live);
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(ALLOCATE_HEAP):
		debug_op2(p,"allocate_heap\n");
		StackN = unsigned_val(Arg(0));
		HeapN = unsigned_val(Arg(1));
		Live = unsigned_val(Arg(2));
		allocate_heap(p, StackN, HeapN, Live);
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(ALLOCATE_ZERO):
		debug_op2(p,"allocate_zero\n");
		StackN = unsigned_val(Arg(0));
		Live = unsigned_val(Arg(1));
		allocate_heap(p, StackN, 0, Live);
		for(ptr = E+StackN; E < ptr; ptr--) {
			make_blank(*ptr);
		}
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(ALLOCATE_HEAP_ZERO):
		debug_op2(p,"allocate_heap_zero\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(TEST_HEAP):
		debug_op2(p,"test_heap\n");
		Resolve(Arg(0), tmp0);
		Resolve(Arg(1), tmp1);
		test_heap(p, unsigned_val(tmp0), unsigned_val(tmp1));
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(INIT):
		debug_op2(p,"init\n");
		Move(NIL, Arg(0));
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(DEALLOCATE):
		debug_op2(p,"deallocate\n");
		p->cp = (BeamInstr*)E[0];
		E += unsigned_val(Arg(0)) + 1;
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(RETURN):
		debug_op2(p,"return\n");
		p->i = p->cp;
		Goto(p);
	OpCase(SEND):
		debug_op2(p,"send\n");
		yield_maybe(p, 2);
		erts_send_message(p, r(0), x(1), 0);
		RESTORE_I(p, p->i, 1);
		Goto(p);
	OpCase(REMOVE_MESSAGE):
		debug_op2(p,"remove_message\n");
		taskENTER_CRITICAL();
		msgp = PEEK_MESSAGE(p);
		UNLINK_MESSAGE(p, msgp);
		JOIN_MESSAGE(p);
		cancel_timer(p);
		taskEXIT_CRITICAL();
		if(msgp->data) {
			vPortFree(msgp->data);
		}
		vPortFree(msgp);
		RESTORE_I(p, p->i, 1);
		Goto(p);
	OpCase(TIMEOUT):
		debug_op2(p,"timeout\n");
		p->flags &= ~F_TIMO;
		JOIN_MESSAGE(p);
		RESTORE_I(p, p->i, 1);
		Goto(p);
	OpCase(LOOP_REC):
		debug_op2(p,"loop_rec\n");
		taskENTER_CRITICAL();
		msgp = PEEK_MESSAGE(p);
		taskEXIT_CRITICAL();
		if(!msgp) {
			p->i = (BeamInstr*)Arg(0);
		}
		else {
			received++;
			RESTORE_I(p, p->i, 3);
			if(msgp->data) {
				need = msgp->data->used_size;
				if(E - HTOP >= need) {
					msgp->m = copy_struct(msgp->m, need, &HTOP);
					vPortFree(msgp->data);
					msgp->data = NULL;
				}
				else {
					SWAPOUT;
					p->fcalls -= erts_garbage_collect(p, 0, NULL, 0);
					SWAPIN;
				}
			}
			r(0) = msgp->m;
		}
		Goto(p);
	OpCase(LOOP_REC_END):
		debug_op2(p,"loop_rec_end\n");
		SAVE_MESSAGE(p);
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(WAIT):
		debug_op2(p,"wait\n");
		SWAPOUT;
		suspended++;
		vTaskSuspend(*(p->handle));
		continued++;
		SWAPIN;
		p->i = (BeamInstr*)Arg(0);
		Goto(p);
	OpCase(WAIT_TIMEOUT):
		debug_op2(p,"wait_timeout\n");
		if ((p->flags & (F_INSLPQUEUE | F_TIMO)) == 0) {
			saved_i = (BeamInstr**)p->def_arg_reg;
			*saved_i = p->i+3;
			set_timer(p, unsigned_val(Arg(1)));
		}
		p->i = (BeamInstr*)Arg(0);
		SWAPOUT;
		suspended++;
		vTaskSuspend(*(p->handle));
		continued++;
		SWAPIN;
		Goto(p);
	OpCase(M_PLUS):
		debug_op2(p,"m_plus\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(M_MINUS):
		debug_op2(p,"m_minus\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(M_TIMES):
		debug_op2(p,"m_times\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(M_DIV):
		debug_op2(p,"m_div\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(INT_DIV):
		debug_op2(p,"int_div\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(INT_REM):
		debug_op2(p,"int_rem\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(INT_BAND):
		debug_op2(p,"int_band\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(INT_BOR):
		debug_op2(p,"int_bor\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(INT_BXOR):
		debug_op2(p,"int_bxor\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(INT_BSL):
		debug_op2(p,"int_bsl\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(INT_BSR):
		debug_op2(p,"int_bsr\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(INT_BNOT):
		debug_op2(p,"int_bnot\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(IS_LT):
		debug_op2(p,"is_lt\n");
		Resolve(Arg(1), tmp0);
		Resolve(Arg(2), tmp1);
		if(tmp0 < tmp1) {
			RESTORE_I(p, p->i, 4);
		}
		else {
			p->i = (BeamInstr*)(Arg(0));
		}
		Goto(p);
	OpCase(IS_GE):
		debug_op2(p,"is_ge\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(IS_EQ):
		debug_op2(p,"is_eq\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(IS_NE):
		debug_op2(p,"is_ne\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(IS_EQ_EXACT):
		debug_op2(p,"is_eq_exact\n");
		Resolve(Arg(1), tmp0);
		Resolve(Arg(2), tmp1);
		if(EQ(tmp0, tmp1)) {
			RESTORE_I(p, p->i, 4);
		}
		else {
			p->i = (BeamInstr*)(Arg(0));
		}
		Goto(p);
	OpCase(IS_NE_EXACT):
		debug_op2(p,"is_ne_exact\n");
		Resolve(Arg(1), tmp0);
		Resolve(Arg(2), tmp1);
		if(!EQ(tmp0, tmp1)) {
			RESTORE_I(p, p->i, 4);
		}
		else {
			p->i = (BeamInstr*)(Arg(0));
		}
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(IS_INTEGER):
		debug_op2(p,"is_integer\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_FLOAT):
		debug_op2(p,"is_float\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_NUMBER):
		debug_op2(p,"is_number\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_ATOM):
		debug_op2(p,"is_atom\n");
		Resolve(Arg(1), tmp1);
		if(!is_atom(tmp1)) {
			p->i = (BeamInstr*)Arg(0);
		}
		else {
			RESTORE_I(p, p->i, 3);
		}
		Goto(p);
	OpCase(IS_PID):
		debug_op2(p,"is_pid\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_REFERENCE):
		debug_op2(p,"is_reference\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_PORT):
		debug_op2(p,"is_port\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_NIL):
		debug_op2(p,"is_nil\n");
		Resolve(Arg(1), tmp1);
		if(is_not_nil(tmp1)) {
			p->i = (BeamInstr*)(Arg(0));
		}
		else {
			RESTORE_I(p, p->i, 3);
		}
		Goto(p);
	OpCase(IS_BINARY):
		debug_op2(p,"is_binary\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_CONSTANT):
		debug_op2(p,"is_constant\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_LIST):
		debug_op2(p,"is_list\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_NONEMPTY_LIST):
		debug_op2(p,"is_nonempty_list\n");
		Resolve(Arg(1), tmp1);
		if(is_not_list(tmp1)) {
			p->i = (BeamInstr*)(Arg(0));
		}
		else {
			RESTORE_I(p, p->i, 3);
		}
		Goto(p);
	OpCase(IS_TUPLE):
		debug_op2(p,"is_tuple\n");
		Resolve(Arg(1), tmp1);
		if(is_not_tuple(tmp1)) {
			p->i = (BeamInstr*)(Arg(0));
		}
		else {
			RESTORE_I(p, p->i, 3);
		}
		Goto(p);
	OpCase(TEST_ARITY):
		debug_op2(p,"test_arity\n");
		Resolve(Arg(1), tmp0);
		Resolve(Arg(2), tmp1);
		if(header_arity(*tuple_val(tmp0)) != unsigned_val(tmp1)) {
			p->i = (BeamInstr*)(Arg(0));
		}
		else {
			RESTORE_I(p, p->i, 4);
		}
		Goto(p);
	OpCase(SELECT_VAL):
		debug_op2(p,"select_val\n");
		arity = unsigned_val(Arg(2));
		Resolve(Arg(0), tmp1);
		for(i=0; i<arity; i+=2) {
			if(Arg(3+i) == tmp1) {
				p->i = (BeamInstr*)(Arg(4+i));
				Goto(p);
			}
		}
		p->i = (BeamInstr*)Arg(1);
		Goto(p);
	OpCase(SELECT_TUPLE_ARITY):
		debug_op2(p,"select_tuple_arity\n");
		arity = unsigned_val(Arg(2));
		Resolve(Arg(0), tmp0);
		desired_arity = arityval(*boxed_val(tmp0));
		for(i=0; i<arity; i+=2) {
			if(desired_arity == unsigned_val(Arg(3+i))) {
				p->i = (BeamInstr*)(Arg(4+i));
				Goto(p);
			}
		}
		p->i = (BeamInstr*)Arg(1);
		Goto(p);
	OpCase(JUMP):
		debug_op2(p,"jump\n");
		p->i = (BeamInstr*)(Arg(0));
		Goto(p);
	OpCase(CATCH):
		debug_op2(p,"catch\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(CATCH_END):
		debug_op2(p,"catch_end\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(MOVE):
		debug_op2(p,"move\n");
		Resolve(Arg(0), tmp1);
		Move(tmp1, Arg(1));
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(GET_LIST):
		debug_op2(p,"get_list\n");
		Resolve(Arg(0), tmp0);
		tmp_ptr = list_val(tmp0);
		Move(CAR(tmp_ptr), Arg(1));
		Move(CDR(tmp_ptr), Arg(2));
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(GET_TUPLE_ELEMENT):
		debug_op2(p,"get_tuple_element\n");
		Resolve(Arg(0), tmp0);
		Resolve(Arg(1), tmp1);
		tmp_ptr = tuple_val(tmp0) + unsigned_val(tmp1) + 1;
		Move(*tmp_ptr, Arg(2));
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(SET_TUPLE_ELEMENT):
		debug_op2(p,"set_tuple_element\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(PUT_STRING):
		debug_op2(p,"put_string\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(PUT_LIST):
		debug_op2(p,"put_list\n");
		Resolve(Arg(0), HTOP[0]);
		Resolve(Arg(1), HTOP[1]);
		Move(make_list(HTOP), Arg(2));
		HTOP += 2;
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(PUT_TUPLE):
		debug_op2(p,"put_tuple\n");
		tuple = make_tuple(HTOP);
		Resolve(Arg(0), arity);
		*HTOP++ = make_arityval(unsigned_val(arity));
		Move(tuple, Arg(1));
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(PUT):
		debug_op2(p,"put\n");
		Resolve(Arg(0), term);
		*HTOP++ = term;
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(BADMATCH):
		debug("badmatch\n");
		debug_term(r(0));
		debug("\n");
		erts_do_exit_process(p, atom_normal);
	OpCase(IF_END):
		debug_op2(p,"if_end\n");
		RESTORE_I(p, p->i, 1);
		Goto(p);
	OpCase(CASE_END):
		debug_op2(p,"case_end\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(CALL_FUN):
		debug_op2(p,"call_fun\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(MAKE_FUN):
		debug_op2(p,"make_fun\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(IS_FUNCTION):
		debug_op2(p,"is_function\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(CALL_EXT_ONLY):
		debug_op2(p,"call_ext_only\n");
		yield_maybe(p, (uint8_t)unsigned_val(Arg(0)));
		e = (Export*)(Arg(1));
		if(e->bif != NULL) {
			SWAPOUT;
			reg[0] = r(0);
			r(0) = (e->bif)(p, reg, p->arity);
			if(p->saved_i != (BeamInstr*)(beam_apply+2)) {
				p->i = p->cp;
			}
			SWAPIN;
			Goto(p);
		}
		p->i = e->address;
		Goto(p);
	OpCase(BS_START_MATCH):
		debug_op2(p,"bs_start_match\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_GET_INTEGER):
		debug_op2(p,"bs_get_integer\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_GET_FLOAT):
		debug_op2(p,"bs_get_float\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_GET_BINARY):
		debug_op2(p,"bs_get_binary\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_SKIP_BITS):
		debug_op2(p,"bs_skip_bits\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(BS_TEST_TAIL):
		debug_op2(p,"bs_test_tail\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_SAVE):
		debug_op2(p,"bs_save\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(BS_RESTORE):
		debug_op2(p,"bs_restore\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(BS_INIT):
		debug_op2(p,"bs_init\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_FINAL):
		debug_op2(p,"bs_final\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_PUT_INTEGER):
		debug_op2(p,"bs_put_integer\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_PUT_BINARY):
		debug_op2(p,"bs_put_binary\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_PUT_FLOAT):
		debug_op2(p,"bs_put_float\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_PUT_STRING):
		debug_op2(p,"bs_put_string\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_NEED_BUF):
		debug_op2(p,"bs_need_buf\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(FCLEARERROR):
		debug_op2(p,"fclearerror\n");
		RESTORE_I(p, p->i, 1);
		Goto(p);
	OpCase(FCHECKERROR):
		debug_op2(p,"fcheckerror\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(FMOVE):
		debug_op2(p,"fmove\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(FCONV):
		debug_op2(p,"fconv\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(FADD):
		debug_op2(p,"fadd\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(FSUB):
		debug_op2(p,"fsub\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(FMUL):
		debug_op2(p,"fmul\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(FDIV):
		debug_op2(p,"fdiv\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(FNEGATE):
		debug_op2(p,"fnegate\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(MAKE_FUN2):
		debug_op2(p,"make_fun2\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(TRY):
		debug_op2(p,"try\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(TRY_END):
		debug_op2(p,"try_end\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(TRY_CASE):
		debug_op2(p,"try_case\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(TRY_CASE_END):
		debug_op2(p,"try_case_end\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(RAISE):
		debug_op2(p,"raise\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_INIT2):
		debug_op2(p,"bs_init2\n");
		RESTORE_I(p, p->i, 7);
		Goto(p);
	OpCase(BS_BITS_TO_BYTES):
		debug_op2(p,"bs_bits_to_bytes\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(BS_ADD):
		debug_op2(p,"bs_add\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(APPLY):
		debug_op2(p,"apply\n");
		yield_maybe(p, unsigned_val(Arg(0))+2);
		if(p->arity == 2) {
			tmp.module = r(0);
			tmp.function = x(1);
		}
		else {
			tmp.module = x(p->arity-2);
			tmp.function = x(p->arity-1);
		}
		tmp.arity = p->arity-2;
		e = erts_export_get(&tmp);
		RESTORE_I(p, p->cp, 2);
		if(e->bif != NULL) {
			SWAPOUT;
			reg[0] = r(0);
			r(0) = (e->bif)(p, reg, p->arity);
			if(p->saved_i != (BeamInstr*)(beam_apply+2)) {
				p->i = p->cp;
			}
			SWAPIN;
			Goto(p);
		}
		p->i = e->address;
		Goto(p);
	OpCase(APPLY_LAST):
		debug_op2(p,"apply_last\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_BOOLEAN):
		debug_op2(p,"is_boolean\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_FUNCTION2):
		debug_op2(p,"is_function2\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(BS_START_MATCH2):
		debug_op2(p,"bs_start_match2\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_GET_INTEGER2):
		debug_op2(p,"bs_get_integer2\n");
		RESTORE_I(p, p->i, 8);
		Goto(p);
	OpCase(BS_GET_FLOAT2):
		debug_op2(p,"bs_get_float2\n");
		RESTORE_I(p, p->i, 8);
		Goto(p);
	OpCase(BS_GET_BINARY2):
		debug_op2(p,"bs_get_binary2\n");
		RESTORE_I(p, p->i, 8);
		Goto(p);
	OpCase(BS_SKIP_BITS2):
		debug_op2(p,"bs_skip_bits2\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_TEST_TAIL2):
		debug_op2(p,"bs_test_tail2\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(BS_SAVE2):
		debug_op2(p,"bs_save2\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_RESTORE2):
		debug_op2(p,"bs_restore2\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(GC_BIF1):
		debug_op2(p,"gc_bif1\n");
		e = (Export*)Arg(2);
		Live = unsigned_val(Arg(1));
		reg[0] = r(0);
		Resolve(Arg(3), reg[Live]);
		SWAPOUT;
		result = (e->bif)(p, reg, Live);
		SWAPIN;
		r(0) = reg[0];
		Move(result, Arg(4));
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(GC_BIF2):
		debug_op2(p,"gc_bif2\n");
		e = (Export*)Arg(2);
		Live = unsigned_val(Arg(1));
		reg[0] = r(0);
		Resolve(Arg(3), reg[Live]);
		Resolve(Arg(4), reg[Live+1]);
		SWAPOUT;
		result = (e->bif)(p, reg, Live);
		SWAPIN;
		r(0) = reg[0];
		Move(result, Arg(5));
		RESTORE_I(p, p->i, 7);
		Goto(p);
	OpCase(BS_FINAL2):
		debug_op2(p,"bs_final2\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_BITS_TO_BYTES2):
		debug_op2(p,"bs_bits_to_bytes2\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(PUT_LITERAL):
		debug_op2(p,"put_literal\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(IS_BITSTR):
		debug_op2(p,"is_bitstr\n");
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_CONTEXT_TO_BINARY):
		debug_op2(p,"bs_context_to_binary\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(BS_TEST_UNIT):
		debug_op2(p,"bs_test_unit\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(BS_MATCH_STRING):
		debug_op2(p,"bs_match_string\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(BS_INIT_WRITABLE):
		debug_op2(p,"bs_init_writable\n");
		RESTORE_I(p, p->i, 1);
		Goto(p);
	OpCase(BS_APPEND):
		debug_op2(p,"bs_append\n");
		RESTORE_I(p, p->i, 9);
		Goto(p);
	OpCase(BS_PRIVATE_APPEND):
		debug_op2(p,"bs_private_append\n");
		RESTORE_I(p, p->i, 7);
		Goto(p);
	OpCase(TRIM):
		debug_op2(p,"trim\n");
		tmp0 = E[0];
		E += unsigned_val(Arg(0));
		E[0] = tmp0;
		RESTORE_I(p, p->i, 3);
		Goto(p);
	OpCase(BS_INIT_BITS):
		debug_op2(p,"bs_init_bits\n");
		RESTORE_I(p, p->i, 7);
		Goto(p);
	OpCase(BS_GET_UTF8):
		debug_op2(p,"bs_get_utf8\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_SKIP_UTF8):
		debug_op2(p,"bs_skip_utf8\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(BS_GET_UTF16):
		debug_op2(p,"bs_get_utf16\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_SKIP_UTF16):
		debug_op2(p,"bs_skip_utf16\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(BS_GET_UTF32):
		debug_op2(p,"bs_get_utf32\n");
		RESTORE_I(p, p->i, 6);
		Goto(p);
	OpCase(BS_SKIP_UTF32):
		debug_op2(p,"bs_skip_utf32\n");
		RESTORE_I(p, p->i, 5);
		Goto(p);
	OpCase(BS_UTF8_SIZE):
		debug_op2(p,"bs_utf8_size\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(BS_PUT_UTF8):
		debug_op2(p,"bs_put_utf8\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(BS_UTF16_SIZE):
		debug_op2(p,"bs_utf16_size\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(BS_PUT_UTF16):
		debug_op2(p,"bs_put_utf16\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(BS_PUT_UTF32):
		debug_op2(p,"bs_put_utf32\n");
		RESTORE_I(p, p->i, 4);
		Goto(p);
	OpCase(ON_LOAD):
		debug_op2(p,"on_load\n");
		RESTORE_I(p, p->i, 1);
		Goto(p);
	OpCase(RECV_MARK):
		debug_op2(p,"recv_mark\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(RECV_SET):
		debug_op2(p,"recv_set\n");
		RESTORE_I(p, p->i, 2);
		Goto(p);
	OpCase(GC_BIF3):
		debug_op2(p,"gc_bif3\n");
		RESTORE_I(p, p->i, 8);
		Goto(p);
	OpCase(LINE):
		//ignore for now
		RESTORE_I(p, p->i, 2);
		Goto(p);
	//special vm ops
	OpCase(BEAM_APPLY):
		debug_op2(p, "beam_apply\n");
		restore_registers(p);
		next = apply(p, r(0), x(1), x(2));
		p->i = next;
		Goto(p);
	OpCase(NORMAL_EXIT):
		//@todo do a lot of stuff when exiting a process
		sprintf(buf, "Pid %d exited, result: ", pid2pix(p->id));
		//debug(buf);
		//debug_term(x0);
		//debug("\n");
		sprintf(buf, "Reclaimed: %u words\n", reclaimed); debug(buf);
		sprintf(buf, "Garbage cols: %u\n", garbage_cols); debug(buf);
		//sprintf(buf, "Reductions: %u\n", p->reductions); debug_op(buf);
		//sprintf(buf, "Context switches %u\n", p->context_switches); debug_op(buf);
		//sprintf(buf, "Ticks: %u\n", ticks - p->started_at); debug_op(buf);
		//sprintf(buf, "GC ticks: %u\n", p->gc_ticks); debug_op(buf);
		sprintf(buf, "Heap size: %u words\n", p->heap_sz); debug(buf);
		erts_do_exit_process(p, atom_normal);
	OpCase(NOP):
		SWAPOUT;
		taskYIELD();
		SWAPIN;
		Goto(p);

	LOOP_END

	p->i = (BeamInstr*)(beam_apply+2);
	Goto(p);
}

void restore_registers(ErlProcess* p) {
	//restore
	for(i=0; i<p->arity; i++) {
		if(i == 0) {
			r(0) = p->arg_reg[0];
		}
		else {
			x(i) = p->arg_reg[i];
		}
	}
}

BeamInstr* apply(ErlProcess* p, Eterm module, Eterm function, Eterm args) {
	int arity = 0;
	Eterm tmp = args;

	while(is_list(tmp)) {
		if(arity == 0) {
			r(0) = CAR(list_val(tmp));
		}
		else {
			x(arity) = CAR(list_val(tmp));
		}
		arity++;
		tmp = CDR(list_val(tmp));
	}

	Export e;
	e.module = module;
	e.function = function;
	e.arity = arity;

	Export *export = erts_export_get(&e);
	return export->address;
}

void test_heap(ErlProcess* p, UInt need, UInt live) {
	if (E - HTOP < need) {
		SWAPOUT;
		reg[0] = r(0);
		int fcalls = erts_garbage_collect(p, need, reg, live);
		p->fcalls -= fcalls;
		r(0) = reg[0];
		SWAPIN;
	}
}

void allocate_heap(ErlProcess* p, UInt stack_need, UInt heap_need, UInt live) {
	int needed = stack_need+1;
	if(E - HTOP < (needed + heap_need)) {
		SWAPOUT;
		reg[0] = r(0);
		int fcalls = erts_garbage_collect(p, needed+heap_need, reg, live);
		p->fcalls -= fcalls;
		r(0) = reg[0];
		SWAPIN;
	}
	E -= needed;
	*E = (Eterm)(p->cp);

	p->cp = 0;
}

Eterm* erts_heap_alloc(ErlProcess* p, UInt need, UInt xtra, UInt live) {
	SWAPOUT;
	reg[0] = r(0);
	int fcalls = erts_garbage_collect(p, need+xtra, reg, live);
	p->fcalls -= fcalls;
	r(0) = reg[0];
	SWAPIN;

	Eterm *hp = p->htop;
	p->htop += need;

	return hp;
}

static void timeout_proc(ErlProcess *p) {
	BeamInstr** pi = (BeamInstr**)p->def_arg_reg;
	p->i = *pi;
	p->flags |= F_TIMO;
	p->flags &= ~F_INSLPQUEUE;
	resumed++;
	xTaskResumeFromISR(*(p->handle));
}

static inline void cancel_timer(ErlProcess *p) {
	if(p->flags & F_INSLPQUEUE) {
		p->flags &= ~(F_INSLPQUEUE|F_TIMO);
		erts_cancel_timer(&p->timer);
	}
	else {
		p->flags &= ~F_TIMO;
	}
	resumed++;
	vTaskResume(*(p->handle));
}

static inline void set_timer(ErlProcess* p, UInt timeout) {
	if(timeout == 0) {
		p->flags |= F_TIMO;
		return;
	}
	p->flags |= F_INSLPQUEUE;
	p->flags &= ~F_TIMO;
	erts_set_timer(&p->timer, (ErlTimeoutProc)timeout_proc, NULL, (void*)p, timeout);
}
