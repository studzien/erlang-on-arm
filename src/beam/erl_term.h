/*
 * erl_term.h
 *
 *  Created on: Nov 2, 2013
 *      Author: Studnicki
 */

#ifndef ERL_TERM_H_
#define ERL_TERM_H_

#include "FreeRTOS.h"
#include "config.h"

// tagged Erlang term (32 bits)
typedef uint32_t Eterm;
typedef uint8_t byte;
typedef int32_t SInt;
typedef uint32_t UInt;

#define _TAG_PRIMARY_SIZE   2
#define _TAG_PRIMARY_MASK   0x3
#define TAG_PRIMARY_HEADER  0x0
#define TAG_PRIMARY_LIST    0x1
#define TAG_PRIMARY_BOXED   0x2
#define TAG_PRIMARY_IMMED1  0x3

#define primary_tag(x)  ((x) & _TAG_PRIMARY_MASK)

#define _TAG_IMMED1_SIZE    4
#define _TAG_IMMED1_MASK    0xF
#define _TAG_IMMED1_PID     ((0x0 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_PORT    ((0x1 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_IMMED2  ((0x2 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_SMALL   ((0x3 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)

#define _TAG_IMMED2_SIZE    6
#define _TAG_IMMED2_MASK    0x3F
#define _TAG_IMMED2_ATOM    ((0x0 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_CATCH   ((0x1 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_NIL     ((0x3 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)

#define make_atom(x) ((Eterm)(((x) << _TAG_IMMED2_SIZE) + _TAG_IMMED2_ATOM))

#define make_small(x) ((Eterm)(((x) << _TAG_IMMED1_SIZE) + _TAG_IMMED1_SMALL))
#define unsigned_val(x) ((x) >> _TAG_IMMED1_SIZE)
#define signed_val(x) ((SInt)(x) >> _TAG_IMMED1_SIZE)
#define is_small(x) (((x) & _TAG_IMMED1_MASK) == _TAG_IMMED1_SMALL)

#define MY_IS_SSMALL(x) (((UInt) ((((x)) >> (SMALL_BITS-1)) + 1)) < 2)

#define pix2pid(x) ((Eterm)(((x) << _TAG_IMMED1_SIZE) + _TAG_IMMED1_PID))
#define pid2pix(x) ((x) >> _TAG_IMMED1_SIZE)

#define X_REG_DEF 0
#define Y_REG_DEF 1
#define R_REG_DEF 2

#define make_rreg() R_REG_DEF
#define make_xreg(ix) (((ix) << 4) | X_REG_DEF)
#define make_yreg(ix) (((ix) << 4) | Y_REG_DEF)

#define is_rreg(x) (((x) & 0xf) == R_REG_DEF)
#define is_yreg(x) (((x) & 0xf) == Y_REG_DEF)
#define is_xreg(x) (((x) & 0xf) == X_REG_DEF)
#define x_reg_index(x) ((x) >> 4)
#define y_reg_index(x) ((x) >> 4)

#define CONS(hp, car, cdr) \
	(CAR(hp)=(car), CDR(hp)=(cdr), make_list(hp))

#define CAR(x) ((x)[0])
#define CDR(x) ((x)[1])

#define make_list(x) ((Eterm)(x) + TAG_PRIMARY_LIST)
#define list_val(x) ((Eterm*)((x) - TAG_PRIMARY_LIST))
#define is_list(x)      (((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_LIST)

#define NIL (((Eterm)(0) << _TAG_IMMED2_SIZE) | _TAG_IMMED2_NIL)

#define is_immed(x) (((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_IMMED1)
#define IS_CONST(x) is_immed((x))

#define THE_NON_VALUE (Eterm)(0)

#define ARITYVAL_SUBTAG     (0x0 << _TAG_PRIMARY_SIZE) /* TUPLE */
#define BIN_MATCHSTATE_SUBTAG   (0x1 << _TAG_PRIMARY_SIZE)
#define POS_BIG_SUBTAG      (0x2 << _TAG_PRIMARY_SIZE) /* BIG: tags 2&3 */
#define NEG_BIG_SUBTAG      (0x3 << _TAG_PRIMARY_SIZE) /* BIG: tags 2&3 */
#define _BIG_SIGN_BIT       (0x1 << _TAG_PRIMARY_SIZE)
#define REF_SUBTAG      (0x4 << _TAG_PRIMARY_SIZE) /* REF */
#define FUN_SUBTAG      (0x5 << _TAG_PRIMARY_SIZE) /* FUN */
#define FLOAT_SUBTAG        (0x6 << _TAG_PRIMARY_SIZE) /* FLOAT */
#define EXPORT_SUBTAG   (0x7 << _TAG_PRIMARY_SIZE) /* FLOAT */
#define _BINARY_XXX_MASK    (0x3 << _TAG_PRIMARY_SIZE)
#define REFC_BINARY_SUBTAG  (0x8 << _TAG_PRIMARY_SIZE) /* BINARY */
#define HEAP_BINARY_SUBTAG  (0x9 << _TAG_PRIMARY_SIZE) /* BINARY */
#define SUB_BINARY_SUBTAG   (0xA << _TAG_PRIMARY_SIZE) /* BINARY */
#define EXTERNAL_PID_SUBTAG (0xC << _TAG_PRIMARY_SIZE) /* EXTERNAL_PID */
#define EXTERNAL_PORT_SUBTAG    (0xD << _TAG_PRIMARY_SIZE) /* EXTERNAL_PORT */
#define EXTERNAL_REF_SUBTAG (0xE << _TAG_PRIMARY_SIZE) /* EXTERNAL_REF */

#define _TAG_HEADER_ARITYVAL    (TAG_PRIMARY_HEADER|ARITYVAL_SUBTAG)
#define _TAG_HEADER_FUN     (TAG_PRIMARY_HEADER|FUN_SUBTAG)
#define _TAG_HEADER_POS_BIG (TAG_PRIMARY_HEADER|POS_BIG_SUBTAG)
#define _TAG_HEADER_NEG_BIG (TAG_PRIMARY_HEADER|NEG_BIG_SUBTAG)
#define _TAG_HEADER_FLOAT   (TAG_PRIMARY_HEADER|FLOAT_SUBTAG)
#define _TAG_HEADER_EXPORT  (TAG_PRIMARY_HEADER|EXPORT_SUBTAG)
#define _TAG_HEADER_REF     (TAG_PRIMARY_HEADER|REF_SUBTAG)
#define _TAG_HEADER_REFC_BIN    (TAG_PRIMARY_HEADER|REFC_BINARY_SUBTAG)
#define _TAG_HEADER_HEAP_BIN    (TAG_PRIMARY_HEADER|HEAP_BINARY_SUBTAG)
#define _TAG_HEADER_SUB_BIN (TAG_PRIMARY_HEADER|SUB_BINARY_SUBTAG)
#define _TAG_HEADER_EXTERNAL_PID  (TAG_PRIMARY_HEADER|EXTERNAL_PID_SUBTAG)
#define _TAG_HEADER_EXTERNAL_PORT (TAG_PRIMARY_HEADER|EXTERNAL_PORT_SUBTAG)
#define _TAG_HEADER_EXTERNAL_REF  (TAG_PRIMARY_HEADER|EXTERNAL_REF_SUBTAG)
#define _TAG_HEADER_BIN_MATCHSTATE (TAG_PRIMARY_HEADER|BIN_MATCHSTATE_SUBTAG)

#define _TAG_HEADER_MASK    0x3F
#define _HEADER_SUBTAG_MASK 0x3C    /* 4 bits for subtag */
#define _HEADER_ARITY_OFFS  6

/* header (arityval or thing) access methods */
#define _make_header(sz,tag)  ((UInt)(((sz) << _HEADER_ARITY_OFFS) + (tag)))
#define is_header(x)    (((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_HEADER)
#define make_boxed(x) ((UInt) (x) + TAG_PRIMARY_BOXED)
#define boxed_val(x) ((Eterm*) ((x) - TAG_PRIMARY_BOXED))
#define header_arity(x) ((x) >> _HEADER_ARITY_OFFS)

/* bignum access methods */
#define make_pos_bignum_header(sz)  _make_header((sz),_TAG_HEADER_POS_BIG)
#define make_neg_bignum_header(sz)  _make_header((sz),_TAG_HEADER_NEG_BIG)
#define make_big(x) make_boxed((x))
#define big_val(x) boxed_val((x))
#endif /* ERL_TERM_H_ */
