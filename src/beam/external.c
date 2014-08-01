/*
 * external.c
 *
 *  Created on: Jul 17, 2014
 *      Author: Studnicki
 */

#include "external.h"
#include "sys.h"

static SInt decoded_size(byte *ep, byte* endp, int internal_tags);
static byte* dec_term(Eterm** hpp, byte* ep, Eterm* objp);

SInt erts_decode_ext_size(byte *ext, UInt size)
{
    if (size == 0 || *ext != VERSION_MAGIC)
    	return -1;
    return decoded_size(ext+1, ext+size, 0);
}

Eterm erts_decode_ext(Eterm **hpp, byte **ext) {
    Eterm obj;
    byte *ep = *ext;
    if (*ep++ != VERSION_MAGIC)
    	return THE_NON_VALUE;
    ep = dec_term(hpp, ep, &obj);
    if (!ep) {
    	return THE_NON_VALUE;
    }
    *ext = ep;
    return obj;
}

static byte* dec_term(Eterm** hpp, byte* ep, Eterm* objp) {
	Eterm* hp_saved = *hpp;
	int n;

	register Eterm* hp = *hpp;	/* Please don't take the address of hp */
	Eterm* next = objp;

	*next = (Eterm) NULL;

	while (next != NULL) {
		objp = next;
		next = (Eterm *) (*objp);

		switch (*ep++) {
		case INTEGER_EXT:
		{
			SInt sn = get_int32(ep);

			ep += 4;
			if (MY_IS_SSMALL(sn)) {
				*objp = make_small(sn);
			} else {
				*objp = small_to_big(sn, hp);
				hp += BIG_UINT_HEAP_SIZE;
			}
			break;
		}
		case SMALL_INTEGER_EXT:
			n = get_int8(ep);
			ep++;
			*objp = make_small(n);
			break;
		case SMALL_BIG_EXT:
			n = get_int8(ep);
			ep++;
			goto big_loop;
		case LARGE_BIG_EXT:
			n = get_int32(ep);
			ep += 4;
			big_loop:
			{
				Eterm big;
				byte* first;
				byte* last;
				UInt neg;

				neg = get_int8(ep); /* Sign bit */
				ep++;

				/*
				 * Strip away leading zeroes to avoid creating illegal bignums.
				 */
				first = ep;
				last = ep + n;
				ep += n;
				do {
					--last;
				} while (first <= last && *last == 0);

				if ((n = last - first + 1) == 0) {
					/* Zero width bignum defaults to zero */
					big = make_small(0);
				} else {
					big = bytes_to_big(first, n, neg, hp);
					if (is_big(big)) {
						hp += big_arity(big) + 1;
					}
				}
				*objp = big;
				break;
			}
		case ATOM_EXT:
			n = get_int16(ep);
			ep += 2;
			//char_enc = ERTS_ATOM_ENC_LATIN1;
			goto dec_term_atom_common;
		case SMALL_ATOM_EXT:
			n = get_int8(ep);
			ep++;
			//char_enc = ERTS_ATOM_ENC_LATIN1;
			goto dec_term_atom_common;
		case ATOM_UTF8_EXT:
			n = get_int16(ep);
			ep += 2;
			//char_enc = ERTS_ATOM_ENC_UTF8;
			goto dec_term_atom_common;
		case SMALL_ATOM_UTF8_EXT:
			n = get_int8(ep);
			ep++;
			//char_enc = ERTS_ATOM_ENC_UTF8;
			dec_term_atom_common:
			/*if (edep && (edep->flags & ERTS_DIST_EXT_BTT_SAFE)) {
				if (!erts_atom_get((char*)ep, n, objp, char_enc)) {
					goto error;
				}
			}
			else*/
			{
				Eterm atom = erts_atom_put(ep, n);
				if (is_non_value(atom))
					goto error;
				*objp = atom;
			}
			ep += n;
			break;
		case LARGE_TUPLE_EXT:
			n = get_int32(ep);
			ep += 4;
			goto tuple_loop;
		case SMALL_TUPLE_EXT:
			n = get_int8(ep);
			ep++;
			tuple_loop:
			*objp = make_tuple(hp);
			*hp++ = make_arityval(n);
			hp += n;
			objp = hp - 1;
			while (n-- > 0) {
				objp[0] = (Eterm) next;
				next = objp;
				objp--;
			}
			break;
		case NIL_EXT:
			*objp = NIL;
			break;
		case LIST_EXT:
			n = get_int32(ep);
			ep += 4;
			if (n == 0) {
				next = objp;
				break;
			}
			*objp = make_list(hp);
			hp += 2*n;
			objp = hp - 2;
			objp[0] = (Eterm) (objp+1);
			objp[1] = (Eterm) (next);
			next = objp;
			objp -= 2;
			while (--n > 0) {
				objp[0] = (Eterm) (next);
				objp[1] = make_list(objp + 2);
				next = objp;
				objp -= 2;
			}
			break;
		case STRING_EXT:
			n = get_int16(ep);
			ep += 2;
			if (n == 0) {
				*objp = NIL;
				break;
			}
			*objp = make_list(hp);
			while (n-- > 0) {
				hp[0] = make_small(*ep++);
				hp[1] = make_list(hp+2);
				hp += 2;
			}
			hp[-1] = NIL;
			break;
			/*case FLOAT_EXT:
	    {
		FloatDef ff;

		if (sys_chars_to_double((char*)ep, &ff.fd) != 0) {
		    goto error;
		}
		ep += 31;
			 *objp = make_float(hp);
		PUT_DOUBLE(ff, hp);
		hp += FLOAT_SIZE_OBJECT;
		break;
	    }
	case NEW_FLOAT_EXT:
	    {
		FloatDef ff;

#if defined(WORDS_BIGENDIAN) || defined(DOUBLE_MIDDLE_ENDIAN)
		ff.fw[0] = get_int32(ep);
		ep += 4;
		ff.fw[1] = get_int32(ep);
		ep += 4;
#else
		ff.fw[1] = get_int32(ep);
		ep += 4;
		ff.fw[0] = get_int32(ep);
		ep += 4;
#endif
		__ERTS_FP_CHECK_INIT(fpexnp);
		__ERTS_FP_ERROR_THOROUGH(fpexnp, ff.fd, goto error);
			 *objp = make_float(hp);
		PUT_DOUBLE(ff, hp);
		hp += FLOAT_SIZE_OBJECT;
		break;
	    }
	case PID_EXT:
			 *hpp = hp;
	    ep = dec_pid(edep, hpp, ep, off_heap, objp);
	    hp = *hpp;
	    if (ep == NULL) {
		goto error;
	    }
	    break;
	case PORT_EXT:
	    {
		Eterm sysname;
		ErlNode *node;
		Uint num;
		Uint cre;

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL) {
		    goto error;
		}
		if ((num = get_int32(ep)) > ERTS_MAX_PORT_NUMBER) {
		    goto error;
		}
		ep += 4;
		cre = get_int8(ep);
		ep++;
		if (!is_valid_creation(cre)) {
		    goto error;
		}

		node = dec_get_node(sysname, cre);
		if(node == erts_this_node) {
			 *objp = make_internal_port(num);
		}
		else {
		    ExternalThing *etp = (ExternalThing *) hp;
		    hp += EXTERNAL_THING_HEAD_SIZE + 1;

		    etp->header = make_external_port_header(1);
		    etp->next = off_heap->first;
		    etp->node = node;
		    etp->data.ui[0] = num;

		    off_heap->first = (struct erl_off_heap_header*)etp;
			 *objp = make_external_port(etp);
		}

		break;
	    }
	case REFERENCE_EXT:
	    {
		Eterm sysname;
		ErlNode *node;
		int i;
		Uint cre;
		Uint32 *ref_num;
		Uint32 r0;
		Uint ref_words;

		ref_words = 1;

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL)
		    goto error;
		if ((r0 = get_int32(ep)) >= MAX_REFERENCE )
		    goto error;
		ep += 4;

		cre = get_int8(ep);
		ep += 1;
		if (!is_valid_creation(cre)) {
		    goto error;
		}
		goto ref_ext_common;

	    case NEW_REFERENCE_EXT:
		ref_words = get_int16(ep);
		ep += 2;

		if (ref_words > ERTS_MAX_REF_NUMBERS)
		    goto error;

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL)
		    goto error;

		cre = get_int8(ep);
		ep += 1;
		if (!is_valid_creation(cre)) {
		    goto error;
		}
		r0 = get_int32(ep);
		ep += 4;
		if (r0 >= MAX_REFERENCE)
		    goto error;

	    ref_ext_common:

		node = dec_get_node(sysname, cre);
		if(node == erts_this_node) {
		    RefThing *rtp = (RefThing *) hp;
		    ref_num = (Uint32 *) (hp + REF_THING_HEAD_SIZE);

#if defined(ARCH_64) && !HALFWORD_HEAP
		    hp += REF_THING_HEAD_SIZE + ref_words/2 + 1;
		    rtp->header = make_ref_thing_header(ref_words/2 + 1);
#else
		    hp += REF_THING_HEAD_SIZE + ref_words;
		    rtp->header = make_ref_thing_header(ref_words);
#endif
			 *objp = make_internal_ref(rtp);
		}
		else {
		    ExternalThing *etp = (ExternalThing *) hp;
#if defined(ARCH_64) && !HALFWORD_HEAP
		    hp += EXTERNAL_THING_HEAD_SIZE + ref_words/2 + 1;
#else
		    hp += EXTERNAL_THING_HEAD_SIZE + ref_words;
#endif

#if defined(ARCH_64) && !HALFWORD_HEAP
		    etp->header = make_external_ref_header(ref_words/2 + 1);
#else
		    etp->header = make_external_ref_header(ref_words);
#endif
		    etp->next = off_heap->first;
		    etp->node = node;

		    off_heap->first = (struct erl_off_heap_header*)etp;
			 *objp = make_external_ref(etp);
		    ref_num = &(etp->data.ui32[0]);
		}

#if defined(ARCH_64) && !HALFWORD_HEAP
			 *(ref_num++) = ref_words // 32-bit arity ;
#endif
		ref_num[0] = r0;
		for(i = 1; i < ref_words; i++) {
		    ref_num[i] = get_int32(ep);
		    ep += 4;
		}
#if defined(ARCH_64) && !HALFWORD_HEAP
		if ((1 + ref_words) % 2)
		    ref_num[ref_words] = 0;
#endif
		break;
	    }
	case BINARY_EXT:
	    {
		n = get_int32(ep);
		ep += 4;

		if ((unsigned)n <= ERL_ONHEAP_BIN_LIMIT) {
		    ErlHeapBin* hb = (ErlHeapBin *) hp;

		    hb->thing_word = header_heap_bin(n);
		    hb->size = n;
		    hp += heap_bin_size(n);
		    sys_memcpy(hb->data, ep, n);
			 *objp = make_binary(hb);
		} else {
		    Binary* dbin = erts_bin_nrml_alloc(n);
		    ProcBin* pb;
		    dbin->flags = 0;
		    dbin->orig_size = n;
		    erts_refc_init(&dbin->refc, 1);
		    sys_memcpy(dbin->orig_bytes, ep, n);
		    pb = (ProcBin *) hp;
		    hp += PROC_BIN_SIZE;
		    pb->thing_word = HEADER_PROC_BIN;
		    pb->size = n;
		    pb->next = off_heap->first;
		    off_heap->first = (struct erl_off_heap_header*)pb;
		    pb->val = dbin;
		    pb->bytes = (byte*) dbin->orig_bytes;
		    pb->flags = 0;
			 *objp = make_binary(pb);
		}
		ep += n;
		break;
	    }
	case BIT_BINARY_EXT:
	    {
		Eterm bin;
		ErlSubBin* sb;
		Uint bitsize;

		n = get_int32(ep);
		bitsize = ep[4];
                if (((bitsize==0) != (n==0)) || bitsize > 8)
                    goto error;
                ep += 5;
		if ((unsigned)n <= ERL_ONHEAP_BIN_LIMIT) {
		    ErlHeapBin* hb = (ErlHeapBin *) hp;

		    hb->thing_word = header_heap_bin(n);
		    hb->size = n;
		    sys_memcpy(hb->data, ep, n);
		    bin = make_binary(hb);
		    hp += heap_bin_size(n);
		} else {
		    Binary* dbin = erts_bin_nrml_alloc(n);
		    ProcBin* pb;
		    dbin->flags = 0;
		    dbin->orig_size = n;
		    erts_refc_init(&dbin->refc, 1);
		    sys_memcpy(dbin->orig_bytes, ep, n);
		    pb = (ProcBin *) hp;
		    pb->thing_word = HEADER_PROC_BIN;
		    pb->size = n;
		    pb->next = off_heap->first;
		    off_heap->first = (struct erl_off_heap_header*)pb;
		    pb->val = dbin;
		    pb->bytes = (byte*) dbin->orig_bytes;
		    pb->flags = 0;
		    bin = make_binary(pb);
		    hp += PROC_BIN_SIZE;
		}
		ep += n;
		if (bitsize == 8 || n == 0) {
			 *objp = bin;
		} else {
                    sb = (ErlSubBin *)hp;
		    sb->thing_word = HEADER_SUB_BIN;
		    sb->orig = bin;
		    sb->size = n - 1;
		    sb->bitsize = bitsize;
		    sb->bitoffs = 0;
		    sb->offs = 0;
		    sb->is_writable = 0;
			 *objp = make_binary(sb);
		    hp += ERL_SUB_BIN_SIZE;
		}
		break;
	    }
	case EXPORT_EXT:
	    {
		Eterm mod;
		Eterm name;
		Eterm temp;
		Sint arity;

		if ((ep = dec_atom(edep, ep, &mod)) == NULL) {
		    goto error;
		}
		if ((ep = dec_atom(edep, ep, &name)) == NULL) {
		    goto error;
		}
			 *hpp = hp;
		ep = dec_term(edep, hpp, ep, off_heap, &temp);
		hp = *hpp;
		if (ep == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		arity = signed_val(temp);
		if (arity < 0) {
		    goto error;
		}
		if (edep && (edep->flags & ERTS_DIST_EXT_BTT_SAFE)) {
		    if (!erts_active_export_entry(mod, name, arity))
			goto error;
                }
			 *objp = make_export(hp);
			 *hp++ = HEADER_EXPORT;
#if HALFWORD_HEAP
			 *((UWord *) (UWord) hp) =  (UWord) erts_export_get_or_make_stub(mod, name, arity);
		hp += 2;
#else
			 *hp++ = (Eterm) erts_export_get_or_make_stub(mod, name, arity);
#endif
		break;
	    }
	    break;
	case NEW_FUN_EXT:
	    {
		ErlFunThing* funp = (ErlFunThing *) hp;
		Uint arity;
		Eterm module;
		byte* uniq;
		int index;
		Sint old_uniq;
		Sint old_index;
		unsigned num_free;
		int i;
		Eterm temp;

		ep += 4;	// Skip total size in bytes
		arity = *ep++;
		uniq = ep;
		ep += 16;
		index = get_int32(ep);
		ep += 4;
		num_free = get_int32(ep);
		ep += 4;
		hp += ERL_FUN_SIZE;
		hp += num_free;
		funp->thing_word = HEADER_FUN;
		funp->num_free = num_free;
			 *objp = make_fun(funp);

		// Module
		if ((ep = dec_atom(edep, ep, &module)) == NULL) {
		    goto error;
		}
			 *hpp = hp;
		// Index
		if ((ep = dec_term(edep, hpp, ep, off_heap, &temp)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_index = unsigned_val(temp);

		// Uniq
		if ((ep = dec_term(edep, hpp, ep, off_heap, &temp)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_uniq = unsigned_val(temp);

		//
		// It is safe to link the fun into the fun list only when
		// no more validity tests can fail.
		///
		funp->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*)funp;

		funp->fe = erts_put_fun_entry2(module, old_uniq, old_index,
					       uniq, index, arity);
		funp->arity = arity;

		hp = *hpp;

		// Environment
		for (i = num_free-1; i >= 0; i--) {
		    funp->env[i] = (Eterm) COMPRESS_POINTER(next);
		    next = funp->env + i;
		}
		// Creator
		funp->creator = (Eterm) COMPRESS_POINTER(next);
		next = &(funp->creator);
		break;
	    }
	case FUN_EXT:
	    {
		ErlFunThing* funp = (ErlFunThing *) hp;
		Eterm module;
		Sint old_uniq;
		Sint old_index;
		unsigned num_free;
		int i;
		Eterm temp;

		num_free = get_int32(ep);
		ep += 4;
		hp += ERL_FUN_SIZE;
		hp += num_free;
			 *hpp = hp;
		funp->thing_word = HEADER_FUN;
		funp->num_free = num_free;
			 *objp = make_fun(funp);

		// Creator pid
		if (*ep != PID_EXT
		    || (ep = dec_pid(edep, hpp, ++ep, off_heap,
				     &funp->creator))==NULL) {
		    goto error;
		}

		// Module
		if ((ep = dec_atom(edep, ep, &module)) == NULL) {
		    goto error;
		}

		// Index
		if ((ep = dec_term(edep, hpp, ep, off_heap, &temp)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_index = unsigned_val(temp);

		// Uniq
		if ((ep = dec_term(edep, hpp, ep, off_heap, &temp)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}

		//
		// It is safe to link the fun into the fun list only when
		// no more validity tests can fail.
		//
		funp->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*)funp;
		old_uniq = unsigned_val(temp);

		funp->fe = erts_put_fun_entry(module, old_uniq, old_index);
		funp->arity = funp->fe->address[-1] - num_free;
		hp = *hpp;

		// Environment
		for (i = num_free-1; i >= 0; i--) {
		    funp->env[i] = (Eterm) COMPRESS_POINTER(next);
		    next = funp->env + i;
		}
		break;
	    }
	case ATOM_INTERNAL_REF2:
	    n = get_int16(ep);
	    ep += 2;
	    if (n >= atom_table_size()) {
		goto error;
	    }
			 *objp = make_atom(n);
	    break;
	case ATOM_INTERNAL_REF3:
	    n = get_int24(ep);
	    ep += 3;
	    if (n >= atom_table_size()) {
		goto error;
	    }
			 *objp = make_atom(n);
	    break;

	case BINARY_INTERNAL_REF:
	    {
		ProcBin* pb = (ProcBin*) hp;
		sys_memcpy(pb, ep, sizeof(ProcBin));
		ep += sizeof(ProcBin);

		erts_refc_inc(&pb->val->refc, 1);
		hp += PROC_BIN_SIZE;
		pb->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*)pb;
		pb->flags = 0;
			 *objp = make_binary(pb);
		break;
	    }
	case BIT_BINARY_INTERNAL_REF:
	    {
		Sint bitoffs = *ep++;
		Sint bitsize = *ep++;
		ProcBin* pb = (ProcBin*) hp;
		ErlSubBin* sub;
		sys_memcpy(pb, ep, sizeof(ProcBin));
		ep += sizeof(ProcBin);

		erts_refc_inc(&pb->val->refc, 1);
		hp += PROC_BIN_SIZE;
		pb->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*)pb;
		pb->flags = 0;

		sub = (ErlSubBin*)hp;
		sub->thing_word = HEADER_SUB_BIN;
		sub->size = pb->size - (bitoffs + bitsize + 7)/8;
		sub->offs = 0;
		sub->bitoffs = bitoffs;
		sub->bitsize = bitsize;
		sub->is_writable = 0;
		sub->orig = make_binary(pb);

		hp += ERL_SUB_BIN_SIZE;
			 *objp = make_binary(sub);
		break;
	    }
			 */
		default:
			error:
			/* UNDO:
			 * Must unlink all off-heap objects that may have been
			 * linked into the process.
			 */
			if (hp < *hpp) { /* Sometimes we used hp and sometimes *hpp */
				hp = *hpp;   /* the largest must be the freshest */
			}
		//undo_offheap_in_area(off_heap, hp_saved, hp);
		*hpp = hp_saved;
		return NULL;
		}
	}
	*hpp = hp;
	return ep;
}

static SInt decoded_size(byte *ep, byte* endp, int internal_tags)
{
    int heap_size = 0;
    int terms;
    int atom_extra_skip = 0;
    UInt n;

#define SKIP(sz)				\
    do {						\
	if ((sz) <= endp-ep) {		\
	    ep += (sz);				\
        } else { return -1; };	\
    } while (0)

#define SKIP2(sz1, sz2)					\
    do {								\
	UInt sz = (sz1) + (sz2);			\
	if (sz1 < sz && (sz) <= endp-ep) {	\
	    ep += (sz);						\
        } else { return -1; }			\
    } while (0)

#define CHKSIZE(sz)						\
    do {								\
	 if ((sz) > endp-ep) { return -1; }	\
    } while (0)

#define ADDTERMS(n)						\
    do {								\
        int before = terms;		        \
	terms += (n);                       \
	if (terms < before) return -1;     	\
    } while (0)


    for (terms=1; terms > 0; terms--) {
    	int tag;

    	CHKSIZE(1);
    	tag = ep++[0];
    	switch (tag) {
    	case INTEGER_EXT:
    		SKIP(4);
    		heap_size += BIG_UINT_HEAP_SIZE;
    		break;
    	case SMALL_INTEGER_EXT:
    		SKIP(1);
    		break;
    	case SMALL_BIG_EXT:
    		CHKSIZE(1);
    		n = ep[0];		/* number of bytes */
    		SKIP2(n, 1+1);		/* skip size,sign,digits */
    		heap_size += 1+(n+sizeof(Eterm)-1)/sizeof(Eterm); /* XXX: 1 too much? */
    		break;
    	case LARGE_BIG_EXT:
    		CHKSIZE(4);
    		n = get_int32(ep);
    		if (n > BIG_ARITY_MAX*sizeof(ErtsDigit)) {
    			return -1;
    		}
    		SKIP2(n,4+1);		/* skip, size,sign,digits */
    		heap_size += 1+1+(n+sizeof(Eterm)-1)/sizeof(Eterm); /* XXX: 1 too much? */
    		break;
    	case ATOM_EXT:
    		CHKSIZE(2);
    		n = get_int16(ep);
    		if (n > MAX_ATOM_CHARACTERS) {
    			return -1;
    		}
    		SKIP(n+2+atom_extra_skip);
    		atom_extra_skip = 0;
    		break;
    	case ATOM_UTF8_EXT:
    		CHKSIZE(2);
    		n = get_int16(ep);
    		ep += 2;
    		if (n > MAX_ATOM_SZ_LIMIT) {
    			return -1;
    		}
    		SKIP(n+atom_extra_skip);
    		atom_extra_skip = 0;
    		break;
    	case SMALL_ATOM_EXT:
    		CHKSIZE(1);
    		n = get_int8(ep);
    		if (n > MAX_ATOM_CHARACTERS) {
    			return -1;
    		}
    		SKIP(n+1+atom_extra_skip);
    		atom_extra_skip = 0;
    		break;
    	case SMALL_ATOM_UTF8_EXT:
    		CHKSIZE(1);
    		n = get_int8(ep);
    		ep++;
    		if (n > MAX_ATOM_SZ_LIMIT) {
    			return -1;
    		}
    		SKIP(n+atom_extra_skip);
    		atom_extra_skip = 0;
    		break;
    	/*case ATOM_CACHE_REF:
    		SKIP(1+atom_extra_skip);
    		atom_extra_skip = 0;
    		break;
    	case PID_EXT:
    		atom_extra_skip = 9;
    		// In case it is an external pid
    		heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
    		terms++;
    		break;
    	case PORT_EXT:
    		atom_extra_skip = 5;
    		// In case it is an external port
    		heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
    		terms++;
    		break;
    	case NEW_REFERENCE_EXT:
    	{
    		int id_words;

    		CHKSIZE(2);
    		id_words = get_int16(ep);

    		if (id_words > ERTS_MAX_REF_NUMBERS)
    			return -1;

    		ep += 2;
    		atom_extra_skip = 1 + 4*id_words;
    		// In case it is an external ref
    		heap_size += EXTERNAL_THING_HEAD_SIZE + id_words;
    		terms++;
    		break;
    	}
    	case REFERENCE_EXT:
    		// In case it is an external ref
    		heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
    		atom_extra_skip = 5;
    		terms++;
    		break;
    	*/
    	case NIL_EXT:
    		break;
    	case LIST_EXT:
    		CHKSIZE(4);
    		n = get_int32(ep);
    		ep += 4;
    		ADDTERMS(n);
    		terms++;
    		heap_size += 2 * n;
    		break;
    	case SMALL_TUPLE_EXT:
    		CHKSIZE(1);
    		n = *ep++;
    		terms += n;
    		heap_size += n + 1;
    		break;
    	case LARGE_TUPLE_EXT:
    		CHKSIZE(4);
    		n = get_int32(ep);
    		ep += 4;
    		ADDTERMS(n);
    		heap_size += n + 1;
    		break;
    	case STRING_EXT:
    		CHKSIZE(2);
    		n = get_int16(ep);
    		SKIP(n+2);
    		heap_size += 2 * n;
    		break;
    	/*case FLOAT_EXT:
    		SKIP(31);
    		heap_size += FLOAT_SIZE_OBJECT;
    		break;
    	case NEW_FLOAT_EXT:
    		SKIP(8);
    		heap_size += FLOAT_SIZE_OBJECT;
    		break;
    	case BINARY_EXT:
    		CHKSIZE(4);
    		n = get_int32(ep);
    		SKIP2(n, 4);
    		if (n <= ERL_ONHEAP_BIN_LIMIT) {
    			heap_size += heap_bin_size(n);
    		} else {
    			heap_size += PROC_BIN_SIZE;
    		}
    		break;
    	case BIT_BINARY_EXT:
    	{
    		CHKSIZE(5);
    		n = get_int32(ep);
    		SKIP2(n, 5);
    		if (n <= ERL_ONHEAP_BIN_LIMIT) {
    			heap_size += heap_bin_size(n) + ERL_SUB_BIN_SIZE;
    		} else {
    			heap_size += PROC_BIN_SIZE + ERL_SUB_BIN_SIZE;
    		}
    	}
    	break;
    	case EXPORT_EXT:
    		terms += 3;
    		heap_size += 2;
    		break;
    	case NEW_FUN_EXT:
    	{
    		unsigned num_free;
    		Uint total_size;

    		CHKSIZE(1+16+4+4);
    		total_size = get_int32(ep);
    		CHKSIZE(total_size);
    		ep += 1+16+4+4;
    		//FALLTHROUGH

    	case FUN_EXT:
    		CHKSIZE(4);
    		num_free = get_int32(ep);
    		ep += 4;
    		if (num_free > MAX_ARG) {
    			return -1;
    		}
    		terms += 4 + num_free;
    		heap_size += ERL_FUN_SIZE + num_free;
    		break;
    	}
    	case ATOM_INTERNAL_REF2:
    		SKIP(2+atom_extra_skip);
    		atom_extra_skip = 0;
    		break;
    	case ATOM_INTERNAL_REF3:
    		SKIP(3+atom_extra_skip);
    		atom_extra_skip = 0;
    		break;

    	case BINARY_INTERNAL_REF:
    		if (!internal_tags) {
    			return -1;
    		}
    		SKIP(sizeof(ProcBin));
    		heap_size += PROC_BIN_SIZE;
    		break;
    	case BIT_BINARY_INTERNAL_REF:
    		if (!internal_tags) {
    			return -1;
    		}
    		SKIP(2+sizeof(ProcBin));
    		heap_size += PROC_BIN_SIZE + ERL_SUB_BIN_SIZE;
    		break;
    	*/
    	default:
    		return -1;
    	}
    }
    /* 'terms' may be non-zero if it has wrapped around */
    return terms==0 ? heap_size : -1;
#undef SKIP
#undef SKIP2
#undef CHKSIZE
}
