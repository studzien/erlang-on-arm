/*
 * external.h
 *
 *  Created on: Jul 17, 2014
 *      Author: Studnicki
 */

#ifndef EXTERNAL_H_
#define EXTERNAL_H_

/* Same order as the ordering of terms in erlang */

/* Since there are 255 different External tag values to choose from
   There is no reason to not be extravagant.
   Hence, the different tags for large/small tuple e.t.c
*/

#include "erl_term.h"
#include "atom.h"
#include "big.h"

#define SMALL_INTEGER_EXT 'a'
#define INTEGER_EXT       'b'
#define FLOAT_EXT         'c'
#define ATOM_EXT          'd'
#define SMALL_ATOM_EXT    's'
#define REFERENCE_EXT     'e'
#define NEW_REFERENCE_EXT 'r'
#define PORT_EXT          'f'
#define NEW_FLOAT_EXT     'F'
#define PID_EXT           'g'
#define SMALL_TUPLE_EXT   'h'
#define LARGE_TUPLE_EXT   'i'
#define NIL_EXT           'j'
#define STRING_EXT        'k'
#define LIST_EXT          'l'
#define BINARY_EXT        'm'
#define BIT_BINARY_EXT    'M'
#define SMALL_BIG_EXT     'n'
#define LARGE_BIG_EXT     'o'
#define NEW_FUN_EXT       'p'
#define EXPORT_EXT        'q'
#define FUN_EXT           'u'
#define ATOM_UTF8_EXT     'v'
#define SMALL_ATOM_UTF8_EXT 'w'

#define DIST_HEADER       'D'
#define ATOM_CACHE_REF    'R'
#define ATOM_INTERNAL_REF2 'I'
#define ATOM_INTERNAL_REF3 'K'
#define BINARY_INTERNAL_REF 'J'
#define BIT_BINARY_INTERNAL_REF 'L'
#define COMPRESSED        'P'


#define VERSION_MAGIC 131   /* 130 in erlang 4.2 */
                /* Increment this when changing the external format. */
                /* ON the other hand, don't change the external format */
                /* since that breaks other people's code! */


SInt erts_decode_ext_size(byte*, UInt);
Eterm erts_decode_ext(Eterm **, byte**);


#endif /* EXTERNAL_H_ */
