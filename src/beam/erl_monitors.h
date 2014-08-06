/*
 * erl_monitors.h
 *
 *  Created on: Aug 4, 2014
 *      Author: Studnicki
 */

#ifndef ERL_MONITORS_H_
#define ERL_MONITORS_H_

#include "erl_term.h"

typedef struct erts_link {
	struct erts_link *next;
	Eterm pid;
} ErtsLink;

int erts_add_link(ErtsLink **root, Eterm pid);
void erts_remove_link(ErtsLink **root, Eterm pid);

#endif /* ERL_MONITORS_H_ */
