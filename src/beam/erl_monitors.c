/*
 * erl_monitors.c
 *
 *  Created on: Aug 4, 2014
 *      Author: Studnicki
 */

#include "erl_monitors.h"
#include "io.h"

static void dump_links(ErtsLink *root, Eterm pid) {
	char buf[30];
	while(root) {
		sprintf(buf, "%d ", pid2pix(root->pid));
		debug(buf);
		root = root->next;
	}
	debug("\n");
}

int erts_add_link(ErtsLink **root, Eterm pid) {
	if(sizeof(ErtsLink) > 1000) {
		debug("erl_monitors.c:23\n");
		debug_32(sizeof(ErtsLink));
	}
	ErtsLink* new = (ErtsLink*)pvPortMalloc(sizeof(ErtsLink));
	new->next = *root;
	new->pid = pid;
	*root = new;

	//dump_links(*root, pid);
	return 0;
}

void erts_remove_link(ErtsLink **root, Eterm pid) {
	ErtsLink* link = *root, *prev = NULL;
	while(link && link->pid != pid) {
		prev = link;
		link = link->next;
	}

	if(link) {
		// change head
		if(prev == NULL) {
			*root = link->next;
		}
		// change in the middle
		else {
			prev->next = link->next;
		}
		vPortFree(link);
	}

	//dump_links(*root, pid);
}
