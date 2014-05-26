/*
 * copy.c
 *
 *  Created on: May 25, 2014
 *      Author: Studnicki
 */

#include "copy.h"

unsigned int size_object(Eterm obj) {
	unsigned int sum = 0;
	for(;;) {
		switch primary_tag(obj) {
		case TAG_PRIMARY_IMMED1:
			//@todo if temporary stack is empty
			return sum;
		}
	}
	return sum;
}

Eterm copy_struct(Eterm obj, unsigned int sz, Eterm** hpp) {
	Eterm *hp = *hpp;
	switch(primary_tag(obj)) {

	}
	return obj;
}
