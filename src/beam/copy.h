/*
 * copy.h
 *
 *  Created on: May 25, 2014
 *      Author: Studnicki
 */

#ifndef COPY_H_
#define COPY_H_

#include "global.h"

unsigned int size_object(Eterm obj);
Eterm copy_struct(Eterm obj, unsigned int sz, Eterm** hpp);

#endif /* COPY_H_ */
