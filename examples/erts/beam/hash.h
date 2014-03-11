/*
 * hash.h
 *
 *  Created on: Nov 2, 2013
 *      Author: Studnicki
 */

#ifndef HASH_H_
#define HASH_H_

// General hash functions

typedef uint64_t HashValue;

typedef struct HashBucket {
	struct HashBucket* next;
	HashValue hvalue;
} HashBucket;

typedef struct HashFunctions {
	H_FUN hash;
} HashFunctions;

#endif /* HASH_H_ */
