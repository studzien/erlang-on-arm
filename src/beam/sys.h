/*
 * sys.h
 *
 *  Created on: Apr 1, 2014
 *      Author: Studnicki
 */

#ifndef SYS_H_
#define SYS_H_


#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))

#define get_int8(s) ((((unsigned char*)  (s))[0] ))

#endif /* SYS_H_ */
