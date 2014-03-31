/*
 * io.h
 *
 *  Created on: Mar 31, 2014
 *      Author: Studnicki
 */

#ifndef IO_H_
#define IO_H_

#if DEBUG == 1
#define debug(what) do { vPrintString(what); } while(0)
#else
#define debug(what) do {} while(0)
#endif

#if DEBUG == 1
#define debug_32(what) do { vPrint4Bytes(what); } while(0)
#else
#define debug_32(what) do {} while(0)
#endif

#if DEBUG == 1
#define debug_8(what) do { vPrintByte(what); } while(0)
#else
#define debug_8(what) do {} while(0)
#endif


#endif /* IO_H_ */
