/*
 * erl_comm.h
 *
 *  Created on: Sep 1, 2014
 *      Author: dfinlay
 */

#ifndef ERL_COMM_H_
#define ERL_COMM_H_

typedef unsigned char byte;

int read_cmd(byte *buf);

int write_cmd(byte *buf, int len);

#endif /* ERL_COMM_H_ */
