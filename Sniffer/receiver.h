#ifndef RECEIVER_H
#define RECEIVER_H
//#include <stdio.h>
//#include <stdlib.h>
//#include <sys/socket.h>
//#include <linux/if_packet.h>
//#include <linux/if_ether.h>
//#include <linux/if_arp.h>
//#include <linux/sockios.h>
//#include <net/ethernet.h>
#include <errno.h>
#include <string.h>
#include "ethernet.h"


//char buf[PAYLOAD_MAX_SIZE];

int getSock();
char* newBuf (void){return (char*) malloc (PAYLOAD_MAX_SIZE*sizeof(char));};
void delBuf (char* buf){free(buf);}

void closeSock(int s){close(s);}
char* getBuf(char*);
int next_pkt(int,struct sockaddr_ll*, char* buf);
void delete_sin (struct sockaddr_ll* sin_addr){free(sin_addr);}

#endif
