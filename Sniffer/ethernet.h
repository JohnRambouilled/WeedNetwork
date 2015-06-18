#ifndef ETHERNET_H
#define ETHERNET_H
#include <stdio.h>
#include <stdlib.h>
#include <linux/sockios.h>
#include <net/if.h>
#include <net/ethernet.h>
#include <netpacket/packet.h>
#include <netinet/if_ether.h>
#include <sys/socket.h>
#include <string.h>
#include <sys/poll.h>
#include <stropts.h>
#include <errno.h>

#define IWFACE  "eth0" //"prism0"
#define PAYLOAD_MAX_SIZE 1500

struct ethernet_pkt{
	struct ethhdr ether;
	char payload[PAYLOAD_MAX_SIZE];
}__attribute__((packed));


//struct pollfd pfds[1]; // Global var for the poll() descriptors list
//fd_set readfs;
//struct timeval timer;

int write_pkt(int sock, struct sockaddr_ll* sin_addr, char* data, unsigned int dataSize);
struct sockaddr_ll* new_sin(int sock);

#endif
