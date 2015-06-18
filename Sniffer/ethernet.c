#include "ethernet.h"

struct sockaddr_ll* new_sin(int sock){
    struct sockaddr_ll* sin_addr = (struct sockaddr_ll*) malloc (sizeof(struct sockaddr*));
	struct ifreq ifr;

	strncpy(ifr.ifr_name,IWFACE,IFNAMSIZ);
	ioctl(sock,SIOCGIFINDEX,&ifr,sizeof(struct ifreq));	
	memset(sin_addr,0,sizeof(struct sockaddr_ll));
	sin_addr->sll_family = PF_PACKET;
	sin_addr->sll_protocol = htons(ETHERTYPE_ARP);
	sin_addr->sll_ifindex = ifr.ifr_ifindex;
	sin_addr->sll_hatype = ARPHRD_ETHER;
	sin_addr->sll_pkttype = PACKET_BROADCAST;
	sin_addr->sll_halen = ETH_ALEN;

//    bind (sock,(struct sockaddr*)sin_addr,sizeof(struct sockaddr_ll));

    return sin_addr;
//	pfds[0].fd = sock;
//	pfds[0].events = POLLIN;


}
int write_pkt(int sock,struct sockaddr_ll* sin_addr, char* data, unsigned int dataSize){
    printf("[ethernet.c,write_pkt] sending %u bytes.\n",dataSize);
	struct ethernet_pkt pkt;
	memset (&pkt,0,sizeof(struct ethernet_pkt));

	memcpy(pkt.ether.h_source,"\x00\x00\x00\x00\x00\x00",ETHER_ADDR_LEN);
	memcpy(pkt.ether.h_dest,"\xff\xff\xff\xff\xff\xff",ETHER_ADDR_LEN);
	pkt.ether.h_proto=ETH_P_IP;

	memcpy(pkt.payload,data,dataSize);

	if (sendto(sock,(char*)&pkt,sizeof(struct ethhdr)+dataSize,0,(struct sockaddr*)sin_addr,sizeof(*sin_addr)) == -1){
	    printf("[ethernet.c, write_pkt_()] sendto() : %s \n", strerror(errno));
		close(sock);
		return -1;
	}

	printf("1 packet sent.\n");
	return 0;

}

