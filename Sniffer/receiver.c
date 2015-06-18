#include "receiver.h"

int getSock(void){
	int s;
	s = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
	if (s == -1) {printf("Error. Maybe not root ?\n"); exit(0);}
	return s;
}

char* getBuf(char* buf){
	struct ethernet_pkt* ret = (struct ethernet_pkt*) buf;
	return ret-> payload;
}
int next_pkt(int s, struct sockaddr_ll* sin_addr, char* buf){
    socklen_t confSiz = sizeof (*((struct sockaddr*) sin_addr));
	//int siz = recvfrom(s,buf,ETH_FRAME_LEN,0,(struct sockaddr*)sin_addr,&confSiz);
	int siz = recvfrom(s,buf,ETH_FRAME_LEN,0,NULL,NULL);
	if (-1 == siz){
		printf("[receiver.c] : %s \n", strerror(errno));
		return 0;
	}
	printf("[receiver.c] : received %d bytes.\n",siz);
	return siz;
}

