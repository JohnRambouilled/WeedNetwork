#include <time.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <sys/types.h>
#include <stdint.h>

#include <netinet/in.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#define UDP_PAYLOAD_MAX 4096
struct udp_packet {char* buf;
                   ssize_t size;
                   struct sockaddr_un* src;
                   int src_size;};

/*Constructors, destructors*/
struct udp_packet* new_udp_packet (int size){
    char* buf = (char*) malloc (size * sizeof(char));
    struct sockaddr_un* src = (struct sockaddr_un*) malloc (sizeof (struct sockaddr_un));
    struct udp_packet* ret = (struct udp_packet*) malloc (sizeof (struct udp_packet));

    ret->buf = buf;
    ret->size = size;
    ret->src = src;

    memset (src,0,sizeof(struct sockaddr_un));
    src->sun_family = AF_UNIX;
    ret->src_size = sizeof(struct sockaddr_un);
    return ret;
}

void free_udp_packet (struct udp_packet* pkt){
    free (pkt->buf);
    free (pkt);
}

/*Accessors*/
char* udp_buf (struct udp_packet* pkt) {return pkt->buf;}
ssize_t udp_size (struct udp_packet* pkt) {return pkt->size;}
char* udp_src (struct udp_packet* pkt) {return pkt->src->sun_path;}
int udp_src_size (struct udp_packet* pkt) {return pkt->src_size;}



/*Reads the next packet from the socket*/

void set_block (int sock){
    int opts = fcntl(sock,F_GETFL);
    opts &= ~O_NONBLOCK;
    fcntl(sock,F_SETFL,opts);
}

struct udp_packet* udp_nextPacket (int sock, ssize_t max_size) {
        /*
    int sock = socket (AF_LOCAL,SOCK_DGRAM,0);
    struct sockaddr_un src;
    src.sun_family = AF_UNIX;
    sprintf((src.sun_path),"testounet3.socket ");
    if (bind (sock,(struct sockaddr*) &src,sizeof(struct sockaddr_un)) < 0){
        printf("[udp.c udp_nextPacket()] bind() error : %s\n",strerror(errno));
        exit(-1);
    }
*/
    char buf[max_size];
    struct udp_packet* ret = NULL;

    struct sockaddr_un dest;
    socklen_t dest_size = sizeof(struct sockaddr_un);
    bzero(&dest,sizeof(dest_size));
    dest.sun_family=AF_UNIX;

    int nb_read = recvfrom(sock,&buf,max_size,0,(struct sockaddr*) &dest, &dest_size);
    if (nb_read < 0){
        printf("[udp.c udp_nextPacket()] recvfrom error : %s\n",strerror(errno));
        exit(-1);
    }
    printf("Packet received : %d bytes from %s(%zu bytes)\n",nb_read,dest.sun_path,dest_size);

/*    struct udp_packet* ret = new_udp_packet (max_size);
    printf("Waiting for packets...\n");
    ret->size = recvfrom (sock,&(ret->buf),max_size,0,(struct sockaddr*)ret->src,(socklen_t*)&(ret->src_size));
    if (ret->size < 0){
        printf("[udp.c udp_nextPacket()] recvfrom error : %s\n",strerror(errno));
        exit(-1);
    }
    printf("Packets received (%d bytes) from %s (%d bytes)\n",ret->size,ret->src->sun_path,ret->src_size);
*/  
      return ret;

}

