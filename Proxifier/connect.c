#define _GNU_SOURCE
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

//#define SOCKET_PATH_STREAM "/home/sat/svn/EN/New3/testounet.stream.socket"
//#define SOCKET_PATH_STREAM "/home/sat/svn/EN/New3/testounet_tcp.socket"
#define PAYLOAD_SIZE 1500
	#define SOCKTYPE_STREAM 0
	#define SOCKTYPE_DGRAM 1
 #define SOCKCONF_TYPE_LOCAL 0
    #define SOCKCONF_TYPE_INET 1

struct sock_wrapper{
    u_int8_t inet_init;
	u_int8_t  type;
	u_int32_t addr;
	u_int16_t port;
}__attribute__((packed));


struct prox_ans{
    u_int8_t type;
    u_int32_t addr;
    u_int16_t port;
    u_int64_t data_len;
}__attribute__((packed));

int (*orig_connect) (int , const struct sockaddr*, socklen_t);
int (*orig_socket) (int,int,int);
ssize_t (*orig_recv) (int, void*, size_t, int);
ssize_t (*orig_send) (int,const void* ,size_t,int);
ssize_t (*orig_sendto) (int,const void* ,size_t,int,const struct sockaddr*, socklen_t);
ssize_t (*orig_recvfrom) (int,void* ,size_t,int,const struct sockaddr*,socklen_t);
ssize_t (*orig_sendmsg) (int , const struct msghdr*, int );
ssize_t (*orig_recvmsg) (int, struct msghdr*, int);


void write_sockaddr (int sockfd, const struct sockaddr_in* addr, int sotype);
char* SOCKET_PATH_STREAM;
char* SOCKET_PATH_DGRAM;



void _init(void)
{
	printf("Loading hack.\n");
	orig_connect = dlsym(RTLD_NEXT,"connect");
	orig_socket = dlsym(RTLD_NEXT,"socket");
	orig_recv = dlsym(RTLD_NEXT,"recv");
	orig_send = dlsym(RTLD_NEXT,"send");
	orig_sendto = dlsym(RTLD_NEXT,"sendto");
	orig_recvfrom = dlsym(RTLD_NEXT,"recvfrom");
    orig_sendmsg = dlsym(RTLD_NEXT,"sendmsg");
    orig_recvmsg = dlsym(RTLD_NEXT,"recvmsg");
    
    SOCKET_PATH_STREAM = getenv ("WEED_TCP_SOCKET");
    SOCKET_PATH_DGRAM = getenv ("WEED_UDP_SOCKET");
    if (NULL == SOCKET_PATH_STREAM){
        perror("please, specifies the socket to the unix tcp server in the global WEED_TCP_SOCKET environment variable.\n");
        exit(-1);
    }

}


ssize_t sendto(int socket, const void *message, size_t length,
              int flags, const struct sockaddr *dest_addr,
              socklen_t dest_len){

    int so_type = -1;
	socklen_t optlen = sizeof so_type;
	
	getsockopt (socket,SOL_SOCKET,SO_TYPE,&so_type,&optlen);
    if (so_type == SOCK_DGRAM || so_type == 2050){
        write_sockaddr(socket,(struct sockaddr_in*) dest_addr,so_type);

        struct sockaddr_un proxy;
        proxy.sun_family = AF_UNIX;
        strcpy((char*)&(proxy.sun_path),SOCKET_PATH_DGRAM);
        ssize_t ret = orig_sendto(socket,message,length,0,(struct sockaddr*)&proxy,sizeof(struct sockaddr_un));
        printf("[connect.c,sendto()] %zd bytes written\n",ret);
        return ret;

        
    }
    else{
            printf("sendto ...%d\n",so_type);
            ssize_t ret = orig_sendto(socket,message,length,flags,dest_addr,dest_len);
            printf("[connect.c,sendto()] %zd bytes written\n",ret);
            return ret;
    }


}


/*
ssize_t recv (int sockfd,void* buf,size_t len, int flags){
    char hj_buf[len];
    printf("[RECV] waiting for data");
    ssize_t ret = orig_recv (sockfd, hj_buf,len,flags);
    printf("[RECV] : %s\n",hj_buf);
    memcpy (buf,&hj_buf,len);
    return ret;

}
*/
ssize_t recvfrom(int socket, void *buffer, size_t length,
                                  int flags, struct sockaddr *address,
                                             socklen_t *address_len){
    struct prox_ans* ans_hdr;
    char buf [4096];
    ssize_t hdr_siz = orig_recvfrom (socket,(void*)&buf,4096,0,NULL,0);
    //ssize_t hdr_siz = orig_recvfrom (socket,(void*)&ans_hdr,sizeof(struct prox_ans),0,NULL,NULL);
    ans_hdr = (struct prox_ans*) &buf;
    struct in_addr addr;
    addr.s_addr = ans_hdr->addr;
    //printf("[connect.c, recvfrom()] RECVFROM() %zu bytes from destinary (%s,%zu)!!\n",hdr_siz,inet_ntoa(addr),sizeof(struct prox_ans));


    /*
    if (length < ans_hdr.data_len){
        printf("[connect.c, recvfrom()] RECVFROM requested length lower than required for receiving the packet (%zu,%zu)\n",length,ans_hdr.data_len);
        exit(-1);
    }
    */
    //TODO check the sizes of the buffers
//    printf("[connect.c,recvfrom()] waiting for data (%zu bytes)\n",ans_hdr->data_len);
//    ssize_t ret = orig_recvfrom (socket,buffer,length,flags,address,address_len);
//    ssize_t ret = orig_recvfrom (socket,buffer,length,flags,address,address_len);
    
    struct sockaddr_in* sender = (struct sockaddr_in*) address;
    memset (sender,0,*address_len);
    sender->sin_family = AF_INET;
    sender->sin_port = ans_hdr->port;
    sender->sin_addr.s_addr = ans_hdr->addr;

    struct in_addr r = sender->sin_addr;
    char* addr2 = inet_ntoa (r);
//    printf("[connect.c,recvfrom()] %zu received after the header %s:%d.\n",ret,addr2,ntohs(sender->sin_port));
    ssize_t ret =  (hdr_siz - sizeof(struct prox_ans));
    //printf("Copying %zu bytes (%zd)...\n",ret,ans_hdr->data_len);
    memcpy(buffer, buf + sizeof(struct prox_ans),hdr_siz - sizeof(struct prox_ans));

//    printf("Copying %d bytes.\n",ret);
/*    int i=0;
    for (i=0; i < ret;i++)
            printf("%c",((char*) buffer)[i]);
    printf("\n");
  */  *address_len = (socklen_t) sizeof (struct sockaddr_in);
     return ret;
  //  return (hdr_siz - sizeof (struct prox_ans));
}

int socket (int domain, int type, int protocol){
	int ret;
    printf("af_inet %d, pf_inet = %d, pf_inet6=%d , ?=%d %d %d\n",AF_INET,PF_INET,PF_INET6,domain,type,protocol);
	//if ((AF_INET == domain) && (SOCK_STREAM == type || SOCK_DGRAM == type || 2050 == type)){
	if ((AF_INET == domain) && (SOCK_DGRAM == type || 2050 == type || SOCK_STREAM == type)){
//    if (AF_INET == domain && SOCK_STREAM == type){
		printf("socket() hijacked => unix\n");
        if (2050 == type)
                type = SOCK_DGRAM;
		ret = orig_socket (AF_UNIX,type,0);	

		if (ret < 0)
			printf("[connect.c, socket()] : %s \n", strerror(errno));
        if (SOCK_DGRAM == type || 2050 == type){
                struct sockaddr_un me;
                me.sun_family=AF_UNIX;
                strcpy((char*)&(me.sun_path),tmpnam(NULL));
                if (bind(ret,&me,sizeof(me)) < 0){
                    printf("[connect.c,connect()] bind() has failed : %s\n",strerror(errno));
                    return -1;
                }
                printf("[connect.c,connect()] udp socket bound.\n");
        }
		printf("socket() hijacked !\n");
		return ret;
	}
        else printf("socket() ignored\n");
	ret = orig_socket(domain,type,protocol);
	return ret;
}

int connect_ (int socket, const struct sockaddr_in* sin,int so_type,char* socket_path, socklen_t address_len){
		struct sockaddr_un* sun = (struct sockaddr_un*) malloc (sizeof(struct sockaddr_un));

        sun->sun_family = AF_UNIX;

        printf("[connect.c,connect_()] connecting to %s\n",socket_path);
		sprintf ((char*) &(sun->sun_path),"%s",socket_path);
		
		int ret = orig_connect(socket,(struct sockaddr*) sun,sizeof(struct sockaddr_un));
		free (sun); 
		if (ret < 0){
			printf("[connect.c, connect_()] : %s \n", strerror(errno));
            exit(-1);
            return ret;
        }
		printf("ret=%d\n",ret);
 		write_sockaddr(socket,sin,so_type);
		return ret;

}
int connect (int socket, const struct sockaddr* address, socklen_t address_len){
	struct sockaddr_in* sin = (struct sockaddr_in*) address;
	int so_type = -1;
	socklen_t optlen = sizeof so_type;
	
	getsockopt (socket,SOL_SOCKET,SO_TYPE,&so_type,&optlen);
	if ((AF_INET == sin->sin_family) || PF_INET == sin->sin_family){ // && SOCK_STREAM  == so_type){
            
        if (SOCK_STREAM == so_type){
                printf("[connect.c,connect()] tcp socket hijacked\n");
                return connect_ (socket,sin,SOCK_STREAM,SOCKET_PATH_STREAM,address_len);
        }
        

        if (2050 == so_type || SOCK_DGRAM == so_type){
                printf("[connect.c, connect()] udp socket hijacked\n");
                return connect_ (socket,sin,SOCK_DGRAM,SOCKET_PATH_DGRAM,address_len);
        }
	}

	return orig_connect (socket, address, address_len);
}

void write_sockaddr (int sockfd, const struct sockaddr_in* addr, int sotype){

    /*Else*/
    printf("[connect.c,write_sockaddr()] weedify a new connection to a remote socket...\n");
    struct sock_wrapper* sw = (struct sock_wrapper*) malloc (sizeof(struct sock_wrapper));
    sw->inet_init=3;
	sw->addr=addr->sin_addr.s_addr;
	sw->port=addr->sin_port;

	if (SOCK_STREAM == sotype)
		sw->type = SOCKTYPE_STREAM;
	else if (SOCK_DGRAM == sotype)
		sw->type = SOCKTYPE_DGRAM;

	printf("sending %zd bytes : %u\t%u\t%u - %s-%s\n",sizeof(struct sock_wrapper),sw->inet_init,addr->sin_addr.s_addr,addr->sin_port,inet_ntoa(addr->sin_addr),
                                        inet_ntoa(*((struct in_addr*) &(sw->addr))) );


	//ssize_t ret = orig_send(sockfd,sw,sizeof(struct sock_wrapper),0);
    //
    struct sockaddr_un server;
    server.sun_family = AF_UNIX;
    if (SOCK_STREAM == sotype)
            strcpy ((server.sun_path),SOCKET_PATH_STREAM);
    else if (SOCK_DGRAM == sotype)
            strcpy((server.sun_path),SOCKET_PATH_DGRAM);

    ssize_t ret = -1;
    if (SOCK_STREAM == sotype)
            ret = orig_send (sockfd,sw,sizeof(struct sock_wrapper),0);
    else if (SOCK_DGRAM == sotype)
            ret = orig_sendto(sockfd,sw,sizeof(struct sock_wrapper),0,(struct sockaddr*)&server,sizeof(struct sockaddr_un));

    if (ret < 0){
			printf("[connect.c, write_addr()] : %s \n", strerror(errno));
    }
	//long int ret = orig_send(sockfd,sw,3*sizeof(u_int8_t) + sizeof(u_int32_t) + sizeof (u_int16_t),0);
//
//	orig_send(sockfd,sw,9,0);
	printf("writing %zd / %zd bytes : %u\t%u\t%u - %s-%s\n",ret,sizeof(struct sock_wrapper),addr->sin_addr.s_addr,addr->sin_port,inet_ntoa(addr->sin_addr),
                                        inet_ntoa(*((struct in_addr*) &(sw->addr))) );
    free(sw);

	return;
}
ssize_t sendmsg(int socket, const struct msghdr *message, int flags){

    printf("[SENDMSG !!!!!]\n");
    orig_sendmsg(socket,message,flags);
}
ssize_t recvmsg(int sockfd, struct msghdr *msg, int flags){
        ssize_t ret;

        printf("[RECVMSG !!!\n");
        ret = orig_recvmsg(sockfd, msg, flags);
        return ret;
}




