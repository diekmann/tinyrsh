#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>

#include "handler.h"


//server listening socket
static int srv_sockfd = 0;

static void shutdown_socket(int sockfd){
	if(srv_sockfd){
		puts("shutdown socket");
		if(shutdown(sockfd, SHUT_RDWR) == -1)
			perror("shutdown in signal handler");
		if(close(sockfd) == 1)
			perror("close in signal handler");
	}
}

static int sig_exit_cnt = 0;

static void handle_sigint(int sig){
	printf("got signal %d\n", sig);
	if(childpid){
		printf("running child: %d\n", childpid);
	}
	++sig_exit_cnt;
	if(sig_exit_cnt >= 2){
		shutdown_socket(srv_sockfd);
		exit(0);
	}
	printf("press %d more times to exit\n", 2 - sig_exit_cnt);
}

static struct sockaddr_in* prepare_listen(const char* port){
	int status;
	struct addrinfo hints;
	struct addrinfo *res; 

	memset(&hints, 0, sizeof hints); // make sure the struct is empty
	hints.ai_family = AF_INET; //ipv4 only
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;

	if ((status = getaddrinfo(NULL, port, &hints, &res)) != 0) {
		fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(status));
		exit(1);
	}

	if(res->ai_next != NULL){
		fprintf(stderr, "getaddrinfo returned more than one result\n");
		exit(1);
	}

	if(res->ai_family != AF_INET){
		fprintf(stderr, "legacy IP only\n");
		exit(1);
	}

	struct sockaddr_in *retval = malloc(sizeof(struct sockaddr_in));

	memcpy(retval, res->ai_addr, sizeof(struct sockaddr_in));

	freeaddrinfo(res);

	return retval;
}


int main(int argc, char **argv){
	const char *const port = "6699";

	signal(SIGINT, handle_sigint);

	char ipstr[INET_ADDRSTRLEN];

	puts("starting");

	// create socket
	if((srv_sockfd = socket(AF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0)) == -1){
		perror("socket");
		exit(1);
	}
	
	int reuse = 1;
	if(setsockopt(srv_sockfd, SOL_SOCKET, SO_REUSEADDR, (const char*)&reuse, sizeof(reuse)) == -1){
        	perror("setsockopt (SO_REUSEADDR)");
	}

	// get local listening addr
	struct sockaddr_in* listenaddr = prepare_listen(port);
	
	inet_ntop(AF_INET, &listenaddr->sin_addr, ipstr, sizeof(ipstr));
	printf("preparing to listen on %s:%s\n", ipstr, port);

	if(bind(srv_sockfd, (struct sockaddr *)listenaddr, sizeof(struct sockaddr_in)) == -1){
		perror("bind");
		exit(1);
	}

	//listen
	if(listen(srv_sockfd, 1) == -1){ //backlog of one!
		perror("listen");
		exit(1);
	}
	free(listenaddr);

	while(1){
		//accept connections
		int client_sockfd;
		socklen_t sin_size = sizeof(struct sockaddr_in);
		struct sockaddr_in remote_addr;
		if((client_sockfd = accept(srv_sockfd, (struct sockaddr *)&remote_addr, &sin_size)) == -1){
			perror("accept");
			exit(1);
		}
		assert(sin_size == sizeof(struct sockaddr_in));
		assert(remote_addr.sin_family == AF_INET);

		inet_ntop(AF_INET, &remote_addr.sin_addr, ipstr, sizeof(ipstr));
		printf("connection from %s\n", ipstr);

		handle_connection(client_sockfd);

		close(client_sockfd);
	}

	close(srv_sockfd);

	return 0;
}

