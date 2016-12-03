#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>

#include "handler.h"

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

	char ipstr[INET_ADDRSTRLEN];

	puts("starting");

	// create socket
	int srv_sockfd;
	if((srv_sockfd = socket(AF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0)) == -1){
		perror("socket");
		exit(1);
	}
	//TODO propper cleanup on exit
	int reuse = 1;
	if (setsockopt(srv_sockfd, SOL_SOCKET, SO_REUSEADDR, (const char*)&reuse, sizeof(reuse)) < 0){
        	perror("setsockopt(SO_REUSEADDR) failed");
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

