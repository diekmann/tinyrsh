#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/wait.h>
#include <sys/socket.h>

#include "handler.h"

int childpid = 0;


void handle_connection(int sockfd){
	printf("handling sockfd %d\n", sockfd);

	if((childpid = fork()) == -1){
		perror("fork");
		exit(1);
	}
	if(childpid == 0){
		//child
		int orig_stderr = dup(STDERR_FILENO);

		dup2(sockfd, STDOUT_FILENO);
		close(sockfd);
		dup2(STDOUT_FILENO, STDIN_FILENO);
		dup2(STDOUT_FILENO, STDERR_FILENO);

		if(execl("/bin/sh", "sh", NULL) == -1){
			//print error in server, do not send to connected client
			dup2(orig_stderr, STDERR_FILENO);
			perror("exec in conn handler");
		}
		exit(0);
	}else{
		printf("Successfully forked %d\n", childpid);
		waitpid(childpid, NULL, 0);
		printf("child %d exited\n", childpid);
		childpid = 0; //child dead?
		if(shutdown(sockfd, SHUT_RDWR) == -1)
			perror("shutdown child's sockfd");
		if(close(sockfd) == -1)
			perror("close child's sockfd");
	}

}

