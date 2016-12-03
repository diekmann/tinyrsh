#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

#include "handler.h"

void handle_connection(int sockfd){
	printf("handling sockfd %d\n", sockfd);

	int childpid;
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
		close(sockfd);
		waitpid(childpid, NULL, 0);
		printf("child %d exited\n", childpid);
	}

}

