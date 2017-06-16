#define _GNU_SOURCE

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <fcntl.h>
#include <errno.h>
#include <assert.h>
#include <dirent.h>

#define IS_BADF(retval) {badf_errno &= (retval == -1 && errno == EBADF);}

static int FD_IS_ZERO(fd_set *set){
    for(int i = 0; i < FD_SETSIZE; ++i){
        if(FD_ISSET(i, set))
            return 0;
    }
    return 1;
}


#define OUTBUF_SIZE 1024
static char outbuf[OUTBUF_SIZE];

//returns a pointer to outbuf. Not multithreded!
static char* try_proc_fd(int fd){
	int written;
	char buf[256];
    memset(outbuf, 0, OUTBUF_SIZE);
	written = snprintf(buf, 256, "/proc/self/fd/%d", fd);
	assert(written > 0 && written < 255);
	written = readlink(buf, outbuf, OUTBUF_SIZE);
	assert(written > 0 && written < OUTBUF_SIZE);
	return outbuf;
}

int main(int argc, char **argv){
	const long int max_fd = sysconf(_SC_OPEN_MAX);
	printf("_POSIX_OPEN_MAX: %d\n", _POSIX_OPEN_MAX);
	printf("sysconf(_SC_OPEN_MAX): %ld\n", max_fd);

	assert (max_fd <= INT_MAX);

	int flags, accmode, haslock, haslock_nonposix, sigpidrecv, pipesize;
	struct flock flock, flock_nonposix;

    // mark all fds we discovered by walking the fd numbers and double check with
    // /proc/self/fd that we got all.
    fd_set handled_fds;
    FD_ZERO(&handled_fds);

	for(int fd = 0; fd < (int)max_fd; ++fd){
		//set to one if EBADF
		int badf_errno = 1;
		
		flags = fcntl(fd, F_GETFD);
		IS_BADF(flags);

		accmode = fcntl(fd, F_GETFL);
		IS_BADF(accmode);

		memset(&flock, 0, sizeof(flock));
		haslock = fcntl(fd, F_GETLK, &flock);
		IS_BADF(haslock);

#ifndef F_OFD_GETLK
#error "fix no F_OFD_GETLK"
		haslock_nonposix = 0;
#else
		memset(&flock_nonposix, 0, sizeof(flock_nonposix));
		haslock_nonposix = fcntl(fd, F_OFD_GETLK, &flock_nonposix);
		IS_BADF(haslock_nonposix);
#endif

		sigpidrecv = fcntl(fd, F_GETOWN);
		IS_BADF(sigpidrecv)

		pipesize = fcntl(fd, F_GETPIPE_SZ);
		IS_BADF(pipesize)

		if(!badf_errno){
			assert(haslock == 0);
			assert(haslock_nonposix == 0);
			printf("%d: %x %x %d %d %d %s\n", fd, flags, accmode,
				flock.l_type == F_UNLCK,
				//flock_nonposix.l_type == F_UNLCK,
				sigpidrecv,
				pipesize,
			    try_proc_fd(fd));
            FD_SET(fd, &handled_fds);
		}
	
	}

    DIR *procdir = opendir("/proc/self/fd");
    assert(procdir);
    struct dirent *entry;
    while((entry = readdir(procdir)) != NULL){
        if(strncmp(entry->d_name, ".", 255) == 0 || strncmp(entry->d_name, "..", 255) == 0){
            continue;
        }
        assert(strnlen(entry->d_name, 255) <= 5);
        for(char *c = entry->d_name; *c; ++c){
            assert(isdigit(*c));
        }
        int fd = atoi(entry->d_name);
        if(fd == dirfd(procdir)){
            continue;
        }
        assert(FD_ISSET(fd, &handled_fds));
        FD_CLR(fd, &handled_fds);
    }
    closedir(procdir);
    assert(FD_IS_ZERO(&handled_fds)); //all fds which are listed in /proc were also found by our fd fcntl enumeration


	return 0;
}

