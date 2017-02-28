/*copied from https://github.com/wking/ccon and man (7) user_namespaces*/
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sched.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mount.h>

#define STACK_SIZE (1024 * 1024)

#define errExit(msg)    do { perror(msg); exit(EXIT_FAILURE); \
                               } while (0)

int pipe_fd[2];  /* Pipe used to synchronize parent and child */

static int child_func(void* args){

  /* Wait until the parent has updated the UID and GID mappings.
     See the comment in main(). We wait for end of file on a
     pipe that will be closed by the parent process once it has
     updated the mappings. */

  close(pipe_fd[1]);    /* Close our descriptor for the write
                           end of the pipe so that we see EOF
                           when parent closes its descriptor */
  char ch;
  if (read(pipe_fd[0], &ch, 1) != 0) {
      fprintf(stderr,
              "Failure in child: read from pipe returned != 0\n");
      exit(EXIT_FAILURE);
  }

  //try to exec the binary from the NEW mount so we can umount the old
  //puts("bind mount");
  //if(mount("./mnt1", "./mnt1", NULL, MS_BIND, NULL) == -1)
  //  errExit("bindmount1");
  puts("tmpfs mount /");
  if(mount(NULL, "./mnt1", "tmpfs", 0, NULL) == -1)
    errExit("tmpfsmount");

  puts("make mounts private");
  if(mount("none", "/", NULL, MS_REC|MS_PRIVATE, NULL) == -1)
    errExit("mount private");

  puts("mount proc");
  if(mkdir("./mnt1/proc", 777) == -1)
    errExit("mkdir proc");
  if(mount(0, "./mnt1/proc", "proc", MS_NOSUID|MS_NODEV|MS_NOEXEC, NULL) == -1)
    errExit("procmount1");

  puts("cp busybox");
  system("cp busybox ./mnt1/busybox");

  puts("pivot_root");
  if(mkdir("./mnt1/oldroot", 777) == -1)
    errExit("mkdir oldroot");
  if(pivot_root("./mnt1", "./mnt1/oldroot") == -1)
    errExit("pivto_root");

  puts("chdir");
  if(chdir("/") == -1)
    errExit("chdir /");

  // mount must be private
  puts("unount old root");
  if(umount2("/oldroot", MNT_DETACH) == -1)
    errExit("umount");

  puts("execl busybox sh");
  execl("/busybox", "busybox", "sh", NULL);
  errExit("exec");
}

void *child_stack;



/* Update the mapping file 'map_file', with the value provided in
   'mapping', a string that defines a UID or GID mapping. A UID or
   GID mapping consists of one or more newline-delimited records
   of the form:

       ID_inside-ns    ID-outside-ns   length

   Requiring the user to supply a string that contains newlines is
   of course inconvenient for command-line use. Thus, we permit the
   use of commas to delimit records in this string, and replace them
   with newlines before writing the string to the file. */

static void
update_map(char *mapping, char *map_file)
{
    int fd;
    size_t map_len;     /* Length of 'mapping' */

    /* Replace commas in mapping string with newlines */
    map_len = strlen(mapping);
    for (int j = 0; j < (int)map_len; j++)
        if (mapping[j] == ',')
            mapping[j] = '\n';

    fd = open(map_file, O_RDWR);
    if (fd == -1) {
        fprintf(stderr, "ERROR: open %s: %s\n", map_file,
                strerror(errno));
        exit(EXIT_FAILURE);
    }

    ssize_t wres = write(fd, mapping, map_len);
    if (wres < 0)
        errExit("write");
    if ((size_t)wres != map_len) {
        fprintf(stderr, "ERROR: incomplete write %s: %s\n", map_file, strerror(errno));
        exit(EXIT_FAILURE);
    }

    close(fd);
}
/* Linux 3.19 made a change in the handling of setgroups(2) and the
   'gid_map' file to address a security issue. The issue allowed
   *unprivileged* users to employ user namespaces in order to drop
   The upshot of the 3.19 changes is that in order to update the
   'gid_maps' file, use of the setgroups() system call in this
   user namespace must first be disabled by writing "deny" to one of
   the /proc/PID/setgroups files for this namespace.  That is the
   purpose of the following function. */

static void
proc_setgroups_write(pid_t child_pid, char *str)
{
    char setgroups_path[PATH_MAX];
    int fd;

    snprintf(setgroups_path, PATH_MAX, "/proc/%ld/setgroups",
            (long) child_pid);

    fd = open(setgroups_path, O_RDWR);
    if (fd == -1) {

        /* We may be on a system that doesn't support
           /proc/PID/setgroups. In that case, the file won't exist,
           and the system won't impose the restrictions that Linux 3.19
           added. That's fine: we don't need to do anything in order
           to permit 'gid_map' to be updated.

           However, if the error from open() was something other than
           the ENOENT error that is expected for that case,  let the
           user know. */

        if (errno != ENOENT)
            fprintf(stderr, "ERROR: open %s: %s\n", setgroups_path,
                strerror(errno));
        return;
    }

    if (write(fd, str, strlen(str)) == -1)
        fprintf(stderr, "ERROR: write %s: %s\n", setgroups_path,
            strerror(errno));

    close(fd);
}


int main(int argc, char **argv){
  puts("starting main");

  if (pipe(pipe_fd) == -1)
    errExit("pipe");

  child_stack = malloc(STACK_SIZE);
  if(!child_stack)
    errExit("malloc child stack");
  //TODO mprotect pages around stack!

  pid_t child_pid;
  child_pid = clone(&child_func, child_stack+STACK_SIZE, CLONE_NEWUSER|CLONE_NEWNS|CLONE_NEWPID|SIGCHLD, NULL);
  if (child_pid == -1)
    errExit("clone");

  const int MAP_BUF_SIZE = 100;
  char map_buf[MAP_BUF_SIZE];
  char map_path[PATH_MAX];

  snprintf(map_path, PATH_MAX, "/proc/%ld/uid_map",
          (long) child_pid);
  snprintf(map_buf, MAP_BUF_SIZE, "0 %ld 1", (long) getuid());
  update_map(map_buf, map_path);

  proc_setgroups_write(child_pid, "deny");

  snprintf(map_path, PATH_MAX, "/proc/%ld/gid_map",
          (long) child_pid);
  snprintf(map_buf, MAP_BUF_SIZE, "0 %ld 1", (long) getgid());
  update_map(map_buf, map_path);

  /* Close the write end of the pipe, to signal to the child that we
     have updated the UID and GID maps */
  close(pipe_fd[1]);



  if (waitpid(child_pid, NULL, 0) == -1)    /* Wait for child */
    errExit("waitpid");
  printf("child has terminated\n");

  exit(EXIT_SUCCESS);
}

