#include <sys/types.h>
#include <sys/wait.h>

idtype_t waitpid_P_PID = P_PID;

int waitpidoptions_WNOHANG = WNOHANG;
int waitpidoptions_WUNTRACED = WUNTRACED;
int waitpidoptions_WCONTINUED = WCONTINUED;
int waitpidoptions_WEXITED = WEXITED;
int waitpidoptions_WSTOPPED = WSTOPPED;
int waitpidoptions_WNOWAIT = WNOWAIT;

