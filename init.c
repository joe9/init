/* got the below code from
   http://ewontfix.com/14/
   https://github.com/strake/init/blob/master/init.c */
/* TODO add more comments based on openrc /sbin/rc source code from
 * run_program () function */
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/wait.h>

static char *const rcinitcmd[] = { "/etc/rc", NULL };
/* static char *const rcinitcmd[] = { "./hello",NULL }; */
#define LEN(x) (sizeof (x) / sizeof *(x))

pid_t spawn(char *const argv[], char * const *envp);
void sigpropogate (char* name, pid_t *rc_pid,int sig);
void sigreap      (char* name, pid_t *rc_pid,int sig);
void sigrestart   (char* name, pid_t *rc_pid,int sig);

static struct {
   int signal;
   void (*handler)(char * name, pid_t *rc_pid, int sig);
} sigmap[] = {
	{ SIGUSR1, sigpropogate },
	{ SIGUSR2, sigpropogate },
	{ SIGCHLD, sigreap      },
	{ SIGHUP,  sigrestart   }
};

/* using envp as linux kernel sets TERM environment variable which is
 * used by the rc init scripts to figure out if it is a colour
 * terminal. got the below line from runit-init.c */
int main (int argc, char * argv[], char * const *envp) {
    sigset_t set;
    int sig = 0;
    size_t i = 0;
    pid_t rc_pid = 0;
	
    if (getpid () != 1) return EXIT_FAILURE;
    chdir("/");

    if (2 == argc && 0 < atoi(argv[1])) rc_pid = atoi(argv[1]);
    printf( "argc: %d, argv[0]: %s, argv[1]: %s pid: %d\n"
       , argc,argv[0],argv[1],rc_pid);
    if (1 == argc) rc_pid = spawn(rcinitcmd,envp);
	
    sigfillset (&set);
    sigprocmask (SIG_BLOCK, &set, 0);

    while (1) {
	sigwait(&set, &sig);
	for (i = 0; i < LEN(sigmap); i++) {
	    if (sigmap[i].signal == sig) {
	       sigmap[i].handler(argv[0],&rc_pid,sig);
	       break;
	    }
	}
    }
    /* not reachable */
    return EXIT_SUCCESS;
}

void sigpropogate (char* name, pid_t *rc_pid,int sig) {
   /* to avoid warning: unused parameter ‘sig’ [-Wunused-parameter] */
   (void)name;
   if (0 < *rc_pid) kill(*rc_pid,sig);
}
void sigreap (char* name, pid_t *rc_pid,int sig) {
   pid_t wait_pid = 0;
   /* to avoid warning: unused parameter ‘sig’ [-Wunused-parameter] */
   (void)name;
   (void)sig;
   /* printf("sigreap called\n"); */
   while (0 < (wait_pid = waitpid(WAIT_ANY, NULL, WNOHANG))) {
	if (*rc_pid == wait_pid) {
	    /* do not bother with signalling as rc
	     * child does not exist anymore */
	    *rc_pid = 0;
	}
   }
}
void sigrestart (char *name, pid_t *rc_pid,int sig) {
   char pid_str[15];
   if (0 < *rc_pid)   kill(*rc_pid,sig);
   sprintf(pid_str, "%d", *rc_pid);
   execv(name, (char * []){ name,pid_str,0 });
}
pid_t spawn(char *const argv[], char * const *envp) {
    pid_t rc_pid = 0;
    int savederrno = 0;
    sigset_t set;
    /* http://www.cs.cityu.edu.hk/~lwang/fork */
    rc_pid = fork();
    if (0 > rc_pid) perror("fork");
    else if (rc_pid == 0) { /* child */
	sigfillset(&set);
	sigprocmask(SIG_UNBLOCK, &set, NULL);
	setsid ();
	setpgid (0, 0);
	execve (argv[0],argv,envp);
	perror("init: execve /etc/rc");
	execve ("/bin/sh", (char * []){ "sh", 0 }, envp);
	savederrno = errno;
	perror("init: execve /bin/sh");
	_exit(savederrno);
    }
    return rc_pid;
}
