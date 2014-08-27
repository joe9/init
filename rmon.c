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

#include "rmon.h"

#define LEN(x) (sizeof (x) / sizeof *(x))

void sigshutdown   (char * name, pid_t pids[], int sig);
void sigreboot     (char * name, pid_t pids[], int sig);
void sigreap       (char * name, pid_t pids[], int sig);
void sigrestart    (char * name, pid_t pids[], int sig);
void sigpropogate  (char * name, pid_t pids[], int sig);

static struct {
   int signal;
   void (*handler)(char * name, pid_t pids[], int sig);
} sigmap[] = {
	{ SIGUSR1, sigshutdown  },
	{ SIGUSR2, sigreboot    },
	{ SIGCHLD, sigreap      },
	{ SIGHUP,  sigrestart   },
	{ SIGTERM, sigpropogate },
};

pid_t spawn(char *const argv[]) {
    pid_t rc_pid = 0;
    int savederrno = 0;
    sigset_t set;
    /* http://www.cs.cityu.edu.hk/~lwang/fork */
    /* block all signals before fork'ing */
    sigfillset (&set);
    sigprocmask (SIG_BLOCK, &set, 0);
    rc_pid = fork();
    if (0 > rc_pid) perror("fork");
    else if (rc_pid == 0) { /* child */
	sigprocmask(SIG_UNBLOCK, &set, NULL);
	/* setsid (); */
	/* setpgid (0, 0); */
	execv (argv[0],argv);
	perror("pmon: execv /etc/pmon");
	execv ("/bin/sh", (char * []){ "sh", 0 });
	savederrno = errno;
	perror("init: execv /bin/sh");
	_exit(savederrno);
    }
    /* unblock all signals after fork'ing */
    sigprocmask (SIG_UNBLOCK, &set, 0);
    return rc_pid;
}
/* using envp as linux kernel sets TERM environment variable which is
 * used by the rc init scripts to figure out if it is a colour
 * terminal. got the below line from runit-init.c */
int main (int argc, char * argv[], char * const *envp) {
    int sig = 0;
    size_t i = 0;
    sigset_t set;
    pid_t pids[LEN(children)];
    (void)envp;
	
    for (i = 0; i < LEN(children); i++) pids[i] = 0;

    printf( "argc: %d\n", argc);
    for (i = 0; i < (size_t)argc; i++)
       printf("argv[%d]: %s",(int)i,argv[i]);
    printf("\n");

    if (1 == argc) {
       for (i = 0; i < LEN(children); i++) pids[i] = spawn(children[i]);
    } else {
       for (i = 0; i < LEN(children) && i < (size_t)(argc-1); i++)
          if (0 < atoi(argv[i+1])) pids[i] = atoi(argv[i+1]);
    }

    printf("pids: ");
    for (i = 0; i < LEN(children); i++)
       printf("%d: %d, ",(int)i,pids[i]);
    printf("\n");
	
    sigemptyset (&set);
    for (i = 0; i < LEN(sigmap); i++) sigaddset(&set,sigmap[i].signal);
    sigprocmask(SIG_BLOCK, &set, NULL);

    while (1) {
	sigwait(&set, &sig);
	for (i = 0; i < LEN(sigmap); i++) {
	    if (sigmap[i].signal == sig) {
	       sigmap[i].handler(argv[0],pids,sig);
	       break;
	    }
	}
	/* exit when all pids == 0 */
	for (i = 0; i < LEN(children) && 0 == pids[i]; i++) ;
	if (i >= LEN(children)) break;
    }
    return EXIT_SUCCESS;
}

void sigpropogate (char * name, pid_t pids[], int sig) {
   /* to avoid warning: unused parameter ‘sig’ [-Wunused-parameter] */
   (void)name;
   size_t i = 0;
   printf("sigpropogate called\n");
   for (i = 0; i < LEN(children); i++) if (0 < pids[i]) kill(pids[i],sig);
}
void sigreap (char * name, pid_t pids[], int sig) {
   pid_t wait_pid = 0;
   size_t i = 0;
   /* to avoid warning: unused parameter ‘sig’ [-Wunused-parameter] */
   (void)sig;
   (void)name;
   
   printf("sigreap called\n");
   while (0 < (wait_pid = waitpid(WAIT_ANY, NULL, WNOHANG))) {
      printf("%d exited\n",wait_pid);
      for (i = 0; i < LEN(children); i++)
	 if (wait_pid == pids[i]) { pids[i] = 0; break; }
   }
}
void sigshutdown (char * name, pid_t pids[], int sig) {
   (void)sig;
   printf("sigshutdown called\n");
   sigpropogate(name,pids,SIGTERM);
   execv (rcshutdowncmd[0], rcshutdowncmd);
}
void sigreboot (char * name, pid_t pids[], int sig) {
   (void)sig;
   printf("sigreboot called\n");
   sigpropogate(name,pids,SIGTERM);
   execv (rcrebootcmd[0], rcrebootcmd);
}
void sigrestart (char * name, pid_t pids[], int sig) {
   char *argv[LEN(children)+2];
   char pid_str[LEN(children)][15];
   size_t i = 0;

   printf("sigrestart called\n");
   sigpropogate(name,pids,sig);
   for (i = 0; i < LEN(children); i++) sprintf(pid_str[i], "%d", pids[i]);

   argv[0] = name;
   for (i = 0; i < LEN(children); i++) argv[i+1] = (char *)&(pid_str[i]);
   argv[i+1] = 0;

   execv (argv[0], argv);
}
