
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

static pid_t rc_pid     = 0;
static int hup_received = 0;
void signal_handler (int sig) {
   if (SIGHUP == sig) hup_received = 1;
   if (0 < rc_pid)   kill(rc_pid,sig);
}
void waitloop(char * const *envp) {
    sigset_t set;
    pid_t wait_pid;
    char pid_str[15];
    while ((wait_pid = wait(0))) { /* orphans */
       /* when there are no children, do not waste cpu cycles */
       if (-1 == wait_pid && ECHILD == errno) sleep(1);
       /* if the /etc/rc process ends, do not bother signalling it */
       else if (rc_pid == wait_pid) {
	   /* do not bother with signalling as rc
	    * child does not exist anymore */
	   rc_pid = 0;
	   sigfillset (&set);
	   sigdelset (&set, SIGHUP);
	   sigprocmask (SIG_BLOCK, &set, 0);
       }
       if (1 == hup_received) { 
	   sprintf(pid_str, "%d", rc_pid);
           /* execve ( "/home/j/dev/scripts/init/init" */
	   execve ( "/sbin/init"
		  , (char * []){ "init",pid_str,0 }
		  , envp );
       }
    }
}
/* using envp as linux kernel sets TERM environment variable which is
 * used by the rc init scripts to figure out if it is a colour
 * terminal. got the below line from runit-init.c */
int main (int argc, const char * const *argv, char * const *envp) {
    struct sigaction action;
    sigset_t set;
    int savederrno = 0;
	
    if (getpid () != 1) return EXIT_FAILURE;

    /* block all signals */
    sigfillset (&set);
    sigprocmask (SIG_BLOCK, &set, 0);

    /* setup and unblock the HUP signal handler */
    action.sa_handler = signal_handler;
    sigemptyset (&action.sa_mask);
    action.sa_flags = 0;
    sigaction (SIGHUP, &action, NULL);
    /* Unmask only HUP signal */
    sigemptyset (&set);
    sigaddset (&set, SIGHUP);
    sigprocmask (SIG_UNBLOCK, &set, 0);

    if (2 == argc && 0 < atoi(argv[1]))
       rc_pid = atoi(argv[1]);
    printf( "argc: %d, argv[0]: %s, argv[1]: %s pid: %d\n"
       , argc,argv[0],argv[1],rc_pid);
    if (2 == argc) waitloop(envp);
	
    /* We need to block signals until we have forked */
    sigfillset (&set);
    sigprocmask (SIG_BLOCK, &set, 0);
	
    /* http://www.cs.cityu.edu.hk/~lwang/fork */
    rc_pid = fork ();
    if (0 > rc_pid) return EXIT_FAILURE;
	/* init doesn't want to get signals, hence not unblocking */
    /* if (rc_pid > 0) for (;;) wait (&status); /\* orphans *\/ */
    if (0 < rc_pid) {
	/* setup the signal handler to progorate signals to
	 * children */
	 action.sa_handler = signal_handler;
	 sigemptyset (&action.sa_mask);
	 action.sa_flags = 0;
	 sigaction (SIGUSR1, &action, NULL);
	 sigaction (SIGUSR2, &action, NULL);
	 sigaction (SIGHUP, &action, NULL);
	/* Unmask only USR1 and USR2 signals */
	 sigemptyset (&set);
	 sigaddset (&set, SIGUSR1);
	 sigaddset (&set, SIGUSR2);
	 sigaddset (&set, SIGHUP);
	 sigprocmask (SIG_UNBLOCK, &set, 0);
	 waitloop(envp);
	 /* control never comes here */
    }
    /* for child */
    /* Unmask signals */
    sigprocmask (SIG_UNBLOCK, &set, 0);

    setsid ();
    setpgid (0, 0);
    /* return execve ("/etc/rc", (char * []){ "rc", 0 }, (char * []){ 0 }); */
    /* return execve ("/etc/rc", (char * []){ "rc", 0 }, envp); */
    /* execve ("/home/j/dev/scripts/init/hello", (char * []){ "hello", 0 }, envp); */
    execve ("/etc/rc", (char * []){ "rc", 0 }, envp);
    perror("init: execve /etc/rc");
    execve ("/bin/sh", (char * []){ "sh", 0 }, envp);
    savederrno = errno;
    perror("init: execve /bin/sh");
    _exit(savederrno);
}
