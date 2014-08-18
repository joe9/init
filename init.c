
/* got the below code from 
   http://ewontfix.com/14/
   https://github.com/strake/init/blob/master/init.c
*/
/* TODO add more comments based on openrc /sbin/rc source code from
 * run_program () function */
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/wait.h>

static pid_t rc_pid;
void signal_handler (int sig) {
   if (0 < rc_pid) kill(rc_pid,sig);
}

/* using envp as linux kernel sets TERM environment variable which is
 * used by the rc init scripts to figure out if it is a colour
 * terminal. got the below line from runit-init.c */
int main (int argc, const char * const *argv, char * const *envp) {

    struct sigaction action;
    sigset_t set;
    int savederrno;
    pid_t wait_pid;
	
/* 	(void) lines to avoid compiler warnings */
/* init.c:16:15: warning: unused parameter ‘argc’ [-Wunused-parameter] */
/* init.c:16:41: warning: unused parameter ‘argv’ [-Wunused-parameter] */
	(void) argc;
	(void) argv;
	
/* 	if (getpid () != 1) return EXIT_FAILURE; */
	
	/* We need to block signals until we have forked */
	sigfillset (&set);
	sigprocmask (SIG_BLOCK, &set, 0);
	
/* 	http://www.cs.cityu.edu.hk/~lwang/fork */
	rc_pid = fork ();
	if (rc_pid < 0) return EXIT_FAILURE;
	/* init doesn't want to get signals, hence not unblocking */
/* 	if (rc_pid > 0) for (;;) wait (&status); /\* orphans *\/ */
	if (rc_pid > 0) {
	    /* setup the signal handler to progorate signals to
	     * children */
	     action.sa_handler = signal_handler;
	     sigemptyset (&action.sa_mask);
	     action.sa_flags = 0;
	     sigaction (SIGUSR1, &action, NULL);
	     sigaction (SIGUSR2, &action, NULL); 
	    /* Unmask only USR1 and INT signals */
	     sigemptyset (&set);
	     sigaddset (&set, SIGUSR1);
	     sigaddset (&set, SIGUSR2);
	     sigprocmask (SIG_UNBLOCK, &set, 0);
	     while ((wait_pid = wait(0))) { /* orphans */
		/* when there are no children, do not waste cpu cycles */
		if (-1 == wait_pid && ECHILD == errno) sleep(1);
		/* if the /etc/rc process ends, do not bother signalling it */
		else if (rc_pid == wait_pid) {
		    /* do not bother with signalling as rc
		     * child does not exist anymore */
		    rc_pid = 0;
		    sigfillset (&set);
		    sigprocmask (SIG_BLOCK, &set, 0);
		}
	     }
	}
	
	/* Unmask signals */
	sigprocmask (SIG_UNBLOCK, &set, 0);
	
	setsid ();
	setpgid (0, 0);
/* 	return execve ("/etc/rc", (char * []){ "rc", 0 }, (char * []){ 0 }); */
/* 	return execve ("/etc/rc", (char * []){ "rc", 0 }, envp); */
	execve ("/etc/rc", (char * []){ "rc", 0 }, envp);
	perror("init: execve /etc/rc");
	execve ("/bin/sh", (char * []){ "sh", 0 }, envp);
	savederrno = errno;
	perror("init: execve /bin/sh");
        _exit(savederrno);
}
