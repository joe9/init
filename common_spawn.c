
/* from http://git.suckless.org/sinit/plain/sinit.c */
#include "common_spawn.h"

static pid_t child_pid;

void signal_handler (int sig) {
    printf("signal_handler\n");
    psignal (sig, "signal_handler");
    kill(child_pid,sig);
}

int spawn (void (*hndlr)(int), int argc, char **argv, char * const *envp)
{
    struct sigaction action;
    static sigset_t set;

/* (void) lines to avoid compiler warnings */
/* init.c:16:15: warning: unused parameter ‘argc’ [-Wunused-parameter] */
    (void) argc;

    /* We need to block signals until we have forked */
    sigfillset(&set);
    sigprocmask(SIG_BLOCK, &set, NULL);

    child_pid = fork();
    if (child_pid < 0) {
       perror("fork");
       return EXIT_FAILURE;
    } else if (child_pid > 0) { /* parent */

       /* setup the signal handler to progorate signals to
	* children */
	action.sa_handler = hndlr;
	sigemptyset (&action.sa_mask);
	action.sa_flags = 0;
	sigaction (SIGHUP, &action, NULL);
	sigaction (SIGINT, &action, NULL);
	sigaction (SIGTERM, &action, NULL);

	/* unmask signals */
	sigprocmask (SIG_UNBLOCK, &set, 0);

	/* wait as long as any child is there */
	/* reap children */
	while (-1 != wait(NULL) || errno == EINTR) ;

	if (ECHILD == errno) {
	   printf("All children exited\n");
	   return EXIT_SUCCESS;
	} else if (EINVAL == errno) {
	    perror("wait");
	    return EXIT_FAILURE;
	} /* ignore EINTR */ 

    }
    /* child */
/*     else if (child_pid == 0) { */
    /* Unmask signals */
    sigprocmask(SIG_UNBLOCK, &set, NULL);
/*  setsid(); */
    return execve (argv[1], argv + 1, envp);
/*  perror("execve"); */
/*  _exit(EXIT_FAILURE); */
}
