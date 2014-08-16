
/* from http://git.suckless.org/sinit/plain/sinit.c */
#include <argp.h>

#include <sys/types.h>
#include <sys/wait.h>

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

void signal_handler (int sig);

int spawn (void (*hndlr)(int), int argc, char **argv, char * const *envp);

const char *argp_program_bug_address = "joe9mail@gmail.com";
const char *argp_program_version = "version 0.0.1";

static int delay = 0;
static int respawn = 1;

static int
parse_opt (int key, char *arg, struct argp_state *state) {
   switch (key) {
      case 'd': {
	 delay = atoi(arg);
	 break;
      }
      case 'o': {
	 respawn = 0;
	 break;
      }
   }
   return 0;
}

volatile sig_atomic_t respawn = 1;

void signal_handler (int sig) {
   printf("signal_handler\n");
   psignal (sig, "signal_handler");
   respawn = 0;
   kill(child_pid,sig);
}

int main (int argc, char *argv[], char * const *envp) {

   struct argp_option options[] = {
      {"delay",'d',"SECONDS",OPTION_ARG_OPTIONAL,"Delay period between spawns"},
      {"run-once",'o',0,OPTION_ARG_OPTIONAL,"Run once. Do not respawn"},
      {0}
    };

   struct argp argp = {options, parse_opt, "-- command [command arguments]" };

   if (0 != argp_parse (&argp,argc, argv, ARGP_IN_ORDER,0,0)) {
      perror ("could not parse arguments");
   };

   printf ("delay: %d, respawn: %d\n", delay,respawn);

/*    while (1 == respawn) { */
/*       spawn (drespawn_signal_handler, argc, argv, envp); */
/*       sleep(secs); */
/*    } */
   return EXIT_SUCCESS;
}

static pid_t child_pid;

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
