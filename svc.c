
/* from http://git.suckless.org/sinit/plain/sinit.c */
#include <argp.h>

#include <sys/types.h>
#include <sys/wait.h>

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

const char *argp_program_bug_address = "joe9mail@gmail.com";
const char *argp_program_version = "version 0.0.1";
static int delay = 10; /* default, sleep for 10 seconds between spawns */
volatile sig_atomic_t respawn = 1;

void signal_handler (int sig);
int spawn (char **argv, char * const *envp);
static int parse_opt (int key, char *arg, struct argp_state *state);

int main (int argc, char *argv[], char * const *envp) {

   struct argp_option options[] = {
      { .name	= "delay"
      , .key	= 'd'
      , .arg	= "SECONDS"
      , .flags	= 0
      , .doc	= "Delay period between spawns"
      },
      { .name	= "run-once"
      , .key	= 'o'
      , .arg	= 0
      , .flags	= OPTION_ARG_OPTIONAL
      , .doc	= "Run once. Do not respawn."
      },
      {0}
    };
   struct argp argp = {
      .options	= options
    , .parser	= parse_opt
    , .args_doc	= "-- command [command arguments]"
    };

   if (0 != argp_parse (&argp,argc, argv, ARGP_IN_ORDER,0,0)) {
      perror ("could not parse arguments");
   };

   /* commands to test the below 
      make respawn args; ./respawn --run-once;
      ./respawn --run-once -- ; ./respawn --run-once -- ./args ;
      ./respawn --run-once -- ./args "test"
   */
   int i = 0;
   for (i = 0; i < argc; i++) {
/*       printf("index: %d, pointer: %p, %s\n", */
/* 	     i,(void *)&(argv[i]),argv[i]); */
      if (0 == strcmp("--",argv[i])) break;
   }
/*    printf("argc: %d, no: %d\n",argc,i); */
   if (i == argc) {
     printf("-- command not provided\n");
     return EXIT_FAILURE;
   } else if (i + 1 == argc) {
     printf("command not provided\n");
     return EXIT_FAILURE;
   } else i++; /* to skip the -- */
   
/*    printf ("delay: %d, respawn: %d\n", delay,respawn); */

   spawn (&(argv[i]), envp);
   while (1 == respawn) {
      sleep(delay);
      spawn (&(argv[i]), envp);
   }
   return EXIT_SUCCESS;
}

static pid_t child_pid;

void signal_handler (int sig) {
   printf("signal_handler\n");
   psignal (sig, "signal_handler");
   respawn = 0;
   kill(child_pid,sig);
}

int spawn (char **argv, char * const *envp) {
    struct sigaction action;
    static sigset_t set;
    int savederrno;
    pid_t wait_pid;

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
	action.sa_handler = signal_handler;
	sigemptyset (&action.sa_mask);
	action.sa_flags = 0;
	sigaction (SIGHUP, &action, NULL);
	sigaction (SIGINT, &action, NULL);
	sigaction (SIGTERM, &action, NULL);

	/* unmask signals */
	sigprocmask (SIG_UNBLOCK, &set, 0);

	/* wait as long as any child is there */
	/* reap children */
	/* ignore other pids, EINTR and EINVAL also */
	while ((wait_pid = wait(0))) if (-1 == wait_pid && ECHILD == errno) break;

	if (ECHILD == errno) {
	   printf("All children exited\n");
	   return EXIT_SUCCESS;
	}
    }
    /* else if (child_pid == 0) { */ /* child */
    /* Unmask signals */
    sigprocmask(SIG_UNBLOCK, &set, NULL);
/*  setsid(); */
/*     int i = 0; */
/*     while (0 != argv[i]) { */
/*        printf("spawn: %d: %s\n",i,argv[i]); */
/*        i++; */
/*     } */
    execve (argv[0], argv, envp);
    savederrno = errno;
    perror("respawn: error on execve");
    perror(argv[0]);
    _exit(savederrno);
}

static int parse_opt (int key, char *arg, struct argp_state *state) {
/* (void) lines to avoid compiler warnings */
/* init.c:16:15: warning: unused parameter ‘argc’ [-Wunused-parameter] */
    (void) state;

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
