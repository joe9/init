
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

#define LEN(x) (sizeof (x) / sizeof *(x))

const char *argp_program_bug_address = "joe9mail@gmail.com";
const char *argp_program_version = "version 0.0.1";
static int delay = 10; /* default, sleep for 10 seconds between spawns */
volatile sig_atomic_t respawn = 1;
static pid_t child_pid = 0;

void sigreap       (int sig);
void sigrestart    (int sig);
void sigpropogate  (int sig);

static struct {
   int signal;
   void (*handler)(int sig);
} sigmap[] = {
	{ SIGCHLD, sigreap      },
	{ SIGHUP,  sigrestart   },
	{ SIGTERM, sigpropogate },
};
void signal_handler (int sig);
int spawn (char **argv, char * const *envp);
static int parse_opt (int key, char *arg, struct argp_state *state);

int main (int argc, char *argv[], char * const *envp) {
   char pid_str[15];
   char respawn_str[4];
   char delay_str[15];
   
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
      { .name	= "child-pid"
      , .key	= 'p'
      , .arg	= "PID"
      , .flags	= 0
      , .doc	= "Child pid"
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
      make svc args; ./svc --run-once;
      ./svc --run-once -- ; ./svc --run-once -- ./args ;
      ./svc --run-once -- ./args "test"
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

   while (0 == restart && 1 == respawn) {
	spawn (&(argv[i]), envp);
	sigemptyset (&set);
	for (i = 0; i < LEN(sigmap); i++) sigaddset(&set,sigmap[i].signal);
	sigprocmask(SIG_BLOCK, &set, NULL);
	
	while (0 < child_pid && 0 == restart){
	    sigwait(&set, &sig);
	    for (i = 0; i < LEN(sigmap); i++) {
		if (sigmap[i].signal == sig) {
		   sigmap[i].handler(argv[0],pids,sig);
		   break;
		}
	    }
	}
	if (0 == restart && 1 == respawn) sleep(delay);
   }
   if (1 == restart) {
     sprintf(pid_str, "%d", child_pid);
     if (0 == respawn) respawn_str="-o";
     else respawn_str = "";
     sprintf(delay_str, "%d", delay);
     for (i = 0; i < argc; i++) {
     /*       printf("index: %d, pointer: %p, %s\n", */
     /* 	     i,(void *)&(argv[i]),argv[i]); */
	   if (0 == strcmp("--",argv[i])) break;
}
     TODO could not figure out how to do the below without using malloc
     execv ( "/sbin/svc"
	   , (char * []){ "/sbin/svc"
		        , "-d"
		        , delay_str
		        , respawn_str
		        , "-p"
		        , pid_str
		        , &argv[i] This is wrong. This needs to be all argv's from i ... end
		        , 0
		        }
	   );
   }
   return EXIT_SUCCESS;
}

void signal_handler (int sig) {
   printf("signal_handler\n");
   psignal (sig, "signal_handler");
   respawn = 0;
   kill(child_pid,sig);
   if (SIGHUP == sig) {
      restart=1;
   }
}

pid_t spawn (char **argv, char * const *envp) {
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

        return child_pid;

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
      case 'p': {
	 child_pid = atoi(arg);
	 break;
      }
   }
   return 0;
}

void sigreap (int sig) {
   pid_t wait_pid = 0;
   /* to avoid warning: unused parameter ‘sig’ [-Wunused-parameter] */
   (void)sig;
   
   printf("sigreap called\n");
   while (0 < (wait_pid = waitpid(WAIT_ANY, NULL, WNOHANG))) {
      printf("%d exited\n",wait_pid);
      if (wait_pid == child_pid) child_pid = 0; 
   }
}
void sigpropogate (int sig) {
   printf("sigpropogate called\n");
   kill(child_pid,sig);
}
void sigrestart (int sig) {
   /* to avoid warning: unused parameter ‘sig’ [-Wunused-parameter] */
   (void)sig;
   printf("sigrestart called\n");
   /* should I be sending this to child? */
   restart = 1;
}
