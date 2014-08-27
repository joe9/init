
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

static int restart = 0;
static struct {
   int signal;
   void (*handler)(int sig);
} sigmap[] = {
	{ SIGCHLD, sigreap      },
	{ SIGHUP,  sigrestart   },
	{ SIGTERM, sigpropogate },
};
void signal_handler (int sig);
pid_t spawn(char * argv[]) ;
static void *xmalloc(size_t);

static int parse_opt (int key, char *arg, struct argp_state *state);

int main (int argc, char * argv[], char * const *envp) {
   char pid_option[]   = "-p";
   char delay_option[] = "-d";
   char run_once_option[] = "-o";
   char pid_str[15] = {0};
   char delay_str[15] = {0};
   /* +2 for -p <pid>,
      +2 for -d <delay>,
      +1 for "-o" or "-r",
      +1 for trailing null character */
/*    char *args[argc+2+2+1+1] = {0}; */
   char **args;
   char **cmd; /* will be lesser than argc */
   sigset_t set;
   size_t i = 0, j = 0;
   /* do not sleep for the first respawn after restart */
   int sig = 0, sleeping = 1;
   char *sleepcmd[] = {"/usr/bin/sleep",&delay_str[0],NULL};
   
   (void)envp;
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
      { .name	= "respawn"
      , .key	= 'r'
      , .arg	= 0
      , .flags	= OPTION_ARG_OPTIONAL
      , .doc	= "Respawn"
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

   sigemptyset (&set);
   for (i = 0; i < LEN(sigmap); i++)
      sigaddset(&set,sigmap[i].signal);
   sigprocmask(SIG_SETMASK, &set, NULL);

   if (0 != argp_parse (&argp,argc, argv, ARGP_IN_ORDER,0,0)) {
      perror ("could not parse arguments");
   };

   /* used by sleepcmd command */
   sprintf(delay_str, "%d", delay);

   /* commands to test the below 
      make respawn args; ./respawn --run-once;
      ./respawn --run-once -- ; ./respawn --run-once -- ./args ;
      ./respawn --run-once -- ./args "test"
   */
   for (i = 0; i < (size_t)argc; i++) {
      printf("index: %d, pointer: %p, %s\n",
	     (int)i,(void *)&(argv[i]),argv[i]);
      if (0 == strcmp("--",argv[i])) break;
   }
   printf("argc: %d, no: %d\n",argc,(int)i);
   if (i == (size_t)argc) {
     printf("-- command not provided\n");
     return EXIT_FAILURE;
   } else if (i + 1 == (size_t)argc) {
     printf("command not provided\n");
     return EXIT_FAILURE;
   } else cmd = &argv[i+1]; /* to skip the -- */

   for (j=0;j < (size_t)argc-i; j++) {
      printf("cmd index: %d, pointer: %p, %s\n",
	     (int)i,(void *)&(cmd[j]),cmd[j]);
   }
   
   printf ( "delay: %d, respawn: %d, child-pid: %d\n"
	  , delay,respawn, child_pid );

   if (0 == child_pid) {
     child_pid = spawn(cmd);
     sleeping = 0;
   }
   while (1) {
      while (0 < child_pid && 0 == restart){
	 sigwait(&set, &sig);
	 for (i = 0; i < LEN(sigmap); i++) {
	    if (sigmap[i].signal == sig) {
	       sigmap[i].handler(sig);
	       break;
	    }
	 }
      }
      if (1 == restart || 0 == respawn) break;
      if (sleeping || 0 == delay) {
         child_pid = spawn(cmd);
         sleeping = 0;
      } else {
         child_pid = spawn(sleepcmd);
         sleeping = 1;
      }
   }

   if (1 == restart) {
	sprintf(pid_str, "%d", child_pid);
	sprintf(delay_str, "%d", delay);
	if (1 == respawn) run_once_option[1] = 'r';

	args = xmalloc(argc+6*sizeof(char *));

	for (i = 0; i < (size_t)(argc+6*sizeof(char *)); i++)
	   args[i] = 0;

	args[0] = argv[0];
	args[1] = (char *)&pid_option[0];
	args[2] = (char *)&pid_str[0];
	args[3] = (char *)&delay_option[0];
	args[4] = (char *)&delay_str[0];
	args[5] = (char *)&run_once_option[0];
	j = 6;

	for (i = 0; i < (size_t)argc; i++) {
	   printf("index: %d, pointer: %p, %s\n",
		 (int)i,(void *)&(argv[i]),argv[i]);
	   if (0 == strcmp("--",argv[i])) break;
	}
	while (i < (size_t)argc) {
	  args[j] = argv[i]; j++; i++;
	}

	for (i = 0; i < (size_t)argc; i++)
	  printf("index: %d, argv pointer: %p, %s\n",
		(int)i,(void *)&(argv[i]),argv[i]);
	i = 0;
	while (NULL != args[i]){
	    printf("index: %d, args pointer: %p, %s\n",
		  (int)i,(void *)&(args[i]),args[i]);
	    i++;
	}
	execv ( args[0], args );
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

pid_t spawn(char * argv[]) {
   pid_t rc_pid = 0;
   int savederrno = 0;
   sigset_t set, old;
/*    TODO move blocking code to the main loop */
   /* http://www.cs.cityu.edu.hk/~lwang/fork */
   /* block all signals before fork'ing */
   sigfillset (&set);
   sigprocmask (SIG_BLOCK, &set, &old);
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
   sigprocmask(SIG_SETMASK, &old, NULL);
   return rc_pid;
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
      case 'r': {
	 respawn = 1;
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
void *
xmalloc (size_t size)
{
  void *value = malloc (size);
  if (value == 0)
    perror ("virtual memory exhausted");
  return value;
}
