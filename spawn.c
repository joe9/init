
/* can get rid of "spawn" when rmon has the functionality to figure out
   which signals it can propogate to it's children. For example,
   most programs exit when HUP'ed (instead of reloading). If I
   start rc.X without spawn, it will exit when rmon propogates HUP
   to it's children */
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

void sigreap     (char *name, pid_t * pid,int sig) ;
void sigrestart  (char *name, pid_t * pid,int sig) ;
void sigterm     (char *name, pid_t * pid,int sig) ;
void sigpropogate(char *name, pid_t * pid,int sig) ;

static struct {
   int signal;
   void (*handler)(char *name, pid_t * pid,int sig) ;
} sigmap[] = {
	{ SIGCHLD, sigreap    },
	{ SIGHUP,  sigrestart },
	{ SIGTERM, sigterm    },
};
pid_t spawn(char * argv[]) ;

int main (int argc, char * argv[], char * const *envp) {
   sigset_t set;
   size_t i = 0;
   int sig = 0;
   pid_t child_pid = 0;
   
   (void)envp;

   sigemptyset (&set);
   for (i = 0; i < LEN(sigmap); i++)
      sigaddset(&set,sigmap[i].signal);
   sigprocmask(SIG_SETMASK, &set, NULL);

   /* commands to test the below 
      make spawn args; ./spawn ; ./spawn -- ; ./spawn -- ./args ;
      ./spawn -- ./args "test"
   */
    if (2 == argc && 0 < atoi(argv[1])) {
       child_pid = atoi(argv[1]);
	printf( "argc: %d, argv[0]: %s, argv[1]: %s pid: %d\n"
	   , argc,argv[0],argv[1],child_pid);
    } else {
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
	}
	/* +1 to skip the -- */
	child_pid = spawn(&argv[i+1]);
    }

    while (0 < child_pid) {
       sigwait(&set, &sig);
       for (i = 0; i < LEN(sigmap); i++) {
	  if (sigmap[i].signal == sig) {
	     sigmap[i].handler(argv[0],&child_pid,sig);
	     break;
	  }
       }
    }
   return EXIT_SUCCESS;
}

pid_t spawn(char * argv[]) {
   pid_t rc_pid = 0;
   int savederrno = 0;
   sigset_t set, old;
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
	perror("spawn - execv failed:");
	execv ("/bin/sh", (char * []){ "sh", 0 });
	savederrno = errno;
	perror("init: execv /bin/sh");
	_exit(savederrno);
   }
   sigprocmask(SIG_SETMASK, &old, NULL);
   return rc_pid;
}

void sigreap (char *name, pid_t * child_pid,int sig) {
   pid_t wait_pid = 0;
   /* to avoid warning: unused parameter ‘sig’ [-Wunused-parameter] */
   (void)sig;
   (void)name;
   
   printf("sigreap called\n");
   while (0 < (wait_pid = waitpid(WAIT_ANY, NULL, WNOHANG))) {
      printf("%d exited\n",wait_pid);
      if (wait_pid == *child_pid) *child_pid = 0; 
   }
}
void sigterm (char *name, pid_t * pid,int sig) {
   /* to avoid warning: unused parameter ‘sig’ [-Wunused-parameter] */
   (void)name;
   printf("sigterm called\n");
   kill(*pid,sig);
}
void sigrestart (char *name, pid_t * rc_pid,int sig) {
   char pid_str[15];
   (void)sig;
   printf("sigrestart called\n");
   sprintf(pid_str, "%d", *rc_pid);
   execv(name, (char * []){ name,pid_str,0 });
}
