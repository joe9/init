
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

static pid_t rc_pid;
void signal_handler (int sig) {
   if (0 < rc_pid) kill(rc_pid,sig);
}
void hup_handler (int sig) {
   char pid_str[15];
   signal_handler(sig);
   sprintf(pid_str, "%d", rc_pid);
   execve ( "/sbin/init"
/*    execve ( "/home/j/dev/scripts/init/init" */
	  , (char * []){ "init",pid_str,0 }
          , (char * []){ 0 }
          );
}
int checkpid(int pid){
   pid_t ret = 0;
   ret = waitpid(pid,0,WNOHANG|WUNTRACED);
   if (0 == ret) return 1; /* child exists */
   /* check again */
   else if (-1 == ret && EINTR == errno) return checkpid (pid);

   /* child is dead */
   /* if (ret == pid) return 0; */
   /* no child */
   /* else if (-1 == ret && ECHILD == errno) return 0; */
   return 0;
}

/* using envp as linux kernel sets TERM environment variable which is
 * used by the rc init scripts to figure out if it is a colour
 * terminal. got the below line from runit-init.c */
int main (int argc, const char * const *argv, char * const *envp) {
    struct sigaction action, restart;
    sigset_t set;
    int savederrno;
    pid_t wait_pid;
	
    if (getpid () != 1) return EXIT_FAILURE;
	
    if (2 == argc && checkpid(atoi(argv[1]))) rc_pid = atoi(argv[1]);
    printf( "argc: %d, argv[0]: %s, argv[1]: %s pid: %d\n"
	    , argc,argv[0],argv[1],rc_pid);
	
	/* We need to block signals until we have forked */
	sigfillset (&set);
	sigprocmask (SIG_BLOCK, &set, 0);
	
/* 	http://www.cs.cityu.edu.hk/~lwang/fork */
	if (0 == rc_pid) rc_pid = fork ();
	if (0 > rc_pid) return EXIT_FAILURE;
	/* init doesn't want to get signals, hence not unblocking */
/* 	if (rc_pid > 0) for (;;) wait (&status); /\* orphans *\/ */
	if (0 < rc_pid) {
	    /* setup the signal handler to progorate signals to
	     * children */
	     action.sa_handler = signal_handler;
	     sigemptyset (&action.sa_mask);
	     action.sa_flags = 0;
	     sigaction (SIGUSR1, &action, NULL);
	     sigaction (SIGUSR2, &action, NULL);

	     restart.sa_handler = hup_handler;
	     sigemptyset (&restart.sa_mask);
	     restart.sa_flags = 0;
	     sigaction (SIGHUP, &restart, NULL);
	    /* Unmask only USR1 and USR2 signals */
	     sigemptyset (&set);
	     sigaddset (&set, SIGHUP);
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
/* 	execve ("/home/j/dev/scripts/init/hello", (char * []){ "hello", 0 }, envp); */
	execve ("/etc/rc", (char * []){ "rc", 0 }, envp);
	perror("init: execve /etc/rc");
	execve ("/bin/sh", (char * []){ "sh", 0 }, envp);
	savederrno = errno;
	perror("init: execve /bin/sh");
        _exit(savederrno);
}
