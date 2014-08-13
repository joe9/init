
/* got the below code from 
   http://ewontfix.com/14/
   https://github.com/strake/init/blob/master/init.c
*/
/* TODO add more comments based on openrc /sbin/rc source code from
 * run_program () function */
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

/* int main () { */
/* using envp as linux kernel sets TERM environment variable which is
 * used by the rc init scripts to figure out if it is a colour
 * terminal. got the below line from runit-init.c */
int main (int argc, const char * const *argv, char * const *envp) {

	sigset_t set;
	pid_t pid;
	int status;
	
/* 	(void) lines to avoid compiler warnings */
/* init.c:16:15: warning: unused parameter ‘argc’ [-Wunused-parameter] */
/* init.c:16:41: warning: unused parameter ‘argv’ [-Wunused-parameter] */
	(void) argc;
	(void) argv;
	
	if (getpid () != 1) return EXIT_FAILURE;
	
	/* We need to block signals until we have forked */
	sigfillset (&set);
	sigprocmask (SIG_BLOCK, &set, 0);
	
/* 	http://www.cs.cityu.edu.hk/~lwang/fork */
	pid = fork ();
	if (pid < 0) return EXIT_FAILURE;
	if (pid > 0) for (;;) wait (&status); /* orphans */
	
	/* Unmask signals */
	sigprocmask (SIG_UNBLOCK, &set, 0);
	
	setsid ();
	setpgid (0, 0);
/* 	return execve ("/etc/rc", (char * []){ "rc", 0 }, (char * []){ 0 }); */
	return execve ("/etc/rc", (char * []){ "rc", 0 }, envp);
}
