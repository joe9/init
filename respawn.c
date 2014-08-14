
/* added comments to code from 
   https://github.com/strake/init/blob/master/init.c
*/
#include <unistd.h>
#include <stdlib.h>
/* #include <stdio.h> */
#include <sys/wait.h>

int main (int argc, char *argu[], char * const *envp) {
	pid_t pid;
	int c;
	
/* 	(void) lines to avoid compiler warnings */
/* init.c:16:15: warning: unused parameter ‘argc’ [-Wunused-parameter] */
	(void) argc;

	for (;;) {
/* 	   printf ("spawning\n"); */
/* 	http://www.cs.cityu.edu.hk/~lwang/fork */
	   pid = fork ();
	   if (pid < 0) return EXIT_FAILURE;
	   if (pid > 0) waitpid (pid, &c, 0);
	   /* setsid makes the spawned process the child of
	    * init. I want to see the spawned process as the
	    * child of the respawn process as it is easier to kill
	    * the respawning process if the process misbehaves */ 
	   /* 		else return (setsid () < 0 || execv (argu[1], argu + 1)); */
/* 	   else return (execv (argu[1], argu + 1)); */
	   else return execve (argu[1], argu + 1, envp);
	}
}
