#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
/* #include <stdio.h> */

int main (int argc, char *argu[]) {
	pid_t pid;
	int c;

/* 	(void) lines to avoid compiler warnings */
/* init.c:16:15: warning: unused parameter ‘argc’ [-Wunused-parameter] */
	(void) argc;

/* 	http://www.cs.cityu.edu.hk/~lwang/fork */
/* 	printf("first fork\n"); */
	pid = fork ();
	if (pid < 0) return EXIT_FAILURE;
	if (pid > 0) {
		waitpid (pid, &c, 0);
/* 		printf("parent exiting\n"); */
		return c;
	}
/* 	printf("second fork\n"); */
	pid = fork ();
	if (pid < 0) return 1;
	if (pid > 0) return 0;
/* 	printf("after double fork, starting %s\n",argu[1]); */
	return execvp (argu[1], argu + 1);
}
