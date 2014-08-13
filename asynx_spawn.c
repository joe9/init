#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

int main (int argc, char *argu[]) {
	pid_t pid;
	int c;

/* 	(void) lines to avoid compiler warnings */
/* init.c:16:15: warning: unused parameter ‘argc’ [-Wunused-parameter] */
	(void) argc;

	pid = fork ();
	if (pid < 0) return EXIT_FAILURE;
	if (pid > 0) {
		waitpid (pid, &c, 0);
		return c;
	}
	pid = fork ();
	if (pid < 0) return 1;
	if (pid > 0) return 0;
	return execvp (argu[1], argu + 1);
}
