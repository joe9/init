
/* got the below code from 
   http://ewontfix.com/14/
   https://github.com/strake/init/blob/master/init.c
*/
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>

int main () {
	sigset_t set;
	pid_t pid;
	int status;
	
	if (getpid () != 1) return EXIT_FAILURE;
	
	sigfillset (&set);
	sigprocmask (SIG_BLOCK, &set, 0);
	
	pid = fork ();
	if (pid < 0) return EXIT_FAILURE;
	if (pid > 0) for (;;) wait (&status); /* orphans */
	
	sigprocmask (SIG_UNBLOCK, &set, 0);
	
	setsid ();
	setpgid (0, 0);
	return execve ("/etc/rc", (char * []){ "rc", 0 }, (char * []){ 0 });
}
