
#ifndef common_spawn_h__
#define common_spawn_h__

#include <sys/types.h>
#include <sys/wait.h>

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

void signal_handler (int sig);

int spawn (void (*hndlr)(int), int argc, char **argv, char * const *envp);

#endif  // common_spawn_h__
