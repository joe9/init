
/* from http://git.suckless.org/sinit/plain/sinit.c */
#include "common_spawn.h"

int main (int argc, char **argv, char * const *envp) {
   return spawn (signal_handler, argc, argv, envp);
}
