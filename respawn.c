
/* from http://git.suckless.org/sinit/plain/sinit.c */
#include "common_spawn.h"

volatile sig_atomic_t respawn = 1;

void drespawn_signal_handler (int sig) {
   respawn = 0;
   signal_handler(sig);
}

int main (int argc, char **argv, char * const *envp) {
   unsigned int secs = 0;

   while (1 == respawn) {
      spawn (drespawn_signal_handler, argc, argv, envp);
      sleep(secs);
   }
   return EXIT_SUCCESS;
}
