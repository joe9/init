
/* http://ieng9.ucsd.edu/~cs30x/rt_lt.rule.html */
/* http://www.unixwiz.net/techtips/reading-cdecl.html */
/*      "go right when you can, go left when you must"  */
/* rcshutdowncmd is an array of constant pointers to char */
/* else RUNLEVEL="0" /sbin/rc shutdown; exec /sbin/poweroff -f */
static char *const rcshutdowncmd[] =
   {"/bin/sh","-c","RUNLEVEL=0 /sbin/openrc shutdown; test -f /run/apcupsd/powerfail && exec halt -f || exec /sbin/poweroff -f",NULL};
static char *const rcrebootcmd[]   =
   {"/bin/sh","-c","RUNLEVEL=6 /sbin/openrc reboot; exec /sbin/reboot -f",NULL};
/* rcchildren is an array of an array of 4 constant pointers to char */
static char *const children[][4]    = {
   { "/bin/sh", "-c","exec /sbin/respawn -d 10 -- /etc/rc.tty 1",NULL },
   { "/bin/sh", "-c","exec /sbin/respawn -d 10 -- /etc/rc.tty 2",NULL },
   { "/bin/sh", "-c","exec /sbin/respawn -d 10 -- /etc/rc.tty 3",NULL },
   { "/bin/sh", "-c","exec /sbin/respawn -d 10 -- /etc/rc.tty 4",NULL },
   { "/bin/sh", "-c","exec /sbin/respawn -d 10 -- /etc/rc.tty 5",NULL },
   /* can get rid of "spawn" when rmon has the functionality to figure out
      which signals it can propogate to it's children. For example,
      most programs exit when HUP'ed (instead of reloading). If I
      start rc.X without spawn, it will exit when rmon propogates HUP
      to it's children */
   { "/bin/sh", "-c","exec /sbin/spawn -- /etc/rc.X",NULL },
/*    { "./respawn","--","./hello",NULL }, */
/*    { "/bin/sh", "-c","sleep 30; echo \"child 6\";",NULL }, */
 };
