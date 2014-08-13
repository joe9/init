# simplified init version
VERSION = 0.0.1

# paths
PREFIX  =
MANDIR  = ${PREFIX}/share/man
ETCDIR  = ${PREFIX}/etc
SBINDIR = ${PREFIX}/sbin

CC = cc
LD = $(CC)
CPPFLAGS =
CFLAGS   = -Wextra -Wall -Wno-unused-result -Os $(CPPFLAGS)
LDFLAGS  = -static
