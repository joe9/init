# simplified init version
VERSION = 0.0.1

# paths
PREFIX = /sbin
MANPREFIX = $(PREFIX)/share/man

CC = cc
LD = $(CC)
CPPFLAGS =
CFLAGS   = -Wextra -Wall -Wno-unused-result -Os $(CPPFLAGS)
LDFLAGS  = -static
