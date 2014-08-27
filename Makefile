include config.mk

.POSIX:
.SUFFIXES: .c .o

SRC = init.c rmon.c respawn.c
SCRIPTS = rc rc.tty

OBJ = $(SRC:.c=.o)
BIN = $(SRC:.c=)

all: options rmonh bin scripts

options:
	@echo init build options:
	@echo "CFLAGS   = $(CFLAGS)"
	@echo "LDFLAGS  = $(LDFLAGS)"
	@echo "CC       = $(CC)"

# respawn: respawn.hs respawn.cabal
# 	@cabal configure
# 	@cabal build

bin: $(BIN)

scripts: $(SCRIPTS)

$(OBJ): config.mk

.o:
	@echo LD $@
	@$(LD) -o $@ $< $(LDFLAGS)

.c.o:
	@echo CC $<
	@$(CC) -c -o $@ $< $(CFLAGS)

dist: clean
	@echo creating dist tarball
	@mkdir -p init-$(VERSION)
	@cp Makefile README config.mk init.8 init.c \
	 	respawn.c asynx_spawn.c rc rc.tty rc.X \
		init-$(VERSION)
	@tar -cf init-$(VERSION).tar init-$(VERSION)
	@gzip init-$(VERSION).tar
	@rm -rf init-$(VERSION)

installrc.X:
	@echo installing rc.X to $(DESTDIR)$(ETCDIR)
	@mkdir -p $(DESTDIR)$(ETCDIR)
	@cp -f rc.X $(DESTDIR)$(ETCDIR)
	@chmod 755 $(DESTDIR)$(ETCDIR)/rc.X

x: rmonx options rmon rc.X

rmonx:
	@cp -f rmon.x.h rmon.h

rmonh:
	@cp -f rmon.nox.h rmon.h

install: all
	@echo installing executable to $(DESTDIR)$(SBINDIR)
	@mkdir -p $(DESTDIR)$(SBINDIR)
	@cp -f $(BIN) $(DESTDIR)$(SBINDIR)
	@chmod 755 $(DESTDIR)$(SBINDIR)/$(BIN)
	@echo installing scripts to $(DESTDIR)$(ETCDIR)
	@mkdir -p $(DESTDIR)$(ETCDIR)
	@cp -f $(SCRIPTS) $(DESTDIR)$(ETCDIR)
	@chmod 755 $(DESTDIR)$(ETCDIR)/$(SCRIPTS)
	@echo installing manual page to $(DESTDIR)$(MANDIR)/man8
	@mkdir -p $(DESTDIR)$(MANDIR)/man8
	@sed "s/VERSION/$(VERSION)/g" < init.8 > $(DESTDIR)$(MANDIR)/man8/init.8
	@chmod 644 $(DESTDIR)$(MANDIR)/man8/init.8
# 	@cabal install

uninstall:
	@echo removing executable from $(DESTDIR)$(SBINDIR)
	@cd $(DESTDIR)$(SBINDIR) && rm -f $(BIN)
	@echo removing scripts from $(DESTDIR)$(ETCDIR)
	@cd $(DESTDIR)$(ETCDIR) && rm -f $(SCRIPTS)
	@echo removing manual page from $(DESTDIR)$(MANDIR)/man8
	@rm -f $(DESTDIR)$(MANDIR)/man8/init.8

clean:
	@echo cleaning
	@rm -f $(BIN) $(OBJ) init-$(VERSION).tar.gz
# 	@cabal clean
