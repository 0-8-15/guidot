# (C) 2020 JFW

SYS_HOSTPREFIX?=/home/u/.cache/lambdanative/linux

SYS_ROOT?=$(SYS_HOSTPREFIX)

GSC?=$(SYS_ROOT)/bin/gsc
GSI=$(SYS_ROOT)/bin/gsi

GSCFLAGS?= # -keep-c #-debug

GSCINCL?=$(SYS_ROOT)/include

all:	# All derived files
all: ALL_DERIVED_FILES

ALL_DERIVED_FILES=match.o1 match.c match.o # akwmatch.scm
ALL_DERIVED_FILES: $(ALL_DERIVED_FILES)

GAMBIT_C_SOURCE_DIR=include/gambitc

install: all install-match.c
install-host: install install-compile-time

$(SYS_ROOT)/$(GAMBIT_C_SOURCE_DIR):
	@test -d $(SYS_ROOT)/$(GAMBIT_C_SOURCE_DIR) || mkdir $(SYS_ROOT)/$(GAMBIT_C_SOURCE_DIR)

install-gambit-c-generic-dir: $(SYS_ROOT)/$(GAMBIT_C_SOURCE_DIR)

$(SYS_ROOT)/$(GAMBIT_C_SOURCE_DIR)/match.c: match.c
	cp match.c $(SYS_ROOT)/$(GAMBIT_C_SOURCE_DIR)/

install-match.c: install-gambit-c-generic-dir $(SYS_ROOT)/$(GAMBIT_C_SOURCE_DIR)/match.c

install-compile-time: $(SYS_HOSTPREFIX)/lib/match.o1

$(SYS_HOSTPREFIX)/lib/match.o1: match.o1
	cp $< $@

# END of configurable section

PLAIN_SRC=source/match-match-expanders.scm source/match-match-syntax.scm

match.c:	# generic (platform independent) gambit-c souce
match.c: $(PLAIN_SRC) source/gambit-match.scm
	$(GSC) $(GSCFLAGS) -o $@ -c source/gambit-match.scm

match.o:	# TARGET platform object
match.o: match.c
	$(GSC) $(GSCFLAGS) -o $@ -obj match.c

match.o1:	# HOST platform loadable object (for compile time)
match.o1: $(PLAIN_SRC) source/gambit-match.scm
	$(GSC) $(GSCFLAGS) -o $@ -dynamic source/gambit-match.scm

akwmatch.scm:	# lambdanative `akwmatch` module source
akwmatch.scm: $(PLAIN_SRC) source/gambit-match.scm scripts/make-ln-akwmatch.scm \
  $(GENERATED)
	@-rm -f $@
	$(GSI) scripts/make-ln-akwmatch.scm

test: # test it
test:
	$(GSI) source/gambit-match.scm -

# Appendix: Generic Targets

clean:	# remove backupfiles (*~) and $ALL_DERIVED_FILES
clean: force
	rm -f `find . -type f -name "*~" -print` $(ALL_DERIVED_FILES)
force:

help:	# help on possible targets
	-@egrep "^[-A-Za-z0-9\._]+::?.*#" [Mm]akefile
