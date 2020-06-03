all: # Everything
all: build test

SYS_ROOT=/home/u/.cache/lambdanative/linux

GSC=$(SYS_ROOT)/bin/gsc
GSI=$(SYS_ROOT)/bin/gsi

GSCFLAGS= # -debug

GSCINCL=$(SYS_ROOT)/include

TEST_OPTIONS=-e '(define-cond-expand-feature test)'
# BUILD_OPTIONS=-e '(define-cond-expand-feature no-dirty-tagging)'

test:	# run test
test:	force
	$(GSI) $(TEST_OPTIONS) lwip-test.scm

LOCAL_DYNAMIC_LIBRARY_EXTENSION=o1

build: # build library
build:	lwip.$(LOCAL_DYNAMIC_LIBRARY_EXTENSION) lwip.c

SOURCES=lwip.scm

lwip.c: $(SOURCES)
	$(GSC) $(GSCFLAGS) -o $@ -c lwip.scm

lwip.$(LOCAL_DYNAMIC_LIBRARY_EXTENSION): $(SOURCES)
	$(GSC) -dynamic $(BUILD_OPTIONS) lwip.scm

todo: # find things to be done
	find . -type f \( -name '*.org' -o -name '[Mm]akefile' \) -exec egrep '(TODO|TBD)' '{}' ';' -print

missing: # find things to be done/found -- not in any queue, priority: TODO > TDB
	find . -type f \( -name '*.org' -o -name '[Mm]akefile' \) -exec egrep '(TODO|TBD):\?' '{}' ';' -print

clean:	# remove backupfiles (*~)
clean: force
	rm -f `find . -type f -name "*~" -print`
force:

help:	# help on possible targets
	@echo "documented make targets:"
	-@egrep "^[-A-Za-z0-9\._]+::?.*#" [Mm]akefile
