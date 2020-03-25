all: # Everything
all: test

GAMBINDIR=$(HOME)/.cache/lambdanative/linux/bin

TEST_OPTIONS=-e '(define-cond-expand-feature test)'
# BUILD_OPTIONS=-e '(define-cond-expand-feature no-dirty-tagging)'

test:	# run test
test:	force
	$(GAMBINDIR)/gsi $(TEST_OPTIONS) likely.scm

LOCAL_DYNAMIC_LIBRARY_EXTENSION=o1

build: # build library
build:	likely.$(LOCAL_DYNAMIC_LIBRARY_EXTENSION)

SOURCES=likely.scm likely-gambit.scm \
 0001-dynamic-extent.scm 0002-srfi-1.scm \
 0011-stm-imports.scm 0020-stm-impl.scm \
 0100-observable.scm

likely.$(LOCAL_DYNAMIC_LIBRARY_EXTENSION): $(SOURCES)
	$(GAMBINDIR)/gsc -dynamic $(BUILD_OPTIONS) likely.scm

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
