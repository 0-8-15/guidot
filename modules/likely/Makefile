all: # Everything
all: test

TEST_OPTIONS=-e '(define-cond-expand-feature test)'

test:	force
	$(HOME)/.cache/lambdanative/linux/bin/gsi $(TEST_OPTIONS) likely.scm

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
