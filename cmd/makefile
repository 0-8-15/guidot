#			    -*-makefile-*-

## HINTS
# - always distribute `Makefile` while `makefile` is personal

fire:	# compile and run test
	make -f Makefile all
	# /home/u/.cache/lambdanative/linux/cmd/cmd

#fresh:	$FILE
#$FILE:

x:
	echo crabbs > $@
	make refresh REFRESH_COMPARE="cmp -s" REFRESH_TARGET_FILE=$@

#* help targets/templates

#** refresh
#   1. INPUT: $(REFRESH_TARGET_FILE) is moved to $(REFRESH_TARGET_FILE).$(FRESH_EXTENSION_ORIG)
#      - id fresh-version does not yet exist
#      - if REFRESH_COMPARE indicates a change
#   2. REFRESH_CLEANUP_TARGET is made
#   3. REFRESH_TARGET_FILE is made
#   Notes:
#       I.) For boostrap/letrec situations target $(REFRESH_TARGET_FILE)
#           should itself end with a make refresh with the same target.
#      II.) Beware: none-reproducible builds will loop infinitely dues to (I).
#   Examples:
#   x:
#	echo crabbs > $@
#	make refresh REFRESH_COMPARE="cmp -s" REFRESH_TARGET_FILE=$@
#   What NOT to do:
#   infinit-loop:
#	make uuid > $@ # rebuild input
#	make refresh REFRESH_TARGET_FILE=$@

FRESH_EXTENSION_ORIG=fresh
#refresh: $(REFRESH_TARGET_FILE)
REFRESH_TARGET_FILE ?= $(FILE)
REFRESH_CLEANUP_TARGET ?= force
REFRESH_COMPARE ?= cmp -s
refresh::
	@test -z "$(REFRESH_TARGET_FILE)" && (echo refresh_ with empty file && exit 1) || true
	@test -f "$(REFRESH_TARGET_FILE)" || (echo refesh: file \"$(REFRESH_TARGET_FILE)\" missing && false)
	@# @test ! -f "$(REFRESH_TARGET_FILE).$(FRESH_EXTENSION_ORIG)" || (echo refesh: last file \"$(REFRESH_TARGET_FILE).$(FRESH_EXTENSION_ORIG)\" exists, aborting; false)
	@echo refesh: \"$(REFRESH_TARGET_FILE)\", keep last fresh state in \"$(REFRESH_TARGET_FILE).$(FRESH_EXTENSION_ORIG)\"
	@test -f "$(REFRESH_TARGET_FILE).$(FRESH_EXTENSION_ORIG)" || mv $(REFRESH_TARGET_FILE) $(REFRESH_TARGET_FILE).$(FRESH_EXTENSION_ORIG)
	@test ! -f "$(REFRESH_TARGET_FILE)" || ($(REFRESH_COMPARE) "$(REFRESH_TARGET_FILE)" "$(REFRESH_TARGET_FILE).$(FRESH_EXTENSION_ORIG)" || mv $(REFRESH_TARGET_FILE) $(REFRESH_TARGET_FILE).$(FRESH_EXTENSION_ORIG) )
	@make $(REFRESH_CLEANUP_TARGET)
	@make $(REFRESH_TARGET_FILE)

#** UUID
uuid:	# generate UUID
uuid:
	@cat /proc/sys/kernel/random/uuid

#* Appendix
#  generics

force:

help:	# help on possible targets
	-@egrep "^[-A-Za-z0-9\._]+::?.*#" [Mm]akefile
