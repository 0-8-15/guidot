# Where to find compile time tools and libraries
SYS_HOSTPREFIX ?= /home/u/.cache/lambdanative/linux

#  Where to install headers, runtime libraries etc.
SYS_PREFIX ?= $(SYS_HOSTPREFIX)

GSC_SYSPREFIX ?= $(SYS_PREFIX)

GSC ?= $(SYS_GSC)

SYS_PATH ?= $(SYS_PREFIX)/lib/
SYS_PATH2 = $(shell echo $(SYS_PATH) | cut -f2 -d:)
SYS_MOD_PATH ?= $(SYS_PATH2)/modules

all: fossil	# Main target

fossil:	$(SYS_PREFIX)/bin/fossil
	# echo copying $(SYS_PREFIX)/bin/fossil to $(INSTALL_DIR)
	# cp $(SYS_PREFIX)/bin/fossil $(INSTALL_DIR)/libfossil.so
	echo NIX

install: force
	echo cp $(ARG_FROMDIR)/bin/fossil $(ARG_INSTALL_DIR)/libfossil.so
	cp $(ARG_FROMDIR)/bin/fossil $(ARG_INSTALL_DIR)/libfossil.so
	chmod +x $(ARG_INSTALL_DIR)/libfossil.so

clean:	force
clean:

force:

help:	# help on possible targets
	@echo "documented make targets:"
	-@egrep "^[-A-Za-z0-9\._]+::?.*#" [Mm]akefile
