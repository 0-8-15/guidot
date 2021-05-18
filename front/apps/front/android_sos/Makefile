# Where to find compile time tools and libraries
SYS_HOSTPREFIX ?= /home/u/.cache/lambdanative/linux

#  Where to install headers, runtime libraries etc.
SYS_PREFIX ?= $(SYS_HOSTPREFIX)

GSC_SYSPREFIX ?= $(SYS_PREFIX)

GSC ?= $(SYS_GSC)

SYS_PATH ?= $(SYS_PREFIX)/lib/
SYS_PATH1 = $(shell echo $(SYS_PATH) | cut -f1 -d:)
SYS_PATH2 = $(shell echo $(SYS_PATH) | cut -f2 -d:)
SYS_MOD_PATH ?= $(SYS_PATH2)/modules
SYS_MOD_PATH1 ?= $(SYS_PATH1)/modules

all: ot0	# Main target

LIKELY_FILE = $(shell echo $(SYS_MOD_PATH)/likely/likely.scm |cksum|cut -f 1 -d " ")
MATCH_FILE = $(shell echo $(SYS_MOD_PATH)/akwmatch/akwmatch.scm |cksum|cut -f 1 -d " ")
LWIP_FILE =  $(shell echo $(SYS_MOD_PATH)/lwip/lwip.scm |cksum|cut -f 1 -d " ")
SOCKS_FILE =  $(shell echo $(SYS_MOD_PATH)/socks/socks.scm |cksum|cut -f 1 -d " ")
UNSOCKET_FILE =  $(shell echo $(SYS_MOD_PATH)/unsocket/unsocket.scm |cksum|cut -f 1 -d " ")
HTTPPROXY_FILE =  $(shell echo $(SYS_MOD_PATH)/httpproxy/httpproxy.scm |cksum|cut -f 1 -d " ")
DAEMONIAN_FILE =  $(shell echo $(SYS_MOD_PATH)/daemonian/daemonian.scm |cksum|cut -f 1 -d " ")
# IRREGEX_FILE =  $(shell echo $(SYS_MOD_PATH)/irregex2/irregex2.scm |cksum|cut -f 1 -d " ")
IRREGEX_FILE =  $(shell echo $(SYS_MOD_PATH1)/irregex/irregex.scm |cksum|cut -f 1 -d " ")

BEAVER_FILE =  $(shell echo $(SYS_MOD_PATH)/beaver/beaver.scm |cksum|cut -f 1 -d " ")

show:
	@echo likely $(LIKELY_FILE)
	@echo match $(MATCH_FILE)
	@echo lwip $(LWIP_FILE)
	@echo socks $(SOCKS_FILE)
	@echo unsocket $(UNSOCKET_FILE)
	@echo beaver $(BEAVER_FILE)
	@echo daemonian $(DAEMONIAN_FILE)

OT0_LIBSRC_DIR = $(SYS_PREFIX)/lib/onetierzero

INCLUDES += -I $(SYS_PREFIX)/include -I $(OT0_LIBSRC_DIR) -I $(GSCINCL)

GSCINCL = $(GSC_SYSPREFIX)/include

CFLAGS_COMMON ?= -fPIC $(INCLUDES)

CXXFLAGS += -DOMIT_JSON_SUPPORT $(CFLAGS_COMMON)

## Work around an apparent clang bug
CFLAGS += $(CFLAGS_COMMON) -O3

# STRIP=true
STRIP ?= $(SYS_STRIP)

SYS_BUILD_DIR ?= $(SYS_PREFIX)/build

CC=$(SYS_CC)
CXX=$(SYS_CXX)
CFLAGS+=$(INCLUDES)
CXXFLAGS+=$(INCLUDES)

.c.o:
	# $(GSC) -obj -cc-options "$(CFLAGS_COMMON)" -o $@ $<
	$(CC) -c -O1 $(CFLAGS_COMMON) -o $@ $<

00OT0_CLI_GAMBITC_FILES=$(SYS_BUILD_DIR)/$(LIKELY_FILE).c $(SYS_BUILD_DIR)/$(MATCH_FILE).c \
	$(OT0_LIBSRC_DIR)/ot0.c \
	$(SYS_BUILD_DIR)/$(IRREGEX_FILE).c $(SYS_BUILD_DIR)/$(UNSOCKET_FILE).c $(SYS_BUILD_DIR)/$(SOCKS_FILE).c \
	$(SYS_BUILD_DIR)/$(DAEMONIAN_FILE).c \
	$(SYS_BUILD_DIR)/$(LWIP_FILE).c \
	$(OT0_LIBSRC_DIR)/ot0use.c $(SYS_BUILD_DIR)/$(HTTPPROXY_FILE).c $(OT0_LIBSRC_DIR)/ot0cli.c

OT0_CLI_GAMBITC_FILES=$(SYS_BUILD_DIR)/$(LIKELY_FILE).c $(SYS_BUILD_DIR)/$(MATCH_FILE).c \
	$(SYS_BUILD_DIR)/$(LWIP_FILE).c \
	$(SYS_BUILD_DIR)/$(DAEMONIAN_FILE).c \
	$(SYS_BUILD_DIR)/$(BEAVER_FILE).c \
	$(SYS_BUILD_DIR)/$(IRREGEX_FILE).c $(SYS_BUILD_DIR)/$(UNSOCKET_FILE).c $(SYS_BUILD_DIR)/$(HTTPPROXY_FILE).c $(SYS_BUILD_DIR)/$(SOCKS_FILE).c

$(SYS_BUILD_DIR)/libbeaver_.c: $(OT0_CLI_GAMBITC_FILES)
	$(GSC) $(GSCFLAGS) -o $@ -link $(00OT0_CLI_GAMBITC_FILES)

00OT0_CLI_OBJECTS = $(SYS_BUILD_DIR)/$(LIKELY_FILE).o $(SYS_BUILD_DIR)/$(MATCH_FILE).o \
	$(OT0_LIBSRC_DIR)/ot0-hooks.o $(OT0_LIBSRC_DIR)/ot0.o \
	$(SYS_BUILD_DIR)/$(IRREGEX_FILE).o $(SYS_BUILD_DIR)/$(UNSOCKET_FILE).o $(SYS_BUILD_DIR)/$(SOCKS_FILE).o \
	$(SYS_BUILD_DIR)/$(HTTPPROXY_FILE).o \
	$(SYS_BUILD_DIR)/$(LWIP_FILE).o  $(SYS_BUILD_DIR)/$(DAEMONIAN_FILE).o \
	$(OT0_LIBSRC_DIR)/ot0use.o $(OT0_LIBSRC_DIR)/ot0cli.o

OT0_CLI_OBJECTS = $(SYS_BUILD_DIR)/$(LIKELY_FILE).o $(SYS_BUILD_DIR)/$(MATCH_FILE).o \
	$(SYS_BUILD_DIR)/$(BEAVER_FILE).o \
	$(SYS_BUILD_DIR)/$(IRREGEX_FILE).o $(SYS_BUILD_DIR)/$(UNSOCKET_FILE).o $(SYS_BUILD_DIR)/$(SOCKS_FILE).o \
	$(SYS_BUILD_DIR)/$(HTTPPROXY_FILE).o \
	$(SYS_BUILD_DIR)/$(LWIP_FILE).o \
	$(SYS_BUILD_DIR)/$(DAEMONIAN_FILE).o \
	$(OT0_LIBSRC_DIR)/ot0.o $(OT0_LIBSRC_DIR)/ot0use.o \
	# $(OT0_LIBSRC_DIR)/ot0cli.o

REALLY_STATIC=-static -pthread

$(SYS_BUILD_DIR)/libbeaver_.o: $(SYS_BUILD_DIR)/libbeaver_.c
	$(SYS_CC) $(CFLAGS_COMMON) -c -o $@ $<

ot0_real:	ot0cli_.o
	$(SYS_CXX) -o $@ ot0cli_.o -shared -L ../libs/$(APP_ABI)/ -lpayloadshared
	$(SYS_STRIP) $@
	mv $@ ../libs/$(APP_ABI)/libbeaver.so

ot0cli.o: $(OT0_LIBSRC_DIR)/ot0cli.c
	$(SYS_CC) -c -o $@ $<

$(SYS_PREFIX)/lib/libbeaver.so:  show $(00OT0_CLI_OBJECTS) $(SYS_BUILD_DIR)/libbeaver_.o
	$(SYS_CXX) -o $@ $(REALLY_STATIC) -L. -L $(SYS_PREFIX)/lib -L $(SYS_ROOT)/lib -L $(GSC_SYSPREFIX)/lib \
	 $(00OT0_CLI_OBJECTS) $(SYS_BUILD_DIR)/libbeaver_.o \
	 -llwipcore -lgambit -lzerotiercore -ldl -lc # -lutil -lpthread -lc
	$(SYS_STRIP) $@

ot0:	$(SYS_PREFIX)/lib/libbeaver.so force
	echo copying $(SYS_PREFIX)/lib/libbeaver.so to $(INSTALL_DIR)
	# cp $(SYS_PREFIX)/lib/libbeaver.so $(INSTALL_DIR)/libbeaver.so

clean:	force
clean:
	rm -f ot0cli_.*

force:

help:	# help on possible targets
	@echo "documented make targets:"
	-@egrep "^[-A-Za-z0-9\._]+::?.*#" [Mm]akefile
