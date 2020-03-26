VERSION=2.1.2
VERSION_CONTRIB=2.1.0

all: # Everything
all: test

package: # package core and contrib into one
package: lwip-$(VERSION).tar.gz

DISTURL_BASE=http://download.savannah.nongnu.org/releases/lwip

lwip-$(VERSION).tar.gz: lwip-core-$(VERSION).zip lwip-contrib-$(VERSION_CONTRIB).zip
	mkdir .repack
	(cd .repack && unzip ../lwip-core-$(VERSION).zip && unzip ../lwip-contrib-$(VERSION_CONTRIB).zip)
	mv .repack/contrib-$(VERSION_CONTRIB) .repack/lwip-$(VERSION)/contrib
	(cd .repack && tar -czf ../lwip-$(VERSION).tar.gz . )
	rm -rf .repack
	test -z "$(SAVE_IN)" || mv lwip-$(VERSION).tar.gz $(SAVE_IN)
	CS=`sha1sum lwip-$(VERSION).tar.gz|cut -f 1 -d ' '` ; sed -i -es/'PKGHASH=.*$$'/PKGHASH=$${CS}/ make.sh

lwip-core-$(VERSION).zip:
	wget -O lwip-core-$(VERSION).zip $(DISTURL_BASE)/lwip-$(VERSION).zip

lwip-contrib-$(VERSION_CONTRIB).zip:
	wget -O lwip-contrib-$(VERSION_CONTRIB).zip $(DISTURL_BASE)/lwip/contrib-$(VERSION_CONTRIB).zip

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
