LN_HOME=$(HOME)/.cache/lambdanative

BALL_VERSION=0.9.5.9

ARCHIVE=$(LN_HOME)/packages/askemos-$(BALL_VERSION).tar.gz

tar: force
	tar -czf $(ARCHIVE) askemos-$(BALL_VERSION) libupskirt-1.0
	CS=`sha1sum $(ARCHIVE)|cut -f 1 -d ' '` ; sed -i -es/'PKGHASH=.*$$'/PKGHASH=$${CS}/ make.sh

clean: force
	make -C askemos-$(BALL_VERSION) clean

force:
