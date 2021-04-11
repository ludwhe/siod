MANSEC=1

MANPAGES=cp-build csiod ftp-cp ftp-get ftp-put ftp-test http-get http-stress \
	proxy-server siod snapshot-compare snapshot-dir

all:

.PHONY: all install uninstall clean

install:
	-mkdir -p $(PREFIX)/share/man/man$(MANSEC)
	for X in $(MANPAGES); do \
		cp $$X.man $(PREFIX)/share/man/man$(MANSEC)/$$X.$(MANSEC); \
	done

uninstall:
	for X in $(MANPAGES); do \
		$(RM) $(PREFIX)/share/man/man$(MANSEC)/$$X.$(MANSEC); \
	done

clean:
