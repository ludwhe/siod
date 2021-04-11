export PREFIX=/usr/local

all:
	$(MAKE) -C src

.PHONY: all install uninstall check clean

install:
	$(MAKE) -C src install
	$(MAKE) -C docs install

uninstall:
	$(MAKE) -C src uninstall
	$(MAKE) -C docs uninstall

check:
	@echo "No test specified."

clean:
	$(MAKE) -C src clean
