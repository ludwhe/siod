CFLAGS+=-Wall -Wextra -Iinclude -fPIC

LD=gcc
LD_LIB_LIBS=-lm -lc -ldl -lcrypt
LD_EXE_LIBS=-ldl

SO=so

all: siod ssiod libsiod.$(SO) tar.$(SO) parser_pratt.$(SO) ss.$(SO) regex.$(SO) \
	acct.$(SO) ndbm.$(SO) gd.$(SO) #statfs.$(SO)

.PHONY: all extra install uninstall clean

extra: sql_sybase.$(SO) sql_msql.$(SO)

install:
	-mkdir -p $(PREFIX)/bin
	-mkdir -p $(PREFIX)/lib/siod
	cp siod ssiod $(PREFIX)/bin
	cp *.$(SO) $(PREFIX)/lib/siod

uninstall:
	-$(RM) $(PREFIX)/bin/siod
	-$(RM) $(PREFIX)/bin/ssiod
	-$(RM) -r $(PREFIX)/lib/siod

siod: siod.o libsiod.$(SO)
	$(LD) $^ -o $@ $(LD_EXE_LIBS) -Wl,-rpath,.

ssiod: siod.o slib.o sliba.o trace.o slibu.o md5.o
	$(CC) $^ -o $@ -lm -lcrypt -ldl

libsiod.$(SO): slib.o sliba.o trace.o slibu.o md5.o
	$(LD) -shared $^ -o $@ $(LD_LIB_LIBS)

regex.$(SO): regex.o regcomp.o regerror.o regexec.o regfree.o libsiod.$(SO)
	$(LD) -shared $^ -o $@ $(LD_LIB_LIBS)

# depends on FreeTDS (or original Sybase)
sql_sybase.$(SO): sql_sybase.o sql_sybasec.o libsiod.$(SO)
	$(LD) -shared $^ -o $@ -lct $(LD_LIB_LIBS)

# depends on mSQL
sql_msql.$(SO): sql_msql.o libsiod.$(SO)
	$(LD) -shared $^ -o $@ -lmsql $(LD_LIB_LIBS)

%.$(SO): %.o libsiod.$(SO)
	$(LD) -shared $^ -o $@ $(LD_LIB_LIBS)

clean:
	-$(RM) *.o *.$(SO) siod ssiod
