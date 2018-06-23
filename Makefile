# Can be used in:
#	cygwin+mingw in windows
#	native in linux/inux
#	for binary installers see the installers directory

O=o

CFLAGS=-g -I. -I./libfmt	-I./libbio -I./libutf -Wno-trigraphs -Wno-main
CC=gcc
LFLAGS=-lm -L./libfmt/ -L./libbio/ -L./libutf/ -lbio -lfmt -lutf

SYSNAME:=${shell uname| sed 's/_.*//g' }
ARCH:=${shell uname -m}

ifeq ($(ARCH), i386)
	CFLAGS+= -m32
	LFLAGS+= -m32
endif

ifeq ($(SYSNAME), CYGWIN)
	CFLAGS=-g -I/usr/include/ -I. -I./libfmt -I./libbio -I./libutf -Wno-trigraphs -Wno-main
	CC=i686-w64-mingw32-gcc.exe
	LFLAGS=-lm -L./libfmt/ -L./libbio/ -L./libutf/ -lbio -lfmt -lutf ./Installers/icons/picky.res
endif


ifeq ($(SYSNAME), Darwin)
	CFLAGS+=
	LFLAGS+= -mmacosx-version-min=10.4
endif


LD=$(CC)




LIBS=./libfmt/libfmt.a ./libbio/libbio.a ./libutf/libutf.a

all:	y.tab.c mklibs syshdr.h $(O).pam $(O).pick 

y.tab.c: p.y $(HRFILES)
	bison -y -d $<

TARG=\
	pick\
	pam\

HRFILES=\
	u.h\
	p.h\
	pam.h\
	pilib.h\
	vers.h\

HFILES=\
	y.tab.h\
	$(HRFILES)\

OFILES=\
	emalloc.$(O)\
	pam.$(O)\
	$(MDEP.$O)\

PICKOFILES=\
	unix.$(O)\
	y.tab.$(O)\
	lex.$(O)\
	sym.$(O)\
	type.$(O)\
	builtin.$(O)\
	gen.$(O)\
	$(OFILES)

PAMOFILES=\
	unix.$(O)\
	pi.$(O)\
	pilib.$(O)\
	$(OFILES)

YFILES=p.y\

#echo builtins behave in strange ways
syshdr.h:
	cp unix.h syshdr.h
	if uname|grep -i cygwin; then\
		echo '#define ISCYGWIN 1' >> syshdr.h;\
		`which echo` '#define EOL "\r\n"' >>syshdr.h;\
		`which echo` '#define _OBINARY _O_BINARY' >>syshdr.h;\
	else\
		echo '#define ISCYGWIN 0' >> syshdr.h;\
		`which echo` '#define EOL "\n"' >>syshdr.h;\
		`which echo` '#define _OBINARY 0' >>syshdr.h;\
	fi

$(O).pick: $(PICKOFILES)
	$(LD) -o $@ $(LIBS) $? $(LFLAGS) 

$(O).pam: $(PAMOFILES)
	$(LD) -o $@ $(LIBS) $? $(LFLAGS)


test: $(O).pick $(O).pam
	make stest

stest:
	cd tests; ./test.sh $(O)

Test: $(O).pc
	rm -f tests/*.ok
	mk test

mklibs:
	for d in lib*; do\
		test -d $$d &&  (cd $$d && make);\
	done;\

cleanlibs:
	for d in lib*; do\
		test -d $$d &&  (cd $$d && make clean);\
	done;\

X: $(O).pc $(O).pi mklibs
	$(O).pick -g  tests/xample.p 2>&1
	$(O).pam  p.out 2>&1

# NOTE: no target for vers.h, that way we take vers.h from the last Plan 9 install.

clean: cleanlibs
	rm -f *.[o] [o].out o.pick o.pam y.tab.[ch] syshdr.h ./*~

.PHONY: all test stest Test RR clean mklibs cleanlibs syshdr.h
