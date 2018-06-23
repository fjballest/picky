# Use one of:
#	cygwin, windows


NSIS=$(shell cygpath -p "${PROGRAMFILES}\NSIS\makensis.exe")

all:	picky.res picky.exe

install: all

picky.ico:
	cp ../icons/picky.ico .

picky.res: picky.rc picky.ico
	windres picky.rc -O coff -o picky.res
	cp picky.res ../icons

picky.exe: picky.res
	cp ../icons/picky.ico .
	cp ../../o.pam pam.exe
	cp ../../o.pick pick.exe
	"$(NSIS)" picky.nsi	
	mv picky.exe ../bin

clean:
	rm -rf picky.res picky.exe o.pam.exe o.pick.exe  ./*~ picky.ico

.PHONY: all clean install
