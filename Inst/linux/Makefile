NAME=picky
ARCH=$(shell uname -m)
VERS=$(shell ../../o.pick -v| sed 's/[^0-9]*//g')

all: $(NAME)_$(ARCH).deb

install: all

clean:
	rm -f $(NAME)/debian/input/*
	cd $(NAME) && sudo make -f rules clean

deb:
	cp ../icons/picky128.png $(NAME)/debian/input/picky128.png
	cp  ../man/pick.1 $(NAME)/debian/picky.1	#pick.1 is taken already by nmh
	cp ../../o.pick $(NAME)/debian/input/pick
	cp ../../o.pam $(NAME)/debian/input/pam
	strip $(NAME)/debian/input/pam
	strip $(NAME)/debian/input/pick
	chmod +x $(NAME)/rules
	cd $(NAME) && sudo ./rules
	mv picky*.deb ../bin

$(NAME)_$(ARCH).deb:
	cat preinst|sed 's/__ARCH__/'$(ARCH)'/g' >  $(NAME)/debian/preinst
	cat control$(ARCH)|sed 's/__VERS__/'$(VERS)'/g' >  $(NAME)/debian/control
	cat changelog|sed 's/__VERS__/'$(VERS)'/g' >  $(NAME)/debian/changelog
	$(MAKE) clean
	echo compiling $(ARCH)
	$(MAKE) deb


kosher:
	cd ../bin && lintian picky*.deb && echo the deb file is kosher

.PHONY: all clean kosher install deb

