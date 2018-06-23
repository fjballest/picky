</$objtype/mkfile
BIN=/$objtype/bin
SRC=/sys/src/cmd
MDEP=plan9
DIFFSOK=(xxxxxxx ptr.p ptr2.p ptr3.p xample.p sqrts.p ovf.p )

TARG=\
	pick\
	pam\

SOURCE=`{echo *.c}

HFILES=\
	syshdr.h\
	y.tab.h\
	p.h\
	pam.h\
	pilib.h\

OFILES=\
	emalloc.$O\
	pam.$O\
	$MDEP.$O\

PICKOFILES=\
	y.tab.$O\
	lex.$O\
	sym.$O\
	type.$O\
	builtin.$O\
	gen.$O\
	$OFILES\

PAMOFILES=\
	pi.$O\
	pilib.$O\
	$OFILES\

CLEANFILES=\
	syshdr.h\

YFILES=p.y

<$SRC/mkmany

lex.$O: lex.c vers.h $HFILES

$O.pick: $PICKOFILES
	$LD -o $target $prereq

$O.pam: $PAMOFILES
	$LD -o $target $prereq

test:QV: $O.pick $O.pam
	mk stest

syshdr.h: $MDEP.h
	cp $MDEP.h syshdr.h

vers.h:V:
	echo '#define VERS	"'^`{date -n}^'"' > $target

export:V:
	mk clean
	rm -f /tmp/empty
	mkdir /tmp/empty
	rfork n
	bind /tmp/empty tests
	cp /sys/man/1/pick pick.man
	cd /sys/src/cmd
	tar zcvf /tmp/picky.tgz picky
	mv /tmp/picky.tgz /usr/planb/doc/www/export/picky.tgz
	cd /
	tar zcvf /tmp/picky.tgz 386/bin/pick 386/bin/pam sys/man/1/pick
	mv /tmp/picky.tgz /usr/planb/doc/www/export/pickybin.plan9.386.tgz

stest:QV:
	cd tests
	pass=PASS
	for(t in *.p){
		rm -f out.pam
		../$O.pick -g $t > $t.out.raw >[2=1] || ;
		sed 's/((pc)|(sp)|(fp)|(status))=0x[0-9a-fA-F]+//g' $t.out.raw > $t.out
		rm -f $t.out.raw
		test -f out.pam && ../$O.pam out.pam <$t >>$t.out >[2=1] || ; 
		test -e $t.ok || cp $t.out $t.ok
		if(diff -w $t.ok $t.out){
			echo tests/$t.ok
			rm $t.out
		}
		if not{
			echo -n tests/$t.out is not tests/$t.ok
			if(~ $t $DIFFSOK)
				echo ' ' but ok
			if not{
				pass=FAIL
				echo
			}
		}
	}
	if(grep 'sys:' *.out | grep -v '^sqrts.p')
		pass=FAIL
	echo
	echo $pass
	status = ''

Test:V: $O.pc
	rm -f tests/*.ok
	mk test

X:V: $O.pc $O.pi
	$O.pick -g  tests/xample.p >[2=1]
	$O.pam  out.pam >[2=1]
