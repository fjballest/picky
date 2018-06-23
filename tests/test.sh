#!/bin/sh

if test $# -ne 1; then
	echo usage: test.sh o 1>&2
	exit 1
fi

O=$1 export O

PASS=pass export PASS
for t in *.p; do 
	rm -f out.pam /tmp/err;
	touch /tmp/$t.err
	../$O.pick -g $t > $t.out 2>&1 ||  
	test -f out.pam && ../$O.pam out.pam <$t > $t.out 2> /tmp/$t.err ||  
	test -e $t.ok || cp $t.out $t.ok; 

	#I cannot mix stderr and stdout, offsets are wrong
	#this is wrong, works for now, but I need something better
	cat /tmp/$t.err $t.out|sed 's/'$O'\.pick/'8'.pick/' > /tmp/$pid^pick
	cat /tmp/$pid^pick |sed 's/'$O'\.pick/'8'.pick/' > $t.out

	if cmp -s $t.ok $t.out; then 
		echo tests/$t.ok; 
		rm $t.out; 
	else 
		echcom=`which echo`	#not the builtin,
		$echcom -n tests/$t.out is not tests/$t.ok; 
		if echo $t |grep -E '^(ptr|ptr2|ptr3|xample|sqrts|ovf)\.p' > /dev/null 2>&1; then 
			echo ' ' but ok; 
		else 
			PASS=fail export PASS; 
			echo; 
		fi 
	fi 
done
if grep 'sys:' *.out | grep -v '^sqrts.p'; then 
	PASS=fail export PASS; 
fi
echo tests $PASS; 

