install: picky.dmg

clean:
	rm -rf picky dmgdir picky.dmg
	
dmgdir/picky.pkg:
	test -d dmgdir || mkdir dmgdir
	test -d picky || mkdir picky
	cp ../../o.pam picky/pam
	cp ../../o.pick picky/pick
	cd picky
	(cd picky; /Applications/PackageMaker.app/Contents/MacOS/PackageMaker --root . --title 'Picky installer' --out ../dmgdir/picky.pkg --scripts ../scripts --id org.lsub.Picky --install-to /bin)
	chmod +x ./dmgdir/picky.pkg
	
picky.dmg: dmgdir/picky.pkg
	rm -f picky.dmg
	hdiutil create -fs HFS+ -srcfolder dmgdir -volname picky picky.dmg

install: picky.dmg
	cp picky.dmg ../bin

.PHONY: all clean install
