DEST=Flow.app/Contents
rm -rf Flow.app

raco exe --gui -o Flow gui.rkt
say -v Victoria "Done compiling."

mkdir -p ${DEST}/bin/macosx/
cp -R ../bin/macosx/ ${DEST}/bin/macosx/
cp -R ../config/ ${DEST}/config/
cp -R ../interface/ ${DEST}/interface/
cp -R ../occam/ ${DEST}/occam/
mkdir -p ${DEST}/temp

say -v Victoria "Done copying."

if [ -d /tmp/asdfasdfasdfasdfasdf ] ; then
	hdiutil eject /Volumes/Flow
	rm -rf Flow.dmg
	hdiutil create -megabytes 50 -fs HFS+ -volname Flow ./Flow
	open ./Flow.dmg
fi

rm -rf Flow.dmg
../../create-dmg/create-dmg Flow.dmg Flow.app

say -v Victoria "Done making disk image."
