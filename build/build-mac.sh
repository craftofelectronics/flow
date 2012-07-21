BASE=Flow
APP=${BASE}.app
FLOWAPPCONTENTS=${APP}/Contents
ICON=arduino
VOLICON=concurrencycc-logo

rm -rf ${APP}

pushd ../server/
raco exe --gui -o ${BASE} gui.rkt
say -v Victoria "Done compiling."
mv ${APP} ../build
popd

# http://hints.macworld.com/article.php?story=20051225191249727
pushd ../build/
  rm ${FLOWAPPCONTENTS}/Resources/Starter.icns
  convert -scale 256x256 ${ICON}.png ${ICON}-256.png
  sips -s format icns ${ICON}-256.png --out ${FLOWAPPCONTENTS}/Resources/Starter.icns
  convert -scale 256x256 ${VOLICON}.png ${VOLICON}-256.png
  sips -s format icns ${VOLICON}-256.png --out ${VOLICON}-256.icns
popd


mkdir -p ${FLOWAPPCONTENTS}/bin/macosx/
cp -R ../bin/macosx/ ${FLOWAPPCONTENTS}/bin/macosx/
cp -R ../config/ ${FLOWAPPCONTENTS}/config/
cp -R ../interface/ ${FLOWAPPCONTENTS}/interface/
cp -R ../occam/ ${FLOWAPPCONTENTS}/occam/
mkdir -p ${FLOWAPPCONTENTS}/temp

say -v Victoria "Done copying."

rm -rf ${BASE}.dmg
./create-dmg/create-dmg \
	--background ${VOLICON}.png \
	--icon-size 128 \
	--icon ${APP} 220 200 \
	--volicon ${VOLICON}-256.icns \
	${BASE}.dmg ${APP}

say -v Victoria "Done making disk image."

say -v Victoria "Transferring zeroes and ones into the internets."

if [ "${USER}" == "jadudm" ]; then
	scp -i ~/.ssh/id_rsa ${BASE}.dmg jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/
fi
say -v Victoria "I have uploaderized the file."
