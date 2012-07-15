BASE=Flow
APP=${BASE}.app
FLOWAPPCONTENTS=${APP}/Contents
rm -rf ${APP}

pushd ../server/
raco exe --gui -o ${BASE} gui.rkt
say -v Victoria "Done compiling."
mv ${APP} ../build
popd

mkdir -p ${FLOWAPPCONTENTS}/bin/macosx/
cp -R ../bin/macosx/ ${FLOWAPPCONTENTS}/bin/macosx/
cp -R ../config/ ${FLOWAPPCONTENTS}/config/
cp -R ../interface/ ${FLOWAPPCONTENTS}/interface/
cp -R ../occam/ ${FLOWAPPCONTENTS}/occam/
mkdir -p ${FLOWAPPCONTENTS}/temp

say -v Victoria "Done copying."

rm -rf ${BASE}.dmg
./create-dmg/create-dmg ${BASE}.dmg ${APP}

say -v Victoria "Done making disk image."
