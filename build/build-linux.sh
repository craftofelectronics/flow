DEST=../Flow
rm -rf ${DEST}
mkdir -p ${DEST}

raco exe --gui -o Flow gui.rkt
raco distribute ${DEST} Flow
 
cp -R ../bin/unix/ ${DEST}/bin/unix/
cp -R ../config/ ${DEST}/config/
cp -R ../interface/ ${DEST}/interface/
cp -R ../occam/ ${DEST}/occam/
mkdir -p ${DEST}/temp

pushd ..
tar cvzf Flow.tgz Flow/
