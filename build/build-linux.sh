BASE=Flow
DEST=../${BASE}
SRC=../server
rm -rf ${DEST}
mkdir -p ${DEST}

pushd ${SRC}
  raco exe --gui -o ${BASE} gui.rkt
  sleep 2
  raco distribute ${DEST} ${BASE}
	sleep 2
	chmod 755 ${DEST}/${BASE}
popd
 
cp -R ../bin/unix/ ${DEST}/bin/unix/
sleep 2
cp -R ../config/ ${DEST}/config/
sleep 2
cp -R ../interface/ ${DEST}/interface/
sleep 2
cp -R ../occam/ ${DEST}/occam/
sleep 2
mkdir -p ${DEST}/temp

pushd ..
tar cvzf Flow.tgz Flow/

if [ "${USER}" == "reynoldsm" ]; then
	scp -i ~/.ssh/jadudm-rsa ${BASE}.tgz jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/
fi

popd


