rm -rf ./Flow.app/

raco exe --gui -o Flow gui.rkt
say "Done compiling."

cp -R ../bin/ ./Flow.app/Contents/bin/
cp -R ../config/ ./Flow.app/Contents/config/
cp -R ../interface/ ./Flow.app/Contents/interface/
cp -R ../occam/ ./Flow.app/Contents/occam/
mkdir -p ./Flow.app/Contents/temp

say "Done copying."
