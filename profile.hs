cabal install --enable-executable-profiling --ghc-options=-auto-all --disable-documentation &&
echo press enter &&
read &&
time ./dist/build/test/test +RTS -p -hy -i0.01 &&
echo press enter &&
read &&
hp2ps -c *.hp &&
open *.ps

