#!/bin/sh

tool=Benchmarks
buildOpts="--make -O -threaded -Wall -i../src -rtsopts"
outOpts="-odir dist -hidir dist -o dist/${tool}"
profOutOpts="-odir dist/p -hidir dist/p -o dist/${tool}-prof"
profOpts="-prof -auto-all -osuf=op"

mkdir -p dist &&
mkdir -p dist/p &&
ghc ${buildOpts}             ${outOpts}     ${tool}.hs 
ghc ${buildOpts} ${profOpts} ${profOutOpts} ${tool}.hs 


./dist/${tool}-prof +RTS -p -hy -i0.1 && hp2ps -c *.hp && open *.ps



# ghc --make -O -prof -auto-all -osuf=op -threaded -Wall -i../../type-engine/src -i../src -rtsopts ${tool}.hs -odir dist/p -hidir dist/p -o dist/p/${tool}-prof &&
# ./dist/${tool} $2 $3 $4 +RTS -N -I2 -qb -qg -RTS
# ./dist/p/${tool}-prof $2 $3 $4 +RTS -p -hy -i0.01 -N -I2 -qb -qg 
