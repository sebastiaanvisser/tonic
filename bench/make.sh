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


