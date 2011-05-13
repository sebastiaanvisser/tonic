#!/bin/sh

tool=Benchmarks
./dist/${tool}-prof profile parser $1 +RTS -p -hy -i0.05
# hp2ps -c *.hp
# open *.ps

