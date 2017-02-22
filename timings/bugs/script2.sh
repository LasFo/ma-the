#!/bin/bash
ghc -O2 -threaded StmTest.hs
for i in {1..5}
do
  /usr/bin/time -f %E ./StmTest 51 +RTS -N
done
rm *.o
rm *.hi
rm StmTest
