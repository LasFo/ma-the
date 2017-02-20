#!/bin/bash
ghc -O2 -threaded StmTest.hs
for i in {1..10}
do
  /usr/bin/time -f %E ./StmTest 50 +RTS -N
done
rm *.o
rm *.hi
rm StmTest
