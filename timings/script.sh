#!/bin/bash
ghc -O2 -threaded StmTest.hs
rm testfile
for i in {1..25}
do
  /usr/bin/time -a -o testfile -f %E ./StmTest +RTS -N
done
rm *.o
rm *.hi
rm StmTest
./parser testfile
