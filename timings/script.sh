#!/bin/bash
ghc -O2 -threaded StmTest.hs
for j in {1..50}
do
rm testfile
for i in {1..25}
do
  /usr/bin/time -a -o testfile -f %E ./StmTest $j +RTS -N
done
./parser testfile $j STMLAScaling
done
rm *.o
rm *.hi
rm StmTest
