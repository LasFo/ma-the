#!/bin/bash
ghc -O2 -threaded StmTest.hs
for j in {51..75}
do
rm testfile
for i in {1..10}
do
  /usr/bin/time -a -o testfile -f %E ./StmTest $j +RTS -N
done
./parser testfile $j STMWSL1Scaling
done
rm *.o
rm *.hi
rm StmTest
