#!/bin/bash
ghc -O2 -threaded PerformanceTest.hs

for j in {1..75}
do
rm testfile
for i in {1..20}
do
  /usr/bin/time -a -o testfile -f %E ./PerformanceTest $j +RTS -N
done
./parser testfile $j LAScaling2
done
rm *.o
rm *.hi
rm PerformanceTest
