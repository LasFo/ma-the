#!/bin/bash
ghc -O2 -threaded PerformanceTest.hs
for j in {10..11}
do
rm testfile
for i in {1..5}
do
  /usr/bin/time -a -o testfile -f %E ./PerformanceTest $j +RTS -N
done
./parser testfile $j GHCScaling
done
rm *.o
rm *.hi
rm PerformanceTest
