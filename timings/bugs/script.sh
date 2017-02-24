#!/bin/bash
ghc -O2 -threaded PerformanceTest.hs
for j in {5..6}
do
#rm testfile
for i in {1..5}
do
  /usr/bin/time -f %E ./PerformanceTest $j +RTS -N
done
#./parser testfile $j GHCScaling
done
rm *.o
rm *.hi
rm PerformanceTest
