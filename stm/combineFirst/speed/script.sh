#!/bin/bash
ghc --make -O2 StmTest.hs
rm testfile
for i in {1..25}
do
  /usr/bin/time -a -o testfile -f %E ./StmTest 
done

./parser testfile
