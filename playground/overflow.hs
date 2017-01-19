import Data.Bits

--overflows do not evoke exception, they just overflow
main = do 
  print $ (+) (1::Int)  $ clearBit (complement 0::Int) 63

