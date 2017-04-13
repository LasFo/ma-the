import STMLA
import System.IO.Unsafe

--This test is used to check when a tvar is evaluated.
--In this case the outer TVar is evaluated because
--the second readTVar uses patternmatching on the inner value. 

main = do
  tv <- atomically $ newTVar 5 >>= newTVar >>= newTVar 
  a <- atomically $ readTVar tv >>= readTVar >>= readTVar
  print a

main2 = do
  tv <- atomically $ newTVar (5,4)
  a <- atomically $ trans tv
  print a

trans tv = do 
  a <- readTVar tv
  if one a 
     then return 3
     else return 5

--Although the value of the patternmatching are not used
--and the pattern matching is unfailable (one pair) prints 
--"f"
one :: (a,b) -> Bool
one (_,_) = True

pair = unsafePerformIO $ print "f" >> return (1,2)
