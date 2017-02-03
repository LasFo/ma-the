import Control.Concurrent (forkIO, threadDelay)
import STM
import Data.List (sort)
import qualified Data.Traversable as T

--Test case where a number of tvars is created.
--For each tvars a thread is created which increments this
--tvar repeatedly. The main thread on the other hand repeatedly 
--reads all tvars sort their values and write them back in a sorted
--order. In every iteration the main thread print the (sorted) values
--of the tvars.
--Since the main thread needs access to all tvars it usually is aborted
--and even for a small amount of tvars the main thread is never 
--successful. If you want the transactions not to interrupt the
--main thread in every iteration you can add a threadDelay in the
--increaser function to make sure the main thread has some time 
--to work. 


tvars = 3

main = do 
  print "Desired output: sorted List"
  print "Expected output: no output, because the printer is rolled back all the time."
  tvs <- atomically $ T.sequenceA $ replicate tvars (newTVar 0) 
  sequence $  map (forkIO . increaser) tvs
  sortPrint tvs
  
increaser :: TVar Int -> IO ()
increaser tv = do
  atomically $ (readAndModify tv (+1)) **> writeTVar tv
  --threadDelay 1
  increaser tv
  
readAndModify :: TVar a -> (a -> a) -> STM a
readAndModify tv f = readTVar tv <**> pure f 

sortPrint :: [TVar Int] -> IO ()
sortPrint tvs = do
   res <- atomically $ sortAndReturn tvs
   print res
   sortPrint tvs


sortAndReturn :: [TVar Int] -> STM [Int]
sortAndReturn tvs = 
  (T.sequenceA $ map readTVar tvs) <**>
  pure sort >>=
  (\ls -> 
    T.sequenceA (zipWith writeTVar tvs (map pure ls)) *>
    T.sequenceA  (map readTVar tvs))
