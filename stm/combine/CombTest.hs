

import Control.Concurrent (forkIO, threadDelay)
import qualified STMM as M
import qualified STMC as C
import Data.List (sort)


tvars = 3

main = do 
  tvs <- M.atomically $ sequence $ replicate tvars (M.newTVar 0 (>0)) --also possible as C.atomi...
  sequence $  map (forkIO . increaser) tvs
  sortPrint tvs
  
increaser :: C.TVar Int -> IO ()
increaser tv = do
  C.atomically $ C.writeTVar tv (readAndModify tv (+1))
  threadDelay 1
  increaser tv
  
readAndModify :: C.TVar a -> (a -> a) -> C.STM a
readAndModify tv f = res
  where val = C.readTVar tv
        res = (C.<*>) (C.pure f) val
             

sortPrint :: [M.TVar Int] -> IO ()
sortPrint tvs = do
   res <- M.atomically $ sortAndReturn tvs
   print res
   sortPrint tvs


sortAndReturn :: [M.TVar Int] -> M.STM [Int]
sortAndReturn tvs = do
  ls <- sequence $ map M.readTVar tvs
  let sorted = sort ls
  sequence $ zipWith M.writeTVar tvs sorted
  sequence $ map M.readTVar tvs
