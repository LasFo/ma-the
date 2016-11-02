

import Control.Concurrent (forkIO, threadDelay)
import STM
import Data.List (sort)


tvars = 3

main = do 
  tvs <- atomically $ sequence $ replicate tvars (newTVar 0)
  sequence $  map (forkIO . increaser) tvs
  sortPrint tvs
  
increaser :: TVar Int -> IO ()
increaser tv = do
  atomically $ do a <- readAndModify tv (+1)
                  writeTVarLazy tv a
  threadDelay 1
  increaser tv
  
readAndModify :: TVar a -> (a -> a) -> STM (LazyValue a)
readAndModify tv f = do a <- readTVarLazy tv
                        return $ pure f <*> a
             

sortPrint :: [TVar Int] -> IO ()
sortPrint tvs = do
   res <- atomically $ sortAndReturn tvs
   print res
   sortPrint tvs


sortAndReturn :: [TVar Int] -> STM [Int]
sortAndReturn tvs = do
  ls <- sequence $ map readTVar tvs
  let sorted = sort ls
  sequence_ $ zipWith writeTVar tvs sorted
  sequence $ map readTVar tvs
