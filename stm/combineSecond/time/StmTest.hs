module Main where

import Control.Concurrent
--import Control.Concurrent.STM
import STM 

import qualified Data.Traversable as T
import System.Random
import Data.List

threads = 25
iter    = 1000
tvars   = 40
changes = 20

main = do sync <- atomically $ newTVar threads 
          ts <- atomically $ T.sequenceA $ replicate tvars (newTVar 0)
          sequence $ replicate threads (forkIO $ do
                                           perform iter ts 
                                           atomically $ readTVar sync <**>
                                                        pure (subtract 1) **>
                                                        writeTVar sync)
          atomically $ waitZero sync
          vs <- atomically $ T.sequenceA $ map readTVar ts
          print $ sum vs
         
waitZero ::  TVar Int -> STM ()    
waitZero tvar = do
  a <- readTVar tvar 
  if a /= 0  
    then retry
    else return ()
    
{-
waitZero tvar = do
  readTVar tvar >>= (\a -> if a == threads then retry else return ())  
 -}          

perform :: Int -> [TVar Int] -> IO ()
perform 0 _ = return ()
perform n tvs = do
  positions <- sequence (replicate changes $ randomRIO (0,tvars-1))
  atomically $ 
      let tvs' = map (tvs!!) positions
       in foldr (*>) (pure ()) (map (\tv -> readTVar tv <**> pure (+1) **> writeTVar tv) tvs')
  --print "CCCC"
  perform (n-1) tvs

{-
performM :: Int -> [C.TVar Int] -> IO ()
performM 0 _ = return ()
performM n tvs = do
  positions <- sequence (replicate changes $ randomRIO (0,tvars-1))
  M.atomically $ do 
      let tvs'   = map (tvs!!) positions
          fun tv = do a <- M.readTVar tv
                      M.writeTVar tv (a+1)
      sequence $ map fun tvs'
  print "MMMM"
  performM (n-1) tvs

-}
