module Main where

import Control.Concurrent
--import Control.Concurrent.STM
import STMC

import System.Random
import Data.List

threads = 50 
iter    = 1000
tvars   = 20
changes = 10

main = do sync <- atomically $ newTVar 0 (>= 0)
          ts <- atomically $ result $ replicate tvars (newTVar 0 (>= 0))
          sequence $ replicate threads (forkIO $ do
                                           perform iter ts 
                                           atomically $ writeTVar sync (readAndModify sync (+ 1)))
          --getLine
          atomically $ waitZero sync
          vs <- atomically $ result $ map readTVar ts
          print $ sum vs
          
waitZero tvar = 
  let sval = readAndModify tvar (subtract threads)
    in writeTVar tvar sval

readAndModify :: TVar a -> (a -> a) -> STM a 
readAndModify tv f = res
  where val = readTVar tv
        res = modifyVal val f 

perform :: Int -> [TVar Int] -> IO ()
perform 0 _ = return ()
perform n tvs = do
  positions <- sequence (replicate changes $ randomRIO (0,tvars-1))
  atomically $ 
      let tvs' = map (tvs!!) positions
          fun tv = writeTVar tv (readAndModify tv (+1))
       in foldr (<%>) (result []) (map fun tvs')
  perform (n-1) tvs




