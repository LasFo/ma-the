module Main where

import Control.Concurrent
import STMM
--import STMMy
--import Control.Concurrent.STM

import System.Random
import Data.List

threads = 50
iter = 1000
tvars = 20
changes = 10

main = do sync <- atomically $ newTVar threads (const True)
          ts <- atomically $ sequence $ replicate tvars (newTVar 0 (> 0))
          sequence $ replicate threads (forkIO $ do
                                           perform iter ts 
                                           atomically $ do
                                                    n <- readTVar sync
                                                    writeTVar sync (n-1))
          atomically $ waitZero sync
          vs <- atomically $ mapM readTVar ts
          print $ sum vs
          
waitZero tvar = do
  n <- readTVar tvar
  if n>0 then retry
         else return ()

perform 0 _ = return ()
perform n tvs = do
  positions <- sequence (replicate changes $ randomRIO (0,tvars-1))
  atomically $ do
      let tvs' = map (tvs!!) positions
      mapM_ (\tvar -> do v <- readTVar tvar
                         writeTVar tvar (v + 1)) tvs'
  perform (n-1) tvs




