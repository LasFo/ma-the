module Main where

import Control.Concurrent
--import Control.Concurrent.STM
import STM 

import System.Random
import Data.List

threads = 50 
iter    = 100
tvars   = 20
changes = 10

main = do sync <- atomically $ newTVar threads 
          ts <- atomically $ sequence $ replicate tvars (newTVar 0)
          sequence $ replicate threads (forkIO $ do
                                           perform iter ts 
                                           atomically $ do a <- (readAndModify sync (subtract 1))
                                                           writeTVarLazy sync a)
          {-sequence $ replicate threads (forkIO $ do
                                           performM iter ts 
                                           C.atomically $ C.writeTVar sync 
                                                           (readAndModify sync (subtract 1)))
          --getLine-}
          atomically $ waitZero sync
          vs <- atomically $ sequence $ map readTVar ts
          print $ sum vs
          
waitZero tvar = do
  a <- readTVar tvar 
  if a > 0 
    then retry
    else return ()
          

readAndModify :: TVar a -> (a -> a) -> STM (LazyValue a)
readAndModify tv f = do
    a <- readTVarLazy tv
    return $ pure f <*> a

perform :: Int -> [TVar Int] -> IO ()
perform 0 _ = return ()
perform n tvs = do
  positions <- sequence (replicate changes $ randomRIO (0,tvars-1))
  atomically $ do
      let tvs' = map (tvs!!) positions
      sequence_ $ map (\tv -> do a <- readTVarLazy tv
                                 let inc = pure (+1) <*> a
                                 writeTVarLazy tv inc) 
                      tvs' 
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
