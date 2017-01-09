module Main where

import Control.Concurrent
--import Control.Concurrent.STM
import GraphSTM
import Control.Applicative

import System.Random
import Data.List

threads = 5
iter    = 100
tvars   = 20
changes = 10

main = do sync <- atomically $ newTVar 0 
          ts <- atomically $ sequence $ replicate tvars (newTVar 0)
          sequence $ replicate threads (forkIO $ do
                                           perform iter ts
                                           atomically $ do newSync <- readAndModify sync (+1)
                                                           writeTVar sync newSync 
                                           print "fertig")
          getLine
          --atomically $ waitZero sync
          vs <- atomically $ mapM readTVar ts
          vals <- mapM cast vs
          print $ sum vals
{-         
waitZero tvar = 
  let sval = readAndModify tvar (subtract threads)
    in writeTVar tvar sval
-}

readAndModify :: TVar a -> (a -> a) -> STM (STMVal a) 
readAndModify tv f = do 
    val <- readTVar tv
    return (pure f <*> val)

perform :: Int -> [TVar Int] -> IO ()
perform 0 _ = return ()
perform n tvs = do
  positions <- sequence (replicate changes $ randomRIO (0,tvars-1))
  atomically $ do
      let tvs' = map (tvs!!) positions
      mapM_ (\tvar -> do val <- readAndModify tvar (+1)
                         writeTVar tvar val)
            tvs'
  perform (n-1) tvs




