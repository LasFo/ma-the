module Main where

import Control.Concurrent
--import Control.Concurrent.STM
import qualified STMC as C
import qualified STMM as M

import System.Random
import Data.List

threads = 5
iter    = 10000
tvars   = 20
changes = 10

main = do sync <- M.atomically $ M.newTVar (threads) (>= 0)
          ts <- C.atomically $ C.result $ replicate tvars (C.newTVar 0 (>= 0))
          sequence $ replicate threads (forkIO $ do
                                           perform iter ts 
                                           C.atomically $ C.writeTVar sync 
                                                           (readAndModify sync (subtract 1)))
          {-sequence $ replicate threads (forkIO $ do
                                           performM iter ts 
                                           C.atomically $ C.writeTVar sync 
                                                           (readAndModify sync (subtract 1)))
          --getLine-}
          M.atomically $ waitZero sync
          vs <- C.atomically $ C.result $ map C.readTVar ts
          print $ sum vs
         
--for this function monadic version is used          
waitZero tvar = do
  a <- M.readTVar tvar 
  if a > 0 
    then M.retry
    else return ()
          

readAndModify :: C.TVar a -> (a -> a) -> C.STM a
readAndModify tv f = res
  where val = C.readTVar tv
        res = (C.<*>) (C.pure f) val 

--here the non monadic version is used because the value itself does not matter
--that much
perform :: Int -> [C.TVar Int] -> IO ()
perform 0 _ = return ()
perform n tvs = do
  positions <- sequence (replicate changes $ randomRIO (0,tvars-1))
  C.atomically $ 
      let tvs' = map (tvs!!) positions
          fun tv = C.writeTVar tv (readAndModify tv (+1))
       in foldr (C.<%>) (C.result []) (map fun tvs')
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
