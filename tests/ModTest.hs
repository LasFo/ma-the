module ModTest where

import STM
import Control.Concurrent (forkIO)
import Data.List ((!!))
import System.Random
import qualified Data.Traversable as T

--This is a test case for the basic functions of STM
--It creates three TVars initialized with:
--t0 = 42
--t1 = 73
--t2 = 0
--After that two extra threads are created. 
--The first increments repeatedly t2.
--The second add repeatedly t0 to t2.
--And the main thread repeatedly add t1 to t2.
--After the main threa finished, it waits for the other threads 
--to terminate and then prints the value of t2.
--If the STM implementation is correct, the result of any run is the same.
--If it is not correct the result may vary or the test case deadlocks.

main = do
    putStrLn "Desired output: 125000."
    sync <- atomically $ newTVar 2 
    tvs <- atomically $ T.sequenceA $ map newTVar [42,73,0]
    forkIO $ do perform 10000 (inc (tvs !! 2))
                atomically $ (readAndModify sync (subtract 1)) **> writeTVar sync 
    forkIO $ do perform 1000 (add (head tvs) (tvs !! 2) (tvs !! 2))
                atomically $ (readAndModify sync (subtract 1)) **> writeTVar sync 
    perform 1000 $ add (tvs !! 1) (tvs !! 2) (tvs !! 2)
    atomically $ waitForZero sync
    a <- atomically $ readTVar (tvs !! 2)
    print a
 where inc t1       = writeTVar t1 (readAndModify t1 (+1))
       add :: TVar Int -> TVar Int -> TVar Int -> STM ()
       add t1 t2 t3 = 
           pure (+) <*> 
           readTVar t1 <*> 
           readTVar t2 **>
           writeTVar t3
       
waitForZero :: TVar Int -> STM ()
waitForZero tv = 
  readTVar tv >>= 
  (\val -> if val == 0
             then return ()
             else retry)
 

readAndModify :: TVar a -> (a -> a) -> STM a
readAndModify tv f = pure f <*> readTVar tv

perform :: Int -> STM a -> IO ()
perform 0 _  = return ()
perform n t1 = do
  atomically $ t1
  perform (n-1) t1

