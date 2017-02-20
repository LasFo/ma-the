
import System.Environment
import Control.Concurrent
--import Control.Concurrent.STM
import STMP

import qualified Data.Traversable as T
import System.Random
import Data.List

--This is the core test case for STM. It is also used for 
--timing test, since it is configurable.
--It depends on four variable, which can be adjusted to control the test:
--threads: is the number of threads which work parallel
--iter:    is the number of transactions which every thread executes
--tvars:   is the number of tvars the threads are working on
--changes: is the number of increment operations per transactions.
--At first the test spawns *tvars* with 0 initialized tvars. 
--Second the test spawns *threads* threads.
--Each thread knows ever tvars. Each thread performs *iter* times a transaction.
--Before each transaction the thread chooses randomly *changes* tvar(the same
--TVar may occur multiple times). After the TVars are chooses a transaction is 
--evoked, which increments these TVars.
--After every trancsaction is finished, all tvars are read and the values are summed.
--If the core of the STM implementation is correct the result will always be the same,
--namely: threads * iter * changes.
--Otherwhise the result may vary or the test case deadlocks.

--threads = 20
iter    = 1000
tvars   = 50 
changes = 20

main = do args <- getArgs
          let threads = read . head $ args
          sync <- atomically $ newTVar threads 
          ts <- atomically $ T.sequenceA $ replicate tvars (newTVar 0)
          sequence $ replicate threads (forkIO $ do
                                           performM iter ts 
                                           atomically $ readTVar sync >>= (writeTVar sync).(subtract 1))
          atomically $ waitZero sync
          val <- readMVar rollbacks
          putStrLn $ "Rollbacks: " ++ show val
          vs <- atomically $ T.sequenceA $ map readTVar ts
          print $ sum vs
         
waitZero ::  TVar Int -> STM ()    
waitZero tvar = do
  a <- readTVar tvar 
  if a /= 0  
    then retry
    else return ()
    
performM :: Int -> [TVar Int] -> IO ()
performM 0 _ = return ()
performM n tvs = do
  positions <- sequence (replicate changes $ randomRIO (0,tvars-1))
  atomically $ do 
      let tvs'   = map (tvs!!) positions
          fun tv = fmap (+1) (readTVar tv) >>= writeTVar tv 
      mapM_ fun tvs'
  performM (n-1) tvs


