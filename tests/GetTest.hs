module GetTest where

import STM
import System.IO.Unsafe


main = do 
  putStrLn "Desired outputs are given per transaction."
  t0 <- atomically $ newTVar 5
  t1 <- atomically $ newTVar 0
  putStrLn "Transaction 1: no output"
  atomically $ swap t0 t1
  putStrLn "Transaction 2: [0,5]"
  val1 <- atomically $ mapM readTVar [t0,t1]
  print val1
  putStrLn "Transaction 3: no output"
  atomically $ share t0 t1
  putStrLn "Transaction 4: [evaled1,1]"
  val2 <- atomically $ mapM readTVar [t0,t1]
  print val2

--eval is now safe, and does not lead to multiple IOReads
--Every transaction reads every TVar (on IO level) at most once.
swap t0 t1 = do
  a <- eval $ readTVar t0
  readTVar t1 **> writeTVar t0
  writeTVar t1 a

transaction t0 t1 = do
  readTVar t0 <**> pure (+1) **> writeTVar t0
  readTVar t0 <**> pure (+1) **> writeTVar t0

share :: TVar Int -> TVar Int -> STM ()
share t0 t1 = do
  val <- eval $ readTVar t0
  let newVal = fmap (unsafePerformIO $ putStr "evaled" >> return (+1)) val
  writeTVar t0 newVal
  writeTVar t1 newVal 
