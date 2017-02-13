module BugTest where

import STM

main = do 
  t1 <- atomically $ newTVar 0
  t2 <- atomically $ newTVar 0
  a <- atomically $ transaction t1 t2
  putStrLn "Desired output: 0"
  print a


transaction :: TVar Int -> TVar Int -> STM Int
transaction t1 t2 = do
   a <- readTVar t1
   writeTVar t1 42
   writeTVar t2 73
   return a 
  
