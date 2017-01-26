import STM

main = do 
  t1 <- atomically $ newTVar 0
  t2 <- atomically $ newTVar 0
  a <- atomically $ transaction t1 t2
  print a


transaction :: TVar Int -> TVar Int -> STM Int
transaction t1 t2 = do
   a <- eval $ readTVar t1
   writeTVar t1 $ pure 42
   writeTVar t2 $ pure 73
   a 
  
