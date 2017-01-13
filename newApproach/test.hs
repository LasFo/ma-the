import STM

main = do
  tv1 <- atomically $ newTVar 0
  tv2 <- atomically $ newTVar 0
  atomically $ trans tv1 tv2
  a <- atomically $ mapM readTVar [tv1,tv2]
  print a

trans t1 t2 = readTVar t1 <**> pure (+ 5) <<* writeTVar t1 *>> writeTVar t2

 
