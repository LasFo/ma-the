import STM 

main = do 
  [t1,t2,t3] <- atomically $ sequenceA $ map newTVar [42,73,15]
  atomically $ swap t1 t2 t3
  res <- atomically $ sequenceA $ map readTVar [t1,t2,t3]
  print res

swap t1 t2 t3 = do
  readTVar t3 **> writeTVar t1
  readTVar t2 **> writeTVar t3
