import STM 

main = do 
  [t1,t2,t3] <- atomically $ sequenceA $ map newTVar [42,73,15]
  atomically $ rotate t1 t2 t3
  res <- atomically $ sequenceA $ map readTVar [t1,t2,t3]
  print res

rotate t1 t2 t3 = do
  readTVar t3 **> writeTVar t1
  readTVar t2 **> writeTVar t3


main2 = do 
  [t1,t2] <- atomically $ sequenceA $ map newTVar [42,73]
  atomically $ swap t1 t2
  res <- atomically $ sequenceA $ map readTVar [t1,t2]
  print res


swap t1 t2 = do
  v1 <- eval (readTVar t1) 
  readTVar t2 **> writeTVar t1
  writeTVar t2 v1
