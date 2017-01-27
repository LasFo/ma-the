import STM 

main = do
  tv <- atomically $ newTVar 0
  printLoop tv


printLoop :: TVar Int -> IO ()
printLoop tv = do
  val <- atomically $ readTVar tv
  print val
  printLoop tv
