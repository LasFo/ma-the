import STMWSL
import Control.Concurrent

main = do 
  [t1,t2] <- atomically $ mapM newTVar [1,0]
  forkIO $ atomically $ readTVar t1 >>= writeTVar t2 
  atomically $ readTVar t2 >>= writeTVar t2
  a <- atomically $ readTVar t2
  print a
