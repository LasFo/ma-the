import STM 
import Control.Concurrent (forkIO)

main = do
  tv1 <- atomically $ newTVar 0
  tv2 <- atomically $ newTVar 0
  forkIO $ (readLoop tv1 tv2) >> print "finished"
  writeLoop tv1

readLoop :: TVar Int -> TVar Int -> IO ()
readLoop tv1 tv2 = atomically $ do
  val1 <- readTVar tv1
  val2 <- readTVar tv2
  if val1 > 0
    then if val2 > 0 then return () else retry
    else retry
   
writeLoop :: TVar Int -> IO()
writeLoop tv = do
  atomically $ do val <- readTVar tv
                  writeTVar tv (val + 1)
  writeLoop tv
