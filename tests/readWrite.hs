import STM
import Control.Concurrent

main = do 
  [t1,t2,sync] <- atomically $ mapM newTVar [0,0,(- 2)]
  forkIO $ perform t2 t1 10000 (+1) >> (atomically $ readTVar sync >>= writeTVar sync . (+1)) 
  forkIO $ perform t1 t2 10000 (subtract 1) >> (atomically $ readTVar sync >>= writeTVar sync . (+1)) 
  atomically $ waitForZero sync
  val <- atomically $ mapM readTVar [t1,t2]
  print $ (val :: [Int])

waitForZero sync = do
  a <- readTVar sync
  if a == 0 
     then return ()
     else retry

perform :: TVar Int -> TVar Int -> Int -> (Int -> Int) -> IO ()
perform _  _  0    f = return ()
perform t1 t2 iter f = do
  atomically $ trans t1 t2 f
  perform t1 t2 (iter - 1) f

trans t1 t2 f = do 
  a <- readTVar t1
  writeTVar t2 (f a)
