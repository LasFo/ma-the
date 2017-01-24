import STM
import Control.Concurrent
import System.IO.Unsafe

main = do [t1,t2] <- atomically $ sequence [newTVar 0, newTVar 0]
          forkIO (loopCheck t1 t2)
          increment t1 t2

loopCheck :: TVar Int -> TVar Int -> IO()
loopCheck t1 t2 = do
  val <- atomically $ chek t1 t2
  print val
  loopCheck t1 t2

chek :: TVar Int -> TVar Int -> STM Int
chek t1 t2 = do
  val1 <- readTVar t1
 -- unsafePerformIO $ do yield
  --                     return (return 2)
  val2 <- readTVar t2
  if val1 /= val2 
    then error "The two values are different"
    else return val1

increment :: TVar Int -> TVar Int -> IO()
increment t1 t2 = do atomically $ do val1 <- readTVar t1
                                     val2 <- readTVar t2
                                     writeTVar t1 $ pure (val1 + 1)
                                     writeTVar t2 $ pure (val2 + 1)
                     increment t1 t2
 


