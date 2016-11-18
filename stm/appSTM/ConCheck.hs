import STMC
import Control.Concurrent (forkIO)

main = do
  tv <- atomically $ newTVar 5 (<10)
  atomically $ inc tv <%>
               replaceConstraint tv (>6) <%> 
               inc tv
  perform 100 $ inc tv
  val <- atomically $ readTVar tv
  print val

inc :: TVar Int -> STM ()
inc t1 = writeTVar t1 (readAndModify t1 (+1))

readAndModify :: TVar a -> (a -> a) -> STM a
readAndModify tv f = pure f <*> readTVar tv

perform :: Int -> STM a -> IO ()
perform 0 _  = return ()
perform n t1 = do
  atomically $ t1
  perform (n-1) t1
   
