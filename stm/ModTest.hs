import STMC
import Control.Concurrent (forkIO)
import Data.List ((!!))
import System.Random

main = do
    sync <- atomically $ newTVar 0 (>= 0)
    tvs <- atomically $ result $ map (flip newTVar (> 3)) [42,73,0]
    forkIO $ do perform 1000 (inc (tvs !! 2))
                atomically $ writeTVar sync (readAndModify sync (+ 1))
    forkIO $ do perform 100 (add (head tvs) (tvs !! 2) (tvs !! 2))
                atomically $ writeTVar sync (readAndModify sync (+ 1))
    perform 100 $ add (tvs !! 1) (tvs !! 2) (tvs !! 2)
    atomically $ writeTVar sync (readAndModify sync (subtract 2))
    a <- atomically $ readTVar (tvs !! 2)
    print a

 where inc t1       = writeTVar t1 (readAndModify t1 (+1))
       add :: TVar Int -> TVar Int -> TVar Int -> STM ()
       add t1 t2 t3 = 
         let v1  = readTVar t1
             v2  = readTVar t2
             res = pure (+) <*> v1 <*> v2 
           in writeTVar t3 res
       
 

readAndModify :: TVar a -> (a -> a) -> STM a
readAndModify tv f = pure f <*> readTVar tv

perform :: Int -> STM a -> IO ()
perform 0 _  = return ()
perform n t1 = do
  atomically $ t1
  perform (n-1) t1

