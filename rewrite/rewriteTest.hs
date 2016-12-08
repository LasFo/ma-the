import STM
import Control.Concurrent (forkIO)

main = do
  tv1 <- atomically $ newTVar "Welt"
  forkIO $ loopRead tv1
  loopWrite tv1
  
  
decTVar t1 =  
  readTVar t1 >>= (\a -> writeTVar' t1 (tail a))

incTVar t1 = 
  readTVar t1 >>= (\a -> writeTVar' t1 (inc a))

inc str = 'W' : str
loopRead t1 = do
  atomically $ do incTVar t1
                  decTVar t1
  print "working..."
  loopRead t1 

loopWrite t1 = do
   atomically $ readTVar t1 >>= writeTVar' t1 
   loopWrite t1


