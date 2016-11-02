import STM
import Control.Concurrent (forkIO)

main :: IO()
main = do
  [tv1,tv2] <- atomically $ sequence $ map newTVar [42,73]
  forkIO $ inc tv1 tv2
  inc tv2 tv1
  tvs <- atomically $ sequence $ map readTVarLazy [tv1,tv2]
  vals <- sequence $ map evalLazyVal tvs
  print $ consist vals

consist :: [Int] -> Bool
consist (x:y:_) = x + y == 99999

inc :: TVar Int -> TVar Int -> IO()
inc tv1 tv2 = do
   stop <- atomically $ do 
          orElse (do {increment tv1; check tv1 tv2}) (return True)
   if stop 
       then return ()
       else inc tv1 tv2

increment :: TVar Int -> STM ()
increment tv = do 
   a <- readTVarLazy tv
   writeTVarLazy tv (pure (+1) <*> a)

check :: TVar Int -> TVar Int -> STM Bool
check tv1 tv2 = do
  val1 <- readTVar tv1 
  val2 <- readTVar tv2
  if val1 + val2 == 100000
      then retry
      else return False
