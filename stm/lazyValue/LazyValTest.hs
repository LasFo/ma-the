import STM
import Control.Concurrent (forkIO)

main :: IO()
main = do
  [tv1,tv2,tv3,tv4] <- atomically $ sequence $ map newTVar [42,73,0,0]
  sequence_ $ map forkIO $ [inc tv2 tv4, inc tv1 tv4]
  add tv1 tv2 tv3 tv4
  tvs <- atomically $ sequence $ map readTVarLazy [tv1,tv2,tv3,tv4]
  vals <- sequence $ map evalLazyVal tvs
  print $ consist vals

consist :: [Int] -> Bool
consist (x:y:z:_) = x + y == z

inc :: TVar Int -> TVar Int -> IO()
inc tv1 tv2 = do
   continue <- atomically $ do 
       res <- readTVar tv2
       if res == 1 
           then return False
           else do
             a <- readTVarLazy tv1
             writeTVarLazy tv1 (pure (+1) <*> a)
             return True
   if continue
       then inc tv1 tv2
       else return ()


add :: TVar Int -> TVar Int -> TVar Int -> TVar Int -> IO()
add tv1 tv2 tv3 tv4 = do
  fin <- atomically $ do 
        addShift tv1 tv2 tv3
        check tv3 tv4
  if fin 
      then return ()
      else add tv1 tv2 tv3 tv4

addShift :: TVar Int -> TVar Int -> TVar Int -> STM ()
addShift tv1 tv2 tv3 = do 
  v1 <- readTVarLazy tv2
  v2 <- readTVarLazy tv1
  let sum = pure (+) <*> v1 <*> v2
  writeTVarLazy tv3 sum


check :: TVar Int -> TVar Int -> STM Bool
check tv1 tv2 = do
  val <- readTVar tv1 
  if val < 100000
      then return False
      else do writeTVar tv2 1
              return True

