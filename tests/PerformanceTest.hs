


import Control.Concurrent (forkIO)
import System.Random
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromJust)
import Control.Concurrent.STM
--import STMLA




threads     = 25
iterations  = 2000
tvars       = 50
rWRatio     = 7
writes      = 5

main = do
  sync <- atomically $ newTVar threads
  ts <- atomically $ sequence $ replicate tvars (newTVar 5)
  let tList = zip [1..] ts
      intmap = IM.fromList tList
  sequence $ replicate threads $ (forkIO $ do 
           perform iterations intmap
           atomically $ readTVar sync >>= (writeTVar sync) . (subtract 1))
  atomically $ waitZero sync
  putStrLn "finished" 

waitZero :: TVar Int -> STM ()
waitZero tvar = do
  a <- readTVar tvar
  if a /= 0
     then retry
     else return ()

perform :: Int -> IM.IntMap (TVar Int) -> IO ()
perform 0 _  = return ()
perform n im = do
  positions <- sequence $ replicate writes $ sequence (replicate rWRatio $ randomRIO (1,tvars))
  atomically $ trans positions im
  perform (n-1) im

trans :: [[Int]] -> IM.IntMap (TVar Int) -> STM ()
trans [] _        = return ()
trans (id:ids) im = do 
  let tvs = map fromJust $ map ((flip IM.lookup) im) id
  vals <- mapM readTVar tvs
  writeTVar (head tvs) (sum vals)
  trans ids im
     

