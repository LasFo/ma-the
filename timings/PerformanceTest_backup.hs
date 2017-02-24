


import Control.Concurrent 
import System.Random
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromJust)
import Control.Concurrent.STM
--import STMP
--import STMLA
--import STMWSL1
--import STMWSL2
import System.Environment


threads     = 50
iterations  = 2000
tvars       = 20
rWRatio     = 5
--writes      = 5

main = do
  list <- getArgs
  let writes = read $ head list
  sync <- atomically $ newTVar threads
  ts <- atomically $ sequence $ replicate tvars (newTVar 5)
  let tList = zip [1..] ts
      intmap = IM.fromList tList
  mapM_ (uncurry forkOn) $ zip [0..]
           (replicate threads (perform iterations intmap writes >>
           (atomically $ readTVar sync >>= (writeTVar sync) . (subtract 1))))
  atomically $ waitZero sync
  putStrLn "finished" 

waitZero :: TVar Int -> STM ()
waitZero tvar = do
  a <- readTVar tvar
  if a /= 0
     then retry
     else return ()

perform :: Int -> IM.IntMap (TVar Int) -> Int -> IO ()
perform 0 _  _      = return ()
perform n im writes = do
  positions <- sequence $ replicate writes $ sequence (replicate rWRatio $ randomRIO (1,tvars))
  atomically $ trans positions im
  perform (n-1) im writes

trans :: [[Int]] -> IM.IntMap (TVar Int) -> STM ()
trans [] _        = return ()
trans (id:ids) im = do 
  let tvs = map fromJust $ map ((flip IM.lookup) im) id
  vals <- mapM readTVar tvs
  writeTVar (head tvs) (sum vals)
  trans ids im
     

