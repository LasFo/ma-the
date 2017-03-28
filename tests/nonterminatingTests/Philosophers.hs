
import STM
import Control.Concurrent
import System.Random

{-The most basic test case for synchronization tools:
 - The dining Philosophers-}

philosophers = 5

type Stick = TVar Bool

philosopher :: Int -> Stick -> Stick -> IO ()
philosopher n sl sr = do
  putStrLn $ "Phil" ++ show n ++ ": thinking..."
  randomDelay
  atomically $ takeStick sl >> takeStick sr
  putStrLn $ "Phil" ++ show n ++ ": eating..."
  randomDelay
  atomically $ putStick sl >> putStick sr
  philosopher n sl sr

randomDelay :: IO ()
randomDelay = do
  waitTime <- getStdRandom (randomR (1,1000000))
  threadDelay waitTime

takeStick :: Stick -> STM ()
takeStick s = do
  avail <- readTVar s
  if avail
     then writeTVar s False
     else retry

putStick :: Stick -> STM ()
putStick = flip writeTVar True

main = do 
  sticks <- atomically $ sequence $ replicate philosophers (newTVar True)
  mapM_ forkIO $ makePhils philosophers sticks
  getLine

makePhils :: Int -> [Stick] -> [IO ()]
makePhils 0 _        = []
makePhils n (x:y:xs) = philosopher n x y : makePhils (n-1) (y:xs ++ [x])
