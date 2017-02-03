import STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import System.IO.Unsafe

type Line = [[Int]]

initialLine = [
     [1,0,1,0],
     [0,1,1,0],
     [1,1,1,0],
     [1,1,0,0]]

main = do
   lines <- atomically $ sequence $ map (sequence . (map newTVar)) initialLine
   let completeLines = construct lines
   sequence $ map (forkIO . stepper) completeLines
   loopPrint completeLines

construct :: [[TVar Int]] -> [[TVar Int]]
construct l@(x:_) = (((last . last) l) : x) : newL l
  where newL (x:y:[]) =  ((last x) : y) : []
        newL (x:y:ys) =  ((last x) : y) : newL (y:ys)

global = unsafePerformIO (newMVar ())

loopPrint :: [[TVar Int]] -> IO ()
loopPrint xs = do 
  vall <- atomically $ sequence $ map (sequence . (map readTVar)) xs 
  let vals = map init vall
  print vals
  print $ foldr (+) 0 $ map (foldr (+) 0) vals
  threadDelay 2000000
  --putMVar global ()
  loopPrint xs  

stepper :: [TVar Int] -> IO ()
stepper tvs = do 
  atomically $ do vals <- sequence $ map readTVar tvs
                  sequence $ step (head vals) vals tvs
  --threadDelay 50000
  --takeMVar global
  stepper tvs

step :: Int -> [Int] -> [TVar Int] -> [STM ()]
step _   []     _    = []
step acc (x:[])   (y:[])
   | acc /= x  = (writeTVar' y acc) : []
   | otherwise = []
step acc (o:0:xs) (y:ys) 
   | acc /= o  = (writeTVar' y acc) : step 0 (0:xs) ys
   | otherwise = step 0 (0:xs) ys
step acc (n:m:xs) (y:ys) 
   | acc /= n  = step (m-1) (m:xs) ys
   | otherwise = (writeTVar' y (acc + 1)) : step (m-1) (m:xs) ys 
