module EvalTest where

import STM
import System.IO.Unsafe

--this proves that the evaluation of the function is not demanded before
--the actual value is demanded even though the IORead if performed in the
--commit phase
main = do
  putStrLn "Desired output: 'Inc now' should appear after 'Inc trans performed'. The last output is 1"
  tv <- atomically $ newTVar 0 
  tv2 <- atomically $ newTVar 0 
  atomically $ readTVar tv2 >> readTVar tv <**> pure unsafeInc **> writeTVar tv 
  putStrLn "Inc trans performed"
  a <- atomically $ readTVar tv
  print a





unsafeInc :: Int -> Int
unsafeInc = unsafePerformIO $ putStrLn "Inc now" >> return (+1)

