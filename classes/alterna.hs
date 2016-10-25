import Control.Applicative
import Control.Concurrent.STM

test = do
  tvs <- sequence $ map atomically $ map newTVar [1..5]
  v <- atomically $ orElse (many (proces tvs)) (return [4])
  print v

proces :: [TVar Int] -> STM Int
proces (x:(y:xs)) = do
  var <- readTVar x
  val <- readTVar y 
  if var < val
    then retry
    else return var
