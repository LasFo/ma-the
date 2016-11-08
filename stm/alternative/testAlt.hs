--import Control.Concurrent.STM
import STM
import Control.Applicative      
import Debug.Trace

main = do
  tv <- atomically $ newTVar 1
  res <- atomically $ do
        some $ test tv
 --       newtrans tv
  atomically $ readTVar tv
  print res 

test tv = do 
  a <- readTVar tv
  if (trace (show a) (a > 5))
     then retry
     else do writeTVar tv (a + 1)
             readTVar tv

test2 tv = do
  a <- readTVar tv
  if a > 3 
    then retry
    else return [a+10]

newtrans tv =
  (fmap (:) (test tv)) <*> ((newtrans tv <|> pure []))

somce v = somce_v
  where mancy_v = somce_v <|> (pure [])
        somce_v = (fmap (:) v) <*> mancy_v 
