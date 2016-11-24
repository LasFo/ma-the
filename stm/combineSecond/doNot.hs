main = do 
  readTVar tv <**> pure (+1) **> writeTVar tv 
  x <- readTVar tv2
  if x < 10 then do writeTVar tv2 0
             else writeTVar tv2 (x+1)
