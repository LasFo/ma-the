example = do
   writeTVar t2 (readTVar t1)
   a <- readTVar t2
   if a ...
