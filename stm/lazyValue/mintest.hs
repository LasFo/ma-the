import STM


main = do
   tv <- atomically $ newTVar 0
   atomically $ do a <- readTVarLazy tv
                   let inc = (pure (+ 1)) <*> a
                   writeTVarLazy tv inc
   a <- atomically $ readTVar tv
   print a 
