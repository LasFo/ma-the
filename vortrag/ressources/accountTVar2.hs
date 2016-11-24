transfer :: Account -> Account -> Int -> STM ()
transfer src dst am = do
  readTVar src <**> pure (subtract am) 
                **> writeTVar src 
  readTVar dst <**> pure (+ am) 
                **> writeTVar dst

