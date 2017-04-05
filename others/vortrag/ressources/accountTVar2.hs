transfer :: Account -> Account -> Int -> STM ()
transfer src dst am = do
  readTVar src <**> pure (- am) **> writeTVar src 
  readTVar dst <**> pure (+ am) **> writeTVar dst
