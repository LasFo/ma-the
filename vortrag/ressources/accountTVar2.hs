type Account = TVar Int

transfer :: Account -> Account -> Int -> STM ()
transfer src dst am = do
  writeTVar src ((readTVar src) - am)
  writeTVar dst ((readTVar dst) + am)

