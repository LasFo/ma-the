type Account = MVar Int

transfer :: Account -> Account -> Int -> IO ()
transfer src dst am = do
  a1 <- takeMVar src
  a2 <- takeMVar dst
  putMVar src (a1 - am)
  putMVar dst (a2 + am)

