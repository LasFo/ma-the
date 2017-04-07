type Account = MVar Int

transfer :: Account -> Account -> Int -> IO ()
transfer src dst am = do
  srcBal <- takeMVar src
  dstBal <- takeMVar dst
  putMVar src (srcBal - am)
  putMVar dst (dstBal + am)

