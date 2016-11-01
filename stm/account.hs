import STMC


type Account = TVar Int


getBalance :: Account -> STM Int
getBalance = readTVar

withdraw :: Account -> Int -> STM ()
withdraw acc am = writeTVar acc newBal
  where oldBal = getBalance acc
        newBal = pure (subtract am) <*> oldBal

deposite :: Account -> Int -> STM ()
deposite acc am = writeTVar acc newBal
  where oldBal = getBalance acc
        newBal = pure (+ am) <*> oldBal

transfer :: Account -> Account -> Int -> STM ()
transfer src dst am = withdraw src am <%> deposite dst am

main = do
  [acc1,acc2,acc3] <- atomically $ result $ replicate 3 (newTVar 100 (const True))
  atomically $ transfer acc2 acc3 20 <%> 
               transfer acc1 acc2 30 <%> 
               transfer acc3 acc1 40 <%>
               transfer acc1 acc3 10 <%>
               transfer acc2 acc3 10
  vals <- atomically $ result $ map getBalance [acc1,acc2,acc3]
  print vals
