module Account where
--A basic bank account implementation.
--This may serve as a basis for further test,
--but was implementated since it is a basic example
--for concurrent programming.


import STM


type Account = TVar Int


getBalance :: Account -> STM Int
getBalance = readTVar

withdraw :: Account -> Int -> STM ()
withdraw acc am = getBalance acc <**> 
                  pure (subtract am) 
                  **> writeTVar acc 

deposite :: Account -> Int -> STM ()
deposite acc am = getBalance acc <**> 
                  pure (+ am) **> 
                  writeTVar acc 

transfer :: Account -> Account -> Int -> STM ()
transfer src dst am = withdraw src am *> deposite dst am

main = do
  putStrLn "Desired output: [80,50,170]"
  [acc1,acc2,acc3] <- atomically $ sequenceA $ replicate 3 (newTVar 100)
  atomically $ transfer acc2 acc3 20 *> 
               transfer acc1 acc2 30 *> 
               transfer acc3 acc1 40 *>
               transfer acc1 acc3 30 *>
               transfer acc2 acc3 60
  vals <- atomically $ sequenceA $ map getBalance [acc1,acc2,acc3]
  print vals
