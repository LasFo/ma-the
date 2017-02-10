import qualified Account
import qualified GetTest
import qualified ModTest
import qualified SwapTest
import qualified BugTest
import qualified EvalTest
import qualified StmTest

main = do
  putStrLn "------------Account-----------" 
  Account.main
  putStrLn "------------GetTest-----------" 
  GetTest.main
  putStrLn "------------ModTest-----------" 
  ModTest.main
  putStrLn "-----------SwapTest-----------" 
  SwapTest.main
  putStrLn "------------BugTest-----------" 
  BugTest.main
  putStrLn "-----------EvalTest-----------" 
  EvalTest.main
  putStrLn "------------StmTest-----------"
  putStrLn $ "Desired output: " ++ (show (StmTest.threads * StmTest.iter * StmTest.changes))  
  StmTest.main
