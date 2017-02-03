import STM
import Control.Concurrent 
import System.Random
{-Santa sleeps until wakened by either 9 reindeers or 3 of ten elves
Awakened by reindeers: deliver presents
Awakened by elves: shows them into his study
Reindeers > elves-}

data Gate = MkGate Int (TVar Int)

data Group = MkGroup Int (TVar (Int,Gate,Gate))

helper1 :: Group -> IO() -> IO()
helper1 group do_task = do 
  (in_gate,out_gate) <- joinGroup group
  passGate in_gate
  do_task
  passGate out_gate

meetInStudy :: Int -> IO ()
meetInStudy id = putStr $ "Elf " ++ show id ++ " meeting in the study\n"

deliverToys :: Int -> IO ()
deliverToys id = putStr $ "Reindeer " ++ show id ++ " delivering toys\n"

elf1, reindeer1 :: Group -> Int -> IO()
elf1 gp id = helper1 gp (meetInStudy id)
reindeer1 gp id = helper1 gp (deliverToys id)

newGate :: Int -> STM Gate
newGate n = do
  tv <- newTVar 0
  return (MkGate n tv)

passGate :: Gate -> IO()
passGate (MkGate n tv) = atomically $ do
  n_left <- readTVar tv
  check (n_left > 0)
  writeTVar' tv $ n_left - 1

operateGate :: Gate -> IO()
operateGate (MkGate n tv) = do
  atomically $ writeTVar' tv n
  atomically $ do n_left <- readTVar tv
                  check (n_left == 0)

newGroup :: Int -> IO Group
newGroup n = atomically $ do
  g1 <- newGate n
  g2 <- newGate n
  tv <- newTVar (n,g1,g2)
  return (MkGroup n tv)

joinGroup :: Group -> IO (Gate,Gate)
joinGroup (MkGroup n tv) = atomically $ do
  (n_left,g1,g2) <- readTVar tv
  check (n_left > 0)
  writeTVar' tv (n_left-1,g1,g2)
  return (g1,g2)

awaitGroup :: Group -> STM (Gate,Gate)
awaitGroup (MkGroup n tv) = do 
  (n_left,g1,g2) <- readTVar tv
  check (n_left == 0)
  new_g1 <- newGate n
  new_g2 <- newGate n
  writeTVar' tv (n,new_g1,new_g2)
  return (g1,g2)

elf :: Group -> Int -> IO ThreadId
elf gp id = forkIO (forever (do {elf1 gp id; randomDelay}))

reindeer :: Group -> Int -> IO ThreadId
reindeer gp id = do 
  forkIO (forever (do {reindeer1 gp id; randomDelay}))

forever :: IO() -> IO()
forever act = do 
  act
  forever act

randomDelay :: IO ()
randomDelay = do
  waitTime <- getStdRandom (randomR (1,1000000))
  threadDelay waitTime

santa :: Group -> Group -> IO()
santa elf_gp rein_gp = do
    putStr "----------------\n"
    (task,(in_gate,out_gate)) <- atomically (orElse
                                    (chooseGroup rein_gp "deliver toys")
                                    (chooseGroup elf_gp "meet in my study"))
    putStr $ "Ho! Ho! Ho! let's " ++ task ++ "\n"
    operateGate in_gate
    operateGate out_gate
    santa elf_gp rein_gp
  where chooseGroup :: Group -> String -> STM (String, (Gate,Gate))
        chooseGroup gp task = do gates <- awaitGroup gp
                                 return (task, gates) 

main = do
  print "Desired output: Santa delivers presents or teachings without end."
  print "Compilation may be needed to produce readable output."
  reinGroup <- newGroup 9
  elvesGroup <- newGroup 3
  mapM_ (reindeer reinGroup) [1..9]
  mapM_ (elf elvesGroup) [10..19]
  santa elvesGroup reinGroup
