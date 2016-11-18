module TVar 
    (TVar(TVar), ID, globalLock, globalCount,
    getGlobalId, fNotify, lock) where 

import Control.Concurrent.MVar 
import System.IO.Unsafe (unsafePerformIO)

type ID = Int

data TVar a = TVar (MVar a)           --value
                   ID                 --identifier
                   (MVar [MVar ()])   --waiting queue
                   (MVar (a -> Bool)) --constraint
                   (MVar ())          --lock

lock :: MVar () -> IO (IO())
lock mv = do
  takeMVar mv
  return (putMVar mv ())
                                
globalLock :: MVar ()
globalLock = unsafePerformIO (newMVar ())

globalCount :: MVar Int
globalCount = unsafePerformIO (newMVar 0)

getGlobalId :: IO Int
getGlobalId = do
  num <- takeMVar globalCount
  putMVar globalCount (num+1)
  return num

fNotify :: MVar [MVar ()] -> IO()
fNotify waitQ = do
  queue <- takeMVar waitQ
  mapM_ (flip tryPutMVar ()) queue
  putMVar waitQ []

                   
