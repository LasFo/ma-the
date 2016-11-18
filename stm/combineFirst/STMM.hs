module STMM
           (STM(STM), STMResult(Success), TVar(TVar), 
            newTVar, readTVar, writeTVar, 
            atomically, retry, orElse, proc) where

import qualified Data.IntMap.Lazy as IntMap
import Prelude
import Control.Concurrent
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce
import Data.List (sortBy)
import Data.Maybe (isNothing)
import TVar 

-----------------------
-- The STM interface --
-----------------------

--type ID = Int

-- The STM monad itself
data STM a = STM (StmState -> IO (STMResult a))

instance Monad STM where
  (STM tr1)  >>= k = STM (\state -> do
                          stmRes <- tr1 state
                          case stmRes of
                            Success newState a ->
                               let (STM tr2) = k a in
                                 tr2 newState
                            Retry newState -> return (Retry newState)
                            
                            InValid -> return InValid
                       )
  return x      = STM (\state -> return (Success state x))

-- Usually it is more efficient to perform IO actions then iterate through list.
-- We use Lists for collecting writeSet.
-- A Data.Map should be more efficient.
data StmState = TST {touchedTVars  :: IntMap.IntMap (IO (IO())), 
                     writeSet :: IntMap.IntMap ((),IO((IO(),Bool))), 
  --                   newTVars      :: [ID],
                     --[(ID,((),IO()))], -- (id,(unsafeCoerced Value, Commit action))
		     notifys       :: IO (),
		     --wait          :: IO (),
                     retryMVar     :: MVar ()} --now used for notification 

data STMResult a = Retry StmState
	         | InValid
		 | Success StmState a

initialState :: IO StmState
initialState = do
  --atomicallyId <- getGlobalId
  rMVar <- newEmptyMVar --werden die wieder geloescht??
  return (TST {touchedTVars  = IntMap.empty,
	       writeSet = IntMap.empty,
    --           newTVars      = [],
	       notifys       = return (),
	       --wait          = return (),
               retryMVar     = rMVar}) 

-- Transactional variables
{-data TVar a = TVar (MVar a)  -- global TVar itself. 
                   ID                -- TVar identifier
                   (MVar [MVar ()])  -- wait queue on retry
-}
--werden diese wieder geloescht??
newTVar :: a -> (a -> Bool) -> STM (TVar a)
newTVar v c = STM (\stmState -> do
                    id <- getGlobalId
                    newTVarVal <- newMVar v
                    newCons <- newMVar c
                    newLock <- newMVar ()
                    newWaitQ <- newMVar []
                    let tVar = TVar newTVarVal id newWaitQ newCons newLock
		    return (Success stmState tVar))

readTVar :: TVar a -> STM a
readTVar (TVar mv id waitQ _ l) = STM (\stmState -> do
    val <- tryTakeMVar $ retryMVar stmState  --check if the transaction is valid
    if isNothing val
      then case IntMap.lookup id (writeSet stmState) of
             Just (v,_) -> do
               return (Success stmState (unsafeCoerce v))
             Nothing -> do
               queue <- takeMVar waitQ
               putMVar waitQ $ retryMVar stmState:queue     
               val <- readMVar mv
               let newState = stmState{touchedTVars = IntMap.insert id 
                                                                    (lock l)
                                                                    (touchedTVars stmState)}
               return (Success newState val)
      else return InValid)

--using io actions to collect locks for tvars of different types in one collection
{-io :: MVar a -> IO(IO())
io tv = do tid <- myThreadId
           a <- takeMVar tv
           return (putMVar tv a)
-}

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar mv id waitQ c l) v = STM (\stmState -> do
  val <- tryTakeMVar $ retryMVar stmState
  if isNothing val
    then 
      do let newState =
               stmState{touchedTVars = IntMap.insert id
                                                     (lock l)
                                                     (touchedTVars stmState),
                        writeSet = IntMap.insert id 
                                                      (unsafeCoerce v,do
                                                        c <- readMVar c
                                                        return (do {takeMVar mv; putMVar mv v},
                                                                c v))
                                                      (writeSet stmState),
                        notifys = notifys stmState >> fNotify waitQ}
         return (Success newState ())
    else return InValid)

-- Running STM computations
atomically :: STM a -> IO a
atomically stmAction = do
  iState <- initialState 
  atomically' stmAction iState
  where
    atomically' :: STM a -> StmState -> IO a
    atomically' stmAction state = do
      stmResult <- startSTM stmAction state
      case stmResult of
        Retry newState -> do
          takeMVar (retryMVar state)   -- suspend
          rMVar <- newEmptyMVar
          let reState = state{retryMVar = rMVar}
          atomically' stmAction reState
	InValid -> do
          rMVar <- newEmptyMVar 
          let newState = state{retryMVar = rMVar}
          atomically' stmAction newState
        Success newState res -> do
          unlock <- sequence $ map snd $ IntMap.toAscList $ touchedTVars newState
          (writes, valid1) <- validate $ writeSet newState
         -- let sorted       = sortBy (\a b -> compare (fst a) (fst b)) $ touchedTVars newState
         --     (ids,locker) = unzip sorted
         -- unlocker <- sequence locker
          valid2 <- tryTakeMVar $ retryMVar newState --check validity
          if isNothing valid2 && valid1
            then do
              writes
              notifys newState
              sequence_ unlock
              return res
            else do
              sequence_ unlock
              rMVar <- newEmptyMVar 
              let newState = state{retryMVar = rMVar}
              atomically' stmAction newState

retry  :: STM a
retry =
  STM (\stmState -> do
         return $ Retry stmState)

orElse :: STM a -> STM a -> STM a
orElse (STM stm1) (STM stm2) =
  STM (\stmState -> do
           stm1Res <- stm1 stmState
           case stm1Res of
             Retry newState -> stm2 stmState{touchedTVars = touchedTVars newState}
  	     _              -> return stm1Res)

-------------------
-- Miscellaneous --
-------------------

filt :: [ID] -> [IO()] -> [ID] -> [IO()]
filt []       [] [] = []
filt _        xs [] = xs
filt iden@(id:ids) act@(io:ios) w@(wid:wids)
  | id == wid = filt ids ios wids
  | id <  wid = io : (filt ids ios w)
  | id >  wid = filt iden act wids  
 
startSTM :: STM a -> StmState -> IO (STMResult a)
startSTM stmAct@(STM stm) state = stm state

proc :: IO a -> STM a
proc io = STM (\stmState -> do
                  res <- io
                  return (Success stmState res))

validate :: IntMap.IntMap ((), IO(IO(),Bool)) -> IO (IO(),Bool)
validate ws = validate' (return ()) $ map (snd . snd) $ IntMap.toList ws
  where validate' acc []     = return (acc, True)
        validate' acc (x:xs) = do 
                     (ac, valid) <- x
                     if valid 
                       then validate' (acc >> ac) xs
                       else return (return (),False) 
{-
globalLock :: MVar (IntMap.IntMap (MVar ())) 
globalLock = unsafePerformIO (newMVar IntMap.empty)

globalCount :: MVar Int --[ID]
globalCount  = unsafePerformIO (newMVar 0) --(newMVar [0..])

getGlobalId :: IO Int
getGlobalId = do
  num <- takeMVar globalCount 
  putMVar globalCount (num+1) --(tail nums)
  return num

(>>+) :: IO Bool -> IO Bool -> IO Bool
a1 >>+ a2 = do
  b <- a1
  if b then a2
       else return False

fNotify :: MVar [MVar ()] -> IO ()
fNotify waitQ = do
  queue <- takeMVar waitQ
  mapM_ (flip tryPutMVar ()) queue 
  putMVar waitQ []

replace :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replace k v [] = [(k,v)]
replace k v ((k1,v2):kvs) | k == k1   = (k,v):kvs
                          | otherwise = (k1,v2):replace k v kvs
-}
