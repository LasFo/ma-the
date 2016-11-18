module STMMy 
           (STM(STM), STMResult(Success), TVar(TVar), 
            newTVar, readTVar, writeTVar, 
            atomically, retry, orElse, proc,
            always, alwaysSucceeds) where

import qualified Data.IntMap.Lazy as IntMap
import Prelude
import Control.Concurrent
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce
import Data.List (sortBy,delete)
import Data.Maybe (isNothing)

-----------------------
-- The STM interface --
-----------------------

type ID = Int

-- The STM monad itself
data STM a = STM (StmState -> IO (STMResult a))

instance Monad STM where
  (STM tr1)  >>= k = STM (\state -> do
                          valid <- tryTakeMVar $ retryMVar state
                          if isNothing valid
                            then do stmRes <- tr1 state
                                    case stmRes of
                                      Success newState a -> 
                                        let (STM tr2) = k a in
                                        tr2 newState
                                      Retry newState -> return (Retry newState)
                                      InValid newState -> return (InValid newState)
                            else return (InValid state)
                         )
  return x      = STM (\state -> return (Success state x))

-- Usually it is more efficient to perform IO actions then iterate through list.
-- We use Lists for collecting writtenValues.
-- A Data.Map should be more efficient.
data StmState = TST {touchedTVars  :: [(ID,IO(IO()))], -- not used, but collected for alternative locking
		     isValid       :: IO Bool,
                     writtenValues :: IntMap.IntMap ((),IO()), 
                     newTVars      :: [ID],
		     notifys       :: IO (),
                     retryMVar     :: MVar (),
                     subbed        :: [MVar [MVar ()]],
                     newInv        :: [STM ()]} 

data STMResult a = Retry StmState
	         | InValid StmState
		 | Success StmState a

initialState :: IO StmState
initialState = do
  rMVar <- newEmptyMVar --werden die wieder geloescht??
  return (TST {touchedTVars  = [],
	       isValid       = return True,
	       writtenValues = IntMap.empty,
               newTVars      = [],
	       notifys       = return (),
               retryMVar     = rMVar,
               subbed        = [],
               newInv        = []}) 

-- Transactional variables
data TVar a = TVar (MVar a)  -- global TVar itself. 
                   ID                -- TVar identifier
                   (MVar [MVar ()])  -- wait queue on retry

newTVar   :: a -> STM (TVar a)
newTVar v = STM (\stmState -> do
                    id <- getGlobalId
                    newTVarVal <- newMVar v 
                    newWaitQ <- newMVar []
                    let tVar = TVar newTVarVal id newWaitQ 
		    return (Success stmState tVar))

readTVar :: TVar a -> STM a
readTVar (TVar tVarRef id waitQ) = STM (\stmState -> do
      case IntMap.lookup id (writtenValues stmState) of
             Just (v,_) -> do
               return (Success stmState (unsafeCoerce v))
             Nothing -> do
               queue <- takeMVar waitQ
               putMVar waitQ $ retryMVar stmState:queue     
               val <- readMVar tVarRef
               let newState = stmState{touchedTVars = case lookup id (touchedTVars stmState) of
                                                        Just _  -> touchedTVars stmState
                                                        Nothing -> (id,io tVarRef):touchedTVars stmState,
                                       subbed = waitQ : subbed stmState}
               return (Success newState val))

--using io actions to collect locks for tvars of different types in one collection
io :: MVar a -> IO(IO())
io tv = do tid <- myThreadId
           a <- takeMVar tv
           return (putMVar tv a)


writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tVarRef id waitQ) v = STM (\stmState -> do
      do let newState =
               stmState{touchedTVars = case lookup id (touchedTVars stmState) of
                                         Just _  -> touchedTVars stmState
                                         Nothing ->  (id,io tVarRef):touchedTVars stmState,
                        writtenValues = IntMap.insert id 
                                                      (unsafeCoerce v,do
                                                        putMVar tVarRef v)
                                                      (writtenValues stmState),
                        notifys = notifys stmState >> fNotify waitQ}
         return (Success newState ()))

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
	InValid newState-> do
          rMVar <- newEmptyMVar 
          let reState = state{retryMVar = rMVar}
          atomically' stmAction reState
        Success nState res -> do
          (newState,invB) <- check nState
          let sorted       = sortBy (\a b -> compare (fst a) (fst b)) $ touchedTVars newState
              (ids,locker) = unzip sorted
          unlocker <- sequence locker
          valid <- tryTakeMVar $ retryMVar newState --check validity
          if isNothing valid && invB
            then do
              when (not (null (newInv newState))) 
                   (do invs <- takeMVar invariant
                       putMVar invariant (extendInv invs (newInv newState))) 
              notifys newState
              let (keys,vals) = unzip (IntMap.toList (writtenValues newState))
              sequence_ $ map snd vals
              let unlocks = filt ids unlocker keys 
              sequence_ unlocks
              return res
            else do
              sequence_ unlocker
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
             Retry newState -> stm2 stmState{isValid = isValid newState}
  	     _              -> return stm1Res)

alwaysSucceeds :: STM a -> STM ()
alwaysSucceeds inv = do
  inv >> return ()
  STM (\stmState -> do
    let newState = stmState{newInv = (inv >> return()) : newInv stmState}
    return (Success newState ()))

always :: STM Bool -> STM ()
always inv = alwaysSucceeds $ inv >>= prove
  where prove b = if b then return () else retry


-------------------
-- Miscellaneous --
-------------------

extendInv :: STM () -> [STM ()] -> STM ()
extendInv invs []       = invs
extendInv invs (iv:ivs) = extendInv (iv >> invs) ivs

check :: StmState -> IO (StmState,Bool)
check state = do
    invs <- readMVar invariant
    res <- startSTM invs state
    case res of
      Success newState _  -> checkNew (newInv newState) newState
      _                   -> return (state,False)
  where checkNew []         nstate = return (nstate,True)
        checkNew (inv:invs) nstate = do
          res <- startSTM inv nstate
          case res of
            Success newState  _ -> checkNew invs newState   
            _                   -> return (nstate,False)


invariant :: MVar (STM ())
invariant = unsafePerformIO (newMVar (return()))

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

globalLock :: MVar (IntMap.IntMap (MVar ())) 
globalLock = unsafePerformIO (newMVar IntMap.empty)

{-# NOINLINE globalCount #-}             
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

unsub :: [MVar [MVar ()]] -> MVar () -> IO()
unsub (x:xs) mv = do
  val <- takeMVar x 
  putMVar x (delete mv val)
  unsub xs mv
unsub []     _  = return ()
