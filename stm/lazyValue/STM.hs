module STM
           (STM(STM), STMResult(Success), TVar(TVar), 
            newTVar, readTVar, writeTVar, LazyValue,
            readTVarLazy, writeTVarLazy, (<*>), pure,
            atomically, retry, orElse, proc, evalLazyVal) where

import qualified Data.IntMap.Lazy as IntMap
import Prelude
import Control.Concurrent
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce
import Data.List (sortBy, unionBy)
import Data.Maybe (isNothing)
import Control.Applicative

-----------------------
-- The STM interface --
-----------------------

data LazyValue a = LV (IO a) [MVar [MVar ()]]

instance Functor LazyValue where
  fmap f (LV  v l) = LV (fmap f v) l

instance Applicative LazyValue where
  pure x = LV (return x) []
  (LV f (l1)) <*> (LV v l2) = LV (f <*> v) (unionBy (/=) l1 l2)

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
                                        InValid -> return InValid
                              else return InValid)
  return x         = STM (\state -> return (Success state x))


-- Usually it is more efficient to perform IO actions then iterate through list.
-- We use Lists for collecting normalWSet.
-- A Data.Map should be more efficient.
data StmState = TST {touchedTVars  :: [(ID,IO(IO()))], -- not used, but collected for alternative locking
		     isValid       :: IO Bool,
                     normalWSet    :: IntMap.IntMap ((),IO()),
                     lazyWSet      :: IntMap.IntMap ((),IO(IO())), 
                     newTVars      :: [ID],
		     notifys       :: IO (),
                     retryMVar     :: MVar ()} --now used for notification 

data STMResult a = Retry StmState
	         | InValid
		 | Success StmState a

initialState :: IO StmState
initialState = do
  --atomicallyId <- getGlobalId
  rMVar <- newEmptyMVar --werden die wieder geloescht??
  return (TST {touchedTVars  = [],
	       isValid       = return True,
	       normalWSet    = IntMap.empty,
               lazyWSet      = IntMap.empty,
               newTVars      = [],
	       notifys       = return (),
               retryMVar     = rMVar}) 

-- Transactional variables
data TVar a = TVar (MVar a)  -- global TVar itself. 
                   ID                -- TVar identifier
                   (MVar [MVar ()])  -- wait queue on retry
                   (MVar ())    --lock

--werden diese wieder geloescht??
newTVar   :: a -> STM (TVar a)
newTVar v = STM (\stmState -> do
                    id <- getGlobalId
                    newTVarVal <- newMVar v 
                    newWaitQ <- newMVar []
                    newLock <- newMVar ()
                    let tVar = TVar newTVarVal id newWaitQ newLock
		    return (Success stmState tVar))

readTVar :: TVar a -> STM a
readTVar (TVar tVarRef id waitQ l) = STM (\stmState -> do
      case IntMap.lookup id (normalWSet stmState) of
        Just (v,_) -> do
          return (Success stmState (unsafeCoerce v))
        Nothing -> 
          case IntMap.lookup id (lazyWSet stmState) of
            Just (v,_) -> do let LV act wqs = unsafeCoerce v
                             enter wqs (retryMVar stmState)
                             val <- act
                             return (Success stmState val)
            Nothing -> do
              queue <- takeMVar waitQ
              putMVar waitQ $ retryMVar stmState:queue     
              val <- readMVar tVarRef
              let newState = stmState{
                       touchedTVars = case lookup id (touchedTVars stmState) of
                                       Just _  -> touchedTVars stmState
                                       Nothing -> (id, lock l):touchedTVars stmState}
              return (Success newState val))
    
readTVarLazy :: TVar a -> STM (LazyValue a)
readTVarLazy (TVar mv id waitQ l) = STM (\stmState -> do
   case IntMap.lookup id (normalWSet stmState) of
     Just (v,_) -> return (Success stmState (pure (unsafeCoerce v)))
     Nothing -> 
       case IntMap.lookup id (lazyWSet stmState) of
          Just (v,_) -> return (Success stmState (unsafeCoerce v))
          Nothing -> do 
              let newState = stmState{
                      touchedTVars = case lookup id (touchedTVars stmState) of
                                       Just _  -> touchedTVars stmState
                                       Nothing -> (id,lock l):touchedTVars stmState}
              return (Success newState (LV (readMVar mv) [waitQ])))

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar mv id waitQ l) v = STM (\stmState -> do
         let newState =
               stmState{touchedTVars = case lookup id (touchedTVars stmState) of
                                         Just _  -> touchedTVars stmState
                                         Nothing ->  (id,lock l):touchedTVars stmState,
                        normalWSet = IntMap.insert id 
                                          (unsafeCoerce v,do
                                            takeMVar mv 
                                            putMVar mv v)
                                          (normalWSet stmState),
                        lazyWSet = IntMap.delete id (lazyWSet stmState),
                        notifys = notifys stmState >> fNotify waitQ}
         return (Success newState ()))

writeTVarLazy :: TVar a -> LazyValue a -> STM ()
writeTVarLazy (TVar mv id waitQ l) lv@(LV act _) = STM (\stmState -> do
       let newState = 
             stmState{touchedTVars = case lookup id (touchedTVars stmState) of
                                    Just _  -> touchedTVars stmState
                                    Nothing ->  (id,lock l):touchedTVars stmState,
                   lazyWSet = IntMap.insert id 
                                     (unsafeCoerce lv,do
                                       val <- act 
                                       return (do {takeMVar mv; putMVar mv val}))
                                     (lazyWSet stmState),
                   normalWSet = IntMap.delete id (normalWSet stmState),
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
	InValid -> do
          rMVar <- newEmptyMVar 
          let newState = state{retryMVar = rMVar}
          atomically' stmAction newState
        Success newState res -> do
          let sorted       = sortBy (\a b -> compare (fst a) (fst b)) $ touchedTVars newState
          unlocker <- sequence $ map snd sorted 
          writes <- sequence $ map (snd . snd) $ IntMap.toList (lazyWSet newState)
          valid <- tryTakeMVar $ retryMVar newState --check validity
          if isNothing valid
            then do
              sequence_ $ map (snd . snd) (IntMap.toList (normalWSet newState))
              sequence_ $ writes
              notifys newState
              sequence_ unlocker
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

-------------------
-- Miscellaneous --
-------------------

evalLazyVal :: LazyValue a -> IO a
evalLazyVal (LV act _) = act

enter :: [MVar [MVar ()]] -> MVar () -> IO()
enter [] _ = return ()
enter (x:xs) mv = do
  val <- takeMVar x
  putMVar x (mv : val)
  enter xs mv

lock :: MVar () -> IO(IO())
lock tv = do takeMVar tv
             return (putMVar tv ())


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
{-
replace :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replace k v [] = [(k,v)]
replace k v ((k1,v2):kvs) | k == k1   = (k,v):kvs
                          | otherwise = (k1,v2):replace k v kvs
-}
