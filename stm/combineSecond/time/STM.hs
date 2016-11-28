module STM
           (STM(STM), STMResult(Success), TVar(TVar), 
            newTVar, readTVar, writeTVar, 
            atomically, retry, orElse, (<**>),
            (**>), (<*>), (*>), pure, T.sequenceA) where

import Prelude
import Control.Concurrent
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce
import Data.List (sortBy,delete,union)
import Data.Maybe (isNothing)
import qualified Data.IntMap.Lazy as IntMap
import Control.Applicative
import qualified Data.Traversable as T

-----------------------
-- The STM interface --
-----------------------

type ID = Int

-- The STM monad itself
data STM a = STM (StmState -> IO (STMResult a))

instance Functor STM where
  fmap f (STM tr) = STM (\state -> do 
      res <- tr state
      case res of 
        Success newState wqs val -> return (Success newState wqs (fmap f val))
        Retry newState -> return $ Retry newState
        InValid -> return InValid)

instance Applicative STM where 
   pure a = STM (\state -> return $ Success state [] (return a))
   (STM t2) <*> (STM t1) = STM (\state -> do 
       res1 <- t1 state
       case res1 of 
         Success newState wqs1 act1 -> do
            res2 <- t2 newState 
            case res2 of
              Success finState wqs2 act2 -> 
                   return (Success finState (union wqs1 wqs2) (act2 <*> act1))
              Retry finState -> return $ Retry finState
              InValid -> return InValid
         Retry newState -> return $ Retry newState
         InValid -> return InValid)

infixl 4 **>

(**>) :: STM a -> (STM a -> STM b) -> STM b
(**>) = flip ($)

instance Monad STM where
  (STM tr1)  >>= k = STM (\state -> do
                          valid <- tryTakeMVar $ retryMVar state
                          if isNothing valid
                            then do stmRes <- tr1 state
                                    case stmRes of
                                      Success newState wqs a -> do 
                                        enter wqs (retryMVar newState)
                                        val <- a
                                        let (STM tr2) = k val 
                                        tr2 newState
                                      Retry newState -> return (Retry newState)
                                      InValid -> return InValid
                            else return InValid)
  return x      = STM (\state -> return (Success state [] (return x)))

data StmState = TST {touchedTVars  :: IntMap.IntMap (IO(IO())),
                --writeSet contains: (value, MVar to be written, waitQ of TVar the value depends on)
                     writeSet      :: IntMap.IntMap (IO (),IORef (),[MVar [MVar ()]]), 
                     notifys       :: IO (),
                     retryMVar     :: MVar ()} 

data STMResult a = Retry StmState
                 | InValid
                 | Success StmState [MVar [MVar ()]] (IO a)

initialState :: IO StmState
initialState = do
  rMVar <- newEmptyMVar 
  return (TST {touchedTVars  = IntMap.empty,
               writeSet      = IntMap.empty,
               notifys       = return (),
               retryMVar     = rMVar}) 

-- Transactional variables
data TVar a = TVar (IORef a)   
                   ID               
                   (MVar [MVar ()])  
                   (MVar ())

newTVar   :: a -> STM (TVar a)
newTVar v = STM (\stmState -> do
                    id <- getGlobalId
                    newTVarVal <- newIORef v 
                    newWaitQ <- newMVar []
                    newLock <- newMVar ()
                    let tVar = TVar newTVarVal id newWaitQ newLock
                    return (Success stmState [] (return tVar)))

readTVar :: TVar a -> STM a
readTVar (TVar mv id waitQ lock) = STM (\stmState -> do
      case IntMap.lookup id (writeSet stmState) of
             Just (v,_,wqs) -> do
               return (Success stmState wqs (unsafeCoerce v))
             Nothing -> do
               let newState = stmState{touchedTVars = case IntMap.lookup id (touchedTVars stmState) of
                                                        Just _  -> touchedTVars stmState
                                                        Nothing -> IntMap.insert id (io lock)
                                                                           (touchedTVars stmState)}
               return (Success newState [waitQ] (readIORef mv)))

--using io actions to collect locks for tvars of different types in one collection
io :: MVar a -> IO(IO())
io tv = do tid <- myThreadId
           a <- takeMVar tv
           return (putMVar tv a)

--The value needs to be a STM action to be able to extract the waitQs it depends on 
writeTVar :: TVar a -> STM a -> STM ()
writeTVar (TVar mv id waitQ lock) (STM tr) = STM (\stmState -> do
         res <- tr stmState 
         case res of
           Success newState wqs act -> 
              let newState =
                    stmState{touchedTVars = case IntMap.lookup id (touchedTVars stmState) of
                                              Just _  -> touchedTVars stmState
                                              Nothing ->  IntMap.insert id (io lock) 
                                                                 (touchedTVars stmState),
                             writeSet = IntMap.insert id 
                                            (unsafeCoerce act,unsafeCoerce mv,wqs)
                                            (writeSet stmState),
                             notifys = --if IntMap.member id (writeSet stmState)
                                         --then notifys stmState else --multiple notifys are cheaper
                                         --than lookup in the map
                                         notifys stmState >> fNotify waitQ} in
                 return (Success newState [] (return ())) 
           Retry newState -> return $ Retry newState
           InValid -> return InValid)

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
          takeMVar (retryMVar state) 
          rMVar <- newEmptyMVar
          let reState = state{retryMVar = rMVar}
          atomically' stmAction reState
        InValid -> do
          rMVar <- newEmptyMVar 
          let reState = state{retryMVar = rMVar}
          atomically' stmAction reState
        Success newState _ res -> do --at this point waitQs are no longer important
          unlocker <- sequence $ IntMap.elems (touchedTVars newState)
          valid <- tryTakeMVar $ retryMVar newState 
          if isNothing valid
            then do
              mapM_ write $ IntMap.elems (writeSet newState)
              notifys newState
              a <- res --evaluation of the result while the tvars are locked
              sequence_ unlocker
              return a
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
             Retry newState -> stm2 stmState{touchedTVars = touchedTVars newState}
             _              -> return stm1Res)

-------------------
-- Miscellaneous --
-------------------


{-
filt :: [ID] -> [IO()] -> [ID] -> [IO()]
filt []       [] [] = []
filt _        xs [] = xs
filt iden@(id:ids) act@(io:ios) w@(wid:wids)
  | id == wid = filt ids ios wids
  | id <  wid = io : (filt ids ios w)
  | id >  wid = filt iden act wids  
 -}

write :: (IO (),IORef(), [MVar [MVar ()]]) -> IO ()
write (act,mv,_) = do 
   v <- act
   writeIORef mv v

startSTM :: STM a -> StmState -> IO (STMResult a)
startSTM stmAct@(STM stm) state = stm state

enter :: [MVar [MVar ()]] -> MVar () -> IO ()
enter [] _       = return ()
enter (x:xs) ret = do
   wq <- takeMVar x
   putMVar x (ret:wq)
   enter xs ret

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

