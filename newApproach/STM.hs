module STM
           (STM(STM), STMResult(Success), TVar(TVar), 
            newTVar, readTVar, writeTVar, eval, 
            atomically, retry, orElse, (<**>), (<<*),
            (*>>), (<*>), (*>), pure, T.sequenceA) where

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

type Val a = ([MVar [MVar ()]],a)

-- The STM monad itself
data STM a = STM (StmState -> IO (STMResult a))

instance Functor STM where
  fmap f (STM tr) = STM (\state -> do 
      res <- tr state
      case res of 
        Success newState val -> return (Success newState (fmap f val))
        Retry newState -> return $ Retry newState
        InValid -> return InValid)

instance Applicative STM where 
   pure a = STM (\state -> return $ Success state (return a))
   (STM t2) <*> (STM t1) = STM (\state -> do 
       res1 <- t1 state
       case res1 of 
         Success newState val1 -> do
            res2 <- t2 newState 
            case res2 of
              Success interimState val2 -> do
                   --make sure the unsafeperform ioactions are processed as intended
                   --without logging the inter action would lead to blackholes 
                   let finState = interimState{garbage = [unsafeCoerce val1,unsafeCoerce val2] 
                                                         ++ garbage interimState}
                   return (Success finState ((union (fst val1) (fst val2)),((snd val2) (snd val1))))
              Retry finState -> return $ Retry finState
              InValid -> return InValid
         Retry newState -> return $ Retry newState
         InValid -> return InValid)
   (STM t1) *> (STM t2) = STM (\state -> do
      res1 <- t1 state 
      case res1 of 
        Success newState _val ->
          t2 newState
        Retry newState -> return $ Retry newState
        InValid -> return InValid)

infixl 4 *>>
(*>>) :: STM a -> (Val a -> STM b) -> STM b
(STM a1) *>> f = STM (\state -> do
    res <- a1 state
    case res of
       Success newState val -> do
          let STM a2 = f val
          a2 newState
       Retry newState -> return $ Retry newState
       InValid -> return InValid)

infixl 4 <<*
(<<*) :: STM a -> (Val a -> STM b) -> STM a
(STM a1) <<* f = STM (\state -> do
    res <- a1 state
    case res of
       Success newState val -> do
          let STM a2 = f val
          res2 <- a2 newState
          case res2 of
             Success finState _val -> return (Success finState val)
             Retry finState -> return $ Retry finState
             InValid        -> return InValid
       Retry newState -> return $ Retry newState
       InValid -> return InValid)


instance Monad STM where
  (STM tr1)  >>= k = STM (\state -> do
                          valid <- tryTakeMVar $ retryMVar state
                          if isNothing valid
                            then do stmRes <- tr1 state
                                    case stmRes of
                                      Success newState (deps,val) -> do 
                                        enter deps (retryMVar newState)
                                        let (STM tr2) = k val 
                                        tr2 newState
                                      Retry newState -> return (Retry newState)
                                      InValid -> return InValid
                            else return InValid)
  return x      = STM (\state -> return (Success state (return x)))
  (STM t1) >> (STM t2) = STM (\state -> do
     res1 <- t1 state 
     case res1 of 
       Success newState _val ->
         t2 newState
       Retry newState -> return $ Retry newState
       InValid -> return InValid)
  fail _ = STM (\state -> return InValid)

--needed for functions like swap
--read -> use -> use
--read -> overwrite -> use
eval :: STM a -> STM (STM a)
eval (STM stm) = STM (\state -> do
    res <- stm state
    case res of
      Success newState val -> do
        return (Success newState (return 
                        (STM(\st -> return (Success st val))))) 
      Retry newState -> return (Retry newState)
      InValid -> return (InValid))


data StmState = TST {touchedTVars  :: IntMap.IntMap (IO(IO())),
                     writeSet      :: IntMap.IntMap (Val (),IORef ()), 
                     notifys       :: IO (),
                     retryMVar     :: MVar (),
                     garbage       :: [Val ()]} 

data STMResult a = Retry StmState
                 | InValid
                 | Success StmState (Val a)

initialState :: IO StmState
initialState = do
  rMVar <- newEmptyMVar 
  return (TST {touchedTVars  = IntMap.empty,
               writeSet      = IntMap.empty,
               notifys       = return (),
               retryMVar     = rMVar,
               garbage       = []}) 

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
                    return (Success stmState (return tVar)))

readTVar :: TVar a -> STM a
readTVar (TVar ioRef id waitQ lock) = STM (\stmState -> do
      case IntMap.lookup id (writeSet stmState) of
             Just (v,_ioRef) -> do
               return (Success stmState (unsafeCoerce v))
             Nothing -> do
               let newState = stmState{touchedTVars = case IntMap.lookup id (touchedTVars stmState) of
                                                        Just _  -> touchedTVars stmState
                                                        Nothing -> IntMap.insert id (io lock)
                                                                           (touchedTVars stmState)}
               return (Success newState (buildVal waitQ ioRef)))

--If the evaluation is demanded before the commit phase, it may lead to non inteded behaviour
buildVal :: MVar [MVar ()] -> IORef a -> Val a
buildVal deps ioRef = unsafePerformIO $ print "Hallo" >> readIORef ioRef >>= return . (,) [deps] 

--using io actions to collect locks for tvars of different types in one collection
--used for implicit locks
io :: MVar a -> IO(IO())
io tv = do tid <- myThreadId
           a <- takeMVar tv
           return (putMVar tv a)

--The value needs to be a STM action to be able to extract the waitQs it depends on 
writeTVar :: TVar a -> Val a -> STM ()
writeTVar (TVar ioRef id waitQ lock) val = STM (\stmState -> do
               let newState =
                    stmState{touchedTVars = case IntMap.lookup id (touchedTVars stmState) of
                                              Just _a -> touchedTVars stmState
                                              Nothing ->  IntMap.insert id (io lock) 
                                                                 (touchedTVars stmState),
                             writeSet = IntMap.insert id 
                                            (unsafeCoerce val,unsafeCoerce ioRef)
                                            (writeSet stmState),
                             notifys = notifys stmState >> fNotify waitQ} 
               return (Success newState (return ()))) 
{-
rwIO :: IO a -> IORef a -> IO (IO ())
rwIO act ref = do
  a <- act
  return (writeIORef ref a)
-}
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
        Success newState (_,res) -> do --at this point waitQs are no longer important
          unlocker <- sequence $ IntMap.elems (touchedTVars newState)
          valid <- tryTakeMVar $ retryMVar newState 
          if isNothing valid
            then do
              mapM_ (flip seq (return ())) (garbage newState)
              write $ IntMap.elems (writeSet newState)
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

--needs to be seperated in 2 phases.
--otherwise the some values may be overwritten before they are read
--for details look at swapText which fail when read and write is done
--in one traverse.
write :: [(Val (),IORef())] -> IO ()
write ws = do
    writes <- readWS ws
    writes
  where readWS []               = return (return ())
        readWS ((val,ref):xs)   = do 
             seq val (return ())            
             let (_deps, a) = val 
             ws <- readWS xs
             return (writeIORef ref a >> ws)
             
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

