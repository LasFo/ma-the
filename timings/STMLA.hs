module STMLA
           (STM, STMResult, TVar, 
            newTVar, readTVar, writeTVar, atomically,
            retry, orElse, (<**>), (**>), check, 
            (<*>), (*>), pure, T.sequenceA) where

import Prelude
import Control.Concurrent
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce
import Data.List (sortBy,delete,union)
import Data.Maybe (isNothing, fromJust)
import qualified Data.IntMap.Lazy as IntMap
import Control.Applicative
import qualified Data.Traversable as T
import Data.Tuple.Select (sel3)

-----------------------
-- The STM Classes   --
-----------------------

type ID = Int

type ReadSet = IORef [(IORef (), IORef (IORef ()), MVar [MVar ()])]

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
                   --make sure the unsafePerformIO actions are processed as intended
                   --not logging the internal action would lead to blackholes 
                   let finState = interimState{garbage = [unsafeCoerce val1,unsafeCoerce val2] 
                                                         ++ garbage interimState}
                   return (Success finState (val2 <*> val1))
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

infixl 4 **> 
(**>) = flip ($)


instance Monad STM where
  (STM tr1) >>= k = STM (\state -> do
               stmRes <- tr1 state
               case stmRes of
                 Success newState wVal -> do
                      let (STM tr2) = k $ fromJust $! wVal 
                          newState' = newState{garbage = (unsafeCoerce wVal) : garbage newState}
                      tr2 newState'
                 Retry newState -> return (Retry newState)
                 InValid -> return InValid)
  return x      = STM (\state -> return (Success state (return x)))
  (>>) = (*>)
  fail _ = STM (\state -> return InValid)

--needed for functions like swap
--read -> use -> use
--read -> overwrite -> use
{-eval :: STM a -> STM (STM a)
eval (STM stm) = STM (\state -> do
    res <- stm state
    case res of
      Success newState val -> do
        return (Success newState (return 
                        (STM(\st -> return (Success st val))))) 
      Retry newState -> return (Retry newState)
      InValid -> return (InValid))
-}

data StmState = TST {touchedTVars  :: IntMap.IntMap (MVar ()),
                        -- (oldValue, newValue, MVar to write)
                     writeSet      :: IntMap.IntMap (Maybe (),Maybe (),IORef (IORef ())), 
                     notifys       :: IO (),
                     readSet       :: ReadSet,
                     retryMVar     :: MVar (),
                     garbage       :: [Maybe ()]} 

data STMResult a = Retry StmState
                 | InValid
                 | Success StmState (Maybe a)

initialState :: IO StmState
initialState = do
  rMVar <- newEmptyMVar 
  rs <- newIORef []
  return (TST {touchedTVars  = IntMap.empty,
               writeSet      = IntMap.empty,
               notifys       = return (),
               readSet       = rs,
               retryMVar     = rMVar,
               garbage       = []}) 

-- Transactional variables
data TVar a = TVar (IORef (IORef a))   
                   ID               
                   (MVar [MVar ()])  
                   (MVar ())

newTVar   :: a -> STM (TVar a)
newTVar v = STM (\stmState -> do
                    id <- getGlobalId
                    ioRef <- newIORef v
                    newTVarVal <- newIORef ioRef
                    newWaitQ <- newMVar []
                    newLock <- newMVar ()
                    let tVar = TVar newTVarVal id newWaitQ newLock
                    return (Success stmState (return tVar)))

{-# NOINLINE readTVar #-}
readTVar :: TVar a -> STM a
readTVar (TVar mv id waitQ lock) = STM (\stmState -> do
      case IntMap.lookup id (writeSet stmState) of
             Just (v,_write,_ioRef) ->
               return (Success stmState (unsafeCoerce v))
             Nothing -> do
               let res = buildVal mv (readSet stmState) waitQ
                   newState = stmState{touchedTVars = case IntMap.lookup id (touchedTVars stmState) of
                                                        Just _  -> touchedTVars stmState
                                                        Nothing -> IntMap.insert id lock
                                                                           (touchedTVars stmState),
                                       --entering the value in the writeSet prevents the transaction
                                       --to read a TVar multiple times on IO level
                                       writeSet = IntMap.insert id 
                                                      (unsafeCoerce res, Nothing, unsafeCoerce mv)
                                                      (writeSet stmState)}
               return (Success newState res))

--If the evaluation is demanded before the commit phase, it may lead to non inteded behaviour
{-# NOINLINE buildVal #-}
buildVal :: IORef (IORef a) -> ReadSet -> MVar [MVar ()] -> Maybe a
buildVal mv readSet waitQ = unsafePerformIO $ do 
    {-print "IORead"-}
      rs <- readIORef readSet
      ref <- readIORef mv
      writeIORef readSet $ (unsafeCoerce ref, unsafeCoerce mv, waitQ):rs
      readIORef ref >>= return . Just

check :: Bool -> STM ()
check True  = return ()
check False = retry

--The value needs to be a STM action to be able to extract the waitQs it depends on 
writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar ioRef id waitQ lock) val  = STM (\stmState -> do
                 let val' = Just val
                     newState =
                      stmState{touchedTVars = case IntMap.lookup id (touchedTVars stmState) of
                                                Just _a -> touchedTVars stmState
                                                Nothing ->  IntMap.insert id lock
                                                                   (touchedTVars stmState),
                               writeSet = IntMap.insert id 
                                              (unsafeCoerce val', unsafeCoerce val', unsafeCoerce ioRef)
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
          unlocker <- mapM io $ IntMap.elems (touchedTVars newState)
          valid <- validate $ readSet newState -- tryTakeMVar $ retryMVar newState
          when valid (do rs <- readIORef $ readSet newState
                         let observes = map sel3 rs
                         enter observes (retryMVar newState) 
                         sequence_ unlocker
                         takeMVar (retryMVar newState)
                         unsub (retryMVar newState) observes)
          when (not valid) (sequence_ unlocker)
          newRs <- newIORef []
          rMVar <- newEmptyMVar
          let reState = state{retryMVar = rMVar,
                              readSet   = newRs}
          atomically' stmAction reState
        InValid -> do
          newRs <- newIORef []
          rMVar <- newEmptyMVar 
          let reState = state{retryMVar = rMVar,
                              readSet = newRs}
          atomically' stmAction reState
        Success newState res -> do 
          unlocker <- mapM io $ IntMap.elems (touchedTVars newState)
          valid <- validate $ readSet newState -- tryTakeMVar $ retryMVar newState
          --print valid 
          if valid
            then do
          --    print $ length (garbage newState)
              mapM_ (flip seq (return ())) (garbage newState)
              result <- seq res (return (fromJust res))
              writer <- write $ IntMap.elems (writeSet newState)
              notifys newState
              writer
              sequence_ unlocker
              return result
            else do
              sequence_ unlocker
              newRs <- newIORef []
              rMVar <- newEmptyMVar 
              let newState = state{retryMVar = rMVar,
                                   readSet = newRs}
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

unsub :: MVar () -> [MVar [MVar ()]] -> IO ()
unsub _ret []    = return ()
unsub ret (x:xs) = do
  waitQ <- takeMVar x
  putMVar x (delete ret waitQ)
  unsub ret xs

--using io actions to collect locks for tvars of different types in one collection
--used for implicit locks
io :: MVar a -> IO(IO())
io tv = do tid <- myThreadId
           a <- takeMVar tv
           return (putMVar tv a)
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
write :: [(Maybe (), Maybe (),IORef (IORef ()))] -> IO (IO ())
write ws = do
    writes <- readWS ws
   -- locks
    return writes
  where replace mv val = do ioRef <- newIORef val
                            writeIORef mv ioRef
        readWS []               = return (return ())
        readWS ((_old,Nothing,_ref):xs) = readWS xs
        readWS ((_old,Just a,ref):xs)   = do
          --  putStrLn "Eval ws"
             ws <- readWS xs
             return (replace ref a >> ws)
             
startSTM :: STM a -> StmState -> IO (STMResult a)
startSTM stmAct@(STM stm) state = stm state


enter :: [MVar [MVar()]] -> MVar () -> IO ()
enter [] _       = return ()
enter (x:xs) ret = do
   wq <- takeMVar x
   putMVar x (ret:wq)
   enter xs ret


validate :: ReadSet -> IO Bool
validate rs = do readSet <- readIORef rs
                 valids <- mapM compare readSet
                 return $ and valids
   where compare (ref, mv, _waitQ) = do cRef <- readIORef mv 
                                        return $ cRef == ref

{-# NOINLINE globalCount #-}
globalCount :: MVar Int --[ID]
globalCount  = unsafePerformIO (newMVar 0) --(newMVar [0..])

getGlobalId :: IO Int
getGlobalId = do
  num <- takeMVar globalCount 
  putMVar globalCount (num+1) --(tail nums)
  return num

fNotify :: MVar [MVar ()] -> IO ()
fNotify waitQ = do
  queue <- takeMVar waitQ
  mapM_ (flip tryPutMVar ()) queue 
  putMVar waitQ []

