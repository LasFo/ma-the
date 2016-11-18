module STMBase
           (STM, TVar, newTVar, readTVar, writeTVar, 
            atomically, retry, orElse, proc) where

import Prelude
import Control.Concurrent
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

-----------------------
-- The STM interface --
-----------------------

type ID = Integer

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
-- We use Lists for collecting writtenValues.
-- A Data.Map should be more efficient.
data StmState = TST {touchedTVars  :: [ID],   -- not used, but collected for alternative locking
		     isValid       :: IO Bool,
                     writtenValues :: [(ID,((),IO()))], -- (id,(unsafeCoerced Value, Commit action))
		     notifys       :: IO (),
		     wait          :: IO (),
                     retryMVar     :: MVar ()}

data STMResult a = Retry StmState
	         | InValid
		 | Success StmState a

initialState :: IO StmState
initialState = do
  --atomicallyId <- getGlobalId
  rMVar <- newEmptyMVar --werden die wieder geloescht??
  return (TST {touchedTVars  = [],
	       isValid       = return True,
	       writtenValues = [],
	       notifys       = return (),
	       wait          = return (),
               retryMVar     = rMVar})

-- Transactional variables
data TVar a = TVar (MVar (IORef a))  -- global TVar itself. Warum IORef?? Zum Vergleichen??
                   ID                -- TVar identifier
                   (MVar [MVar ()])  -- wait queue on retry

--werden diese wieder geloescht??
newTVar   :: a -> STM (TVar a)
newTVar v = STM (\stmState -> do
                    id <- getGlobalId
                    newTVarVal <- newIORef v
                    newTVarRef <- newMVar newTVarVal
                    newWaitQ <- newMVar []
                    let tVar = TVar newTVarRef id newWaitQ
		    return (Success stmState tVar))

--warum Show a?? oder debug Ueberreste?
readTVar  :: Show a => TVar a -> STM a
readTVar (TVar tVarRef id waitQ) = STM (\stmState ->
    case lookup id (writtenValues stmState) of
      Just (v,_) -> do
                 --putStr "WR "
                 return (Success stmState (unsafeCoerce v))
      Nothing -> do
         tVarVal <- readMVar tVarRef
         let newState = stmState{touchedTVars = id:touchedTVars stmState,
                                 isValid = do --IORef fuer diesen Vergleich? 
                                      tVarVal' <- readMVar tVarRef
	  			      (return (tVarVal==tVarVal') 
				       >>+ isValid stmState),
                                 wait = do 
				      queue <- takeMVar waitQ
                                      putMVar waitQ 
					      (retryMVar stmState:queue) --mehrfach Vorkommen?
                                      wait stmState}
         val <- readIORef tVarVal
         return (Success newState val))


writeTVar :: Show a => TVar a -> a -> STM ()
writeTVar (TVar tVarRef id waitQ) v = STM (\stmState -> do
   let newState = 
         stmState{touchedTVars = if elem id (touchedTVars stmState)
                                   then touchedTVars stmState
                                   else id:touchedTVars stmState,
                  writtenValues = replace id (unsafeCoerce v,do 
                                                 ref <- newIORef v
                                                 takeMVar tVarRef
                                                 putMVar tVarRef ref)
                                          (writtenValues stmState),
                        --mehrfach Benachrichtigung?
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
          --putStr "W "
 	  wait newState
          takeMVar (retryMVar state)   -- suspend
          --putStr "A "
          atomically' stmAction state
	InValid -> do
          --putStrLn "! "
          atomically' stmAction state
        Success newState res -> do
          --putStr "s "
          takeMVar globalLock
          valid <- isValid newState
          if valid
            then do
              --putStrLn ("+ "++show (map fst (writtenValues newState)))
              sequence_ $ map (snd . snd) (writtenValues newState)
              notifys newState
              putMVar globalLock ()
              return res
            else do
              --putStr "*"
              putMVar globalLock ()
    	      atomically' stmAction state

retry  :: STM a
retry =
  STM (\stmState -> do
         takeMVar globalLock        
         valid <- isValid stmState  -- This validity check could also be done in atomically
         putMVar globalLock ()      -- 
         if valid
           then return (Retry stmState)
           else return InValid)


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

startSTM :: STM a -> StmState -> IO (STMResult a)
startSTM stmAct@(STM stm) state = stm state

proc :: IO a -> STM a
proc io = STM (\stmState -> do
                  res <- io
                  return (Success stmState res))

globalLock :: MVar ()
globalLock = unsafePerformIO (newMVar ())

{-# NOINLINE globalCount #-}
globalCount :: MVar Integer --[ID]
globalCount  = unsafePerformIO (newMVar 0) --(newMVar [0..])

getGlobalId :: IO Integer
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

