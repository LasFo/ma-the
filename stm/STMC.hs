module STMC
        (STM, TVar, newTVar, readTVar, writeTVar,
         modifyVal, atomically, result, (<%>)) where

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce
import qualified Data.IntMap.Strict as IntMap


-------------------
-----STM types-----
-------------------

type ID = Int

data STM a = STM (StmState -> STMResult a)

class Sequence f where
  (<%>)  :: f a -> f b -> f b
  result :: [f a] -> f [a]
  infixl 1 <%>

instance Sequence STM where
  (STM tr1) <%> (STM tr2) = STM (\stmState -> 
                  let Success interimState _ = tr1 stmState 
                      in tr2 interimState)
  result [] = STM (\stmState -> Success stmState (return []))
  result xs = STM (\stmState -> 
                        let (finState,res) = result' stmState xs
                            result' state  [] = (state,[])
                            result' state ((STM ac):xs) = 
                                let Success newState r = ac state
                                    (interimState,rs)  = result' newState xs
                                  in (interimState, r:rs)
                          in Success finState (sequence res))

data StmState = TST {touchedTVars :: IntMap.IntMap (IO (IO())),
                     writeSet     :: IntMap.IntMap (IO (), IO (IO(),Bool)),
                     notifys      :: IO(),
                     wait         :: IO(),
                     retryMVar    :: MVar ()}

data STMResult a = Success StmState (IO a)

initialState :: IO StmState
initialState = do
   rmv <- newEmptyMVar 
   return (TST {touchedTVars = IntMap.empty,
                writeSet     = IntMap.empty,
                notifys      = return (),
                wait         = return (),
                retryMVar    = rmv})

data TVar a = TVar (MVar a)
                   ID
                   (MVar [MVar ()])
                   (a -> Bool)
                   (MVar ())

-----------------------
-----STM interface-----
-----------------------

newTVar :: a -> (a -> Bool) -> STM (TVar a)
newTVar val con = STM (\stmState -> 
        Success stmState newTV)
  where newTV = do 
           nl <- newMVar ()
           id <- getGlobalId
           mv <- newMVar val
           newWaitQ <- newMVar []
           return $ TVar mv id newWaitQ con nl

readTVar :: TVar a -> STM a 
readTVar (TVar mv id waitQ _ l) = STM (\stmState -> 
        case IntMap.lookup id (writeSet stmState) of
          Just (v,_) -> Success stmState (unsafeCoerce v)
          Nothing -> 
            let newState = 
                  stmState{touchedTVars =  if IntMap.member id (touchedTVars stmState)
                                   then touchedTVars stmState
                                   else IntMap.insert id
                                                      (lock l) 
                                                      (touchedTVars stmState),
                           wait = do 
                                queue <- takeMVar waitQ 
                                putMVar waitQ (retryMVar stmState : queue)
                                wait stmState}
                in Success newState (readMVar mv))

writeTVar :: TVar a -> STM a -> STM ()
writeTVar (TVar mv id waitQ c l) (STM val) = STM (\stmState -> 
   let Success interimState res = val stmState
       newState = interimState{
                    touchedTVars = if IntMap.member id (touchedTVars stmState)
                                   then touchedTVars stmState
                                   else IntMap.insert id
                                                      (lock l) 
                                                      (touchedTVars stmState),
                    writeSet = IntMap.insert id (unsafeCoerce res,do
                                                   v <- res
                                                   return (do {takeMVar mv; putMVar mv v}, 
                                                           c v))
                                             (writeSet interimState),
                    notifys = notifys stmState >> fNotify waitQ}
     in Success newState (return ()))

modifyVal :: STM a -> (a -> a) -> STM a
modifyVal (STM tr) f = STM (\stmState -> 
                        let Success newState res = tr stmState 
                           in Success newState (fmap f res))

atomically :: STM a -> IO a
atomically stmAction = do
   iState <- initialState
   atomically' stmAction iState
   where 
      atomically' :: STM a -> StmState -> IO a
      atomically' act@(STM stmAction) state = do
        case stmAction state of
          Success newState res -> do
            unlock <- sequence $ map snd $ IntMap.toAscList $ touchedTVars newState
            (writes, valid) <- validate $ writeSet newState
            if valid 
                then do
                  notifys newState
                  writes
                  sequence unlock
                  res
                else do
                  wait newState
                  sequence unlock
                  takeMVar $ retryMVar newState
                  mv <- newEmptyMVar
                  let reState = state{retryMVar = mv}
                  atomically' act reState


-----------------------
-----Miscellaceous-----
-----------------------

lock :: MVar () -> IO (IO())
lock mv = do
  takeMVar mv
  return (putMVar mv ())

validate :: IntMap.IntMap (IO (), IO(IO(),Bool)) -> IO (IO(),Bool)
validate ws = validate' (return ()) $ map (snd . snd) $ IntMap.toList ws
  where validate' acc []     = return (acc, True)
        validate' acc (x:xs) = do 
           (ac, valid) <- x
           if valid 
             then validate' (acc >> ac) xs
             else return (return (),False) 

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

