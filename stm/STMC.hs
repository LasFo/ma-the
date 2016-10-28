module STMC
        (STM, TVar, newTVar, readTVar, writeTVar,
         atomically, result, (<%>), pure, (<*>),
         replaceConstraint) where

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce
import qualified Data.IntMap.Strict as IntMap
import Control.Applicative
import Data.Maybe (fromJust) 

-------------------
-----STM types-----
-------------------

type ID = Int

data STM a = STM (StmState -> STMResult a)

instance Functor STM where
  fmap f (STM tr) = STM (\state -> 
        let Success newState res = tr state
          in Success newState (fmap f res))

instance Applicative STM where
  pure a = STM (\stmState -> Success stmState (return a))
  (STM f) <*> (STM tr) = STM (\stmState -> 
                        let Success interimState arg = tr stmState
                            Success newState fun     = f interimState
                           in Success newState (fun <*> arg))

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
                     conMods      :: IntMap.IntMap ((),IO()),
                     notifys      :: IO(),
                     wait         :: IO(),
                     retryMVar    :: MVar ()}

data STMResult a = Success StmState (IO a)

initialState :: IO StmState
initialState = do
   rmv <- newEmptyMVar 
   return (TST {touchedTVars = IntMap.empty,
                writeSet     = IntMap.empty,
                conMods     = IntMap.empty,
                notifys      = return (),
                wait         = return (),
                retryMVar    = rmv})

data TVar a = TVar (MVar a)
                   ID
                   (MVar [MVar ()])
                   (MVar (a -> Bool))
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
           nc <- newMVar con
           newWaitQ <- newMVar []
           return $ TVar mv id newWaitQ nc nl

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
writeTVar (TVar mv id waitQ mc l) (STM val) = STM (\stmState -> 
   let Success interimState res = val stmState
       newState = interimState{
                    touchedTVars = if IntMap.member id (touchedTVars stmState)
                                   then touchedTVars stmState
                                   else IntMap.insert id
                                                      (lock l) 
                                                      (touchedTVars stmState),
                    writeSet = IntMap.insert id (unsafeCoerce res,do
                                                   v <- res
                                                   c <- readMVar mc
                                                   return (do {takeMVar mv; putMVar mv v}, 
                                                           c v))
                                             (writeSet interimState),
                    notifys = notifys stmState >> fNotify waitQ}
     in Success newState (return ()))

replaceConstraint :: TVar a -> (a -> Bool) -> STM ()
replaceConstraint (TVar mv id waitQ mc l) c = STM (\stmState ->  
  let newState = stmState{
                    touchedTVars = if IntMap.member id (touchedTVars stmState)
                                   then touchedTVars stmState
                                   else IntMap.insert id
                                                      (lock l) 
                                                      (touchedTVars stmState),
                    writeSet = IntMap.insertWith (flip const) id (unsafeCoerce (readMVar mv),do
                                                   v <- readMVar mv
                                                   c <- readMVar mc
                                                   return (do {takeMVar mv; putMVar mv v}, 
                                                           c v))
                                             (writeSet stmState),
                    conMods = IntMap.insert id (unsafeCoerce c, do {takeMVar mc; putMVar mc c})
                                             (conMods stmState),
                    notifys = notifys stmState >> fNotify waitQ,
                    wait = do 
                         queue <- takeMVar waitQ 
                         putMVar waitQ (retryMVar stmState : queue)
                         wait stmState}
     in Success newState (return ()))

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
            (newCons,valid1) <- conCheck (conMods newState) (writeSet newState)
            (writes, valid2) <- validate $ IntMap.difference (writeSet newState) (conMods newState)
            if valid1 && valid2 
                then do
                  newCons
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

conCheck :: IntMap.IntMap ((),IO()) -> IntMap.IntMap (IO (), IO(IO(),Bool)) -> IO (IO(),Bool)
conCheck nc ws = conCheck' (return ()) (IntMap.toList nc) ws
  where conCheck' acc [] _ = return (acc, True)
        conCheck' acc ((id,(c,ac)):xs) ws = do
           (val,ioac) <- unsafeCoerce $ fromJust $ IntMap.lookup id ws
           (wrac,_) <- ioac
           if (unsafeCoerce c) val 
             then conCheck' (acc >> wrac >> ac) xs ws
             else return (return (), False)


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

