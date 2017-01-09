module GraphSTM where 

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Data.List ((\\))
import Unsafe.Coerce
import Control.Concurrent.MVar 
import System.IO.Unsafe
import Graph
import Data.Maybe (isNothing, fromJust)
import Control.Applicative

newtype NodeVal = NV (MVar (), (Maybe (IO())), ID) 

instance Eq NodeVal where
  (NV (a,_,_)) == (NV (b,_,_)) = a == b

newtype STMVal a = SV (IO a,[MVar [MVar ()]],[NodeVal])

instance Functor STMVal where 
   fmap f (SV (act,wq,nodes)) = SV (fmap f act,wq,nodes)

instance Applicative STMVal where
   pure x = SV (return x, [],[])
   (SV (act1,wq1,nodes1)) <*> (SV (act2,wq2,nodes2)) = SV (act1 <*> act2,wq1++wq2,nodes1++nodes2)

type ID = Int

instance Monad STM where
  return a = STM (\state -> return (Success state a))
  (STM t1) >>= k = STM (\state -> do
                   stmRes <- t1 state
                   case stmRes of
                     Success newState a ->
                       let (STM tr2) = k a in
                         tr2 newState
                     Retry newState -> return (Retry newState)
                     InValid -> return InValid)

data STMState = TST {touchedTVars :: IntMap.IntMap (IO (IO())),
                     tvarGraph    :: G2 NodeVal,
                     writeSet     :: IntMap.IntMap (IO (),MVar (),NodeVal),
                     notifys      :: IO(),
                     retryMVar    :: MVar ()} 

initialState :: IO STMState
initialState = do 
  rMVar <- newEmptyMVar 
  return (TST {touchedTVars = IntMap.empty,
               tvarGraph    = emptyGraph,
               writeSet     = IntMap.empty,
               notifys      = return (),
               retryMVar    = rMVar})

data STM a = STM (STMState -> IO (STMResult a))

data STMResult a = Success STMState a
                 | Retry STMState
                 | InValid

data TVar a = TVar (MVar a)
                   ID
                   (MVar [MVar ()])
                   (MVar ()) -- we want to be able to read TVars while they are locked

cast :: STMVal a -> IO a
cast (SV (act,_,_)) = act

--ungueltig wenn ein Wert, der aus dem WS gelesen wurde,
--hiermit umgewandelt wird. 
getSTMVal :: STMVal a -> STM a
getSTMVal (SV (act,wqs,_)) = STM (\state -> do
     a <- act 
     enter wqs (retryMVar state)
     return (Success state a))

enter :: [MVar [MVar ()]] -> MVar () -> IO ()
enter [] _ = return ()
enter (x:xs) mv = do
  val <- takeMVar x
  putMVar x (mv :val)
  enter xs mv

io :: MVar a -> IO(IO ())
io mv = do
  a <- takeMVar mv
  return (putMVar mv a) 

readTVar :: TVar a -> STM (STMVal a)
readTVar (TVar mv id waitQ lo) = STM (\state ->
                case IntMap.lookup id (writeSet state) of
                   Just (v,_,node) -> return (Success state (SV (unsafeCoerce v,[],[node])))
                   Nothing -> do
                     newMV <- newEmptyMVar
                     wq <- takeMVar waitQ
                     putMVar waitQ (newMV : wq)
                     let newNode = NV (newMV,Nothing,id)
                         newState = state{touchedTVars = IntMap.insert id (io lo) (touchedTVars state),
                                          tvarGraph = addNode newNode  
                                                              [] [] 
                                                              (tvarGraph state)}
                     return (Success newState (SV (readMVar mv,[waitQ],[newNode]))))


writeTVar :: TVar a -> STMVal a -> STM ()
writeTVar (TVar mv id waitQ lo) (SV (act,wqs,nodes)) = STM (\state -> do
        act
        newMV <- newEmptyMVar 
        let newNode = NV (newMV,Just (unsafeCoerce act),id)
            newState = state{touchedTVars = IntMap.insert id (io lo) (touchedTVars state),
                             writeSet = IntMap.insert id (unsafeCoerce act,unsafeCoerce mv,newNode)
                                                         (writeSet state),
                             notifys = fNotify waitQ >> notifys state,
                             tvarGraph = addNode newNode nodes [] (tvarGraph state)}
        return (Success newState ()))

nodeToID :: NodeVal -> ID
nodeToID (NV (_,_,id)) = id

newTVar :: a -> STM (TVar a)
newTVar val = STM (\state -> do
    lock <- newMVar ()
    nmv <- newMVar val
    wq <- newMVar []
    id <- getGlobalId
    return (Success state (TVar nmv id wq lock)))

atomically :: STM a -> IO a
atomically t1 = do
  iState <- initialState
  atomically' t1 iState
  where atomically' :: STM a -> STMState -> IO a
        atomically' t state = do
          stmRes <- start t state
          case stmRes of
            Retry newState -> do
              takeMVar (retryMVar state)
              nrmv <- newEmptyMVar
              let reState = state{retryMVar=nrmv}
              atomically' t reState
            InValid -> do
              nrmv <- newEmptyMVar
              let reState = state{retryMVar=nrmv}
              atomically' t reState
            Success newState res -> do
                 valid <- commit newState
                 if valid then return res
                 else do
                   nrmv <- newEmptyMVar
                   let reState = state{retryMVar=nrmv}
                   atomically' t reState



commit ::  STMState -> IO Bool
commit state = do
   writes <- eval $ writeSet state
   let locks = IntMap.toAscList (touchedTVars state) 
       (keys,vals) = unzip (IntMap.toAscList (writeSet state))
   unlock <- mapM snd locks
   valid1 <- tryTakeMVar $ retryMVar state
   if isNothing valid1 
      then do 
        valid2 <- validate $ tvarGraph state
        if isNothing valid2
             then do 
               writes 
               notifys state
               sequence_ unlock
               return True 
             else do
               sequence_ unlock
               let inValidGraph = fromJust valid2
                   revWrites = reevaluate inValidGraph
                   newState = state{writeSet = redesign revWrites (writeSet state)}
               commit newState
   else return False     

eval :: IntMap.IntMap (IO (),MVar (),NodeVal) -> IO(IO())
eval ws = eval' $ map (\(x,y,_) -> (x,y)) $ map snd (IntMap.toList ws)
  where eval' []     = return (return ())
        eval' ((act,mv):xs) = do
           val <- act
           res <- eval' xs
           return (do {takeMVar mv; putMVar mv val} >> res)



start :: STM a -> STMState ->  IO (STMResult a)
start (STM act) state = act state

redesign :: [(ID,IO ())] -> IntMap.IntMap (IO (),MVar (), NodeVal) ->  
                            IntMap.IntMap (IO (),MVar (), NodeVal)
redesign [] map = map
redesign ((id,val):xs) map = redesign xs $ IntMap.adjust (\(_,b,c) -> (val,b,c)) id map


lock :: [(ID,IO(IO()))] -> [ID] -> IO ([IO()],[IO()])
lock [] _  = return ([],[])
lock xs [] = mapM snd xs >>= (\x -> return (x,[]))        
lock ((id,act):xs) (y:ys) 
  | id == y = do (ro, ws) <- lock xs ys
                 res <- act
                 return (ro,res:ws)
  | id /= y = do (ro,ws) <- lock xs ys
                 res <- act
                 return (res:ro,ws)

reevaluate :: [NodeVal] -> [(ID,IO ())]
reevaluate [] = []
reevaluate ((NV (_,act,id)):xs) = 
  if isNothing act 
       then reevaluate xs
       else ((id, fromJust act):reevaluate xs)

validate :: G2 NodeVal -> IO (Maybe [NodeVal])
validate g = do 
    invalidNodes <- checkValid $ nodes g
    if null invalidNodes 
         then return Nothing
         else return $ Just $ bfs invalidNodes g


checkValid :: [NodeVal] -> IO [NodeVal]
checkValid []     = return []
checkValid (n@(NV(x,_,_)):xs) = do
  res <- checkValid xs
  valid <- tryTakeMVar x
  if isNothing valid 
     then return res
     else return (n:res)
 



{-# NOINLINE globalCount #-}
globalCount :: MVar Int
globalCount = unsafePerformIO $ newMVar 0

getGlobalId :: IO Int
getGlobalId = do
  num <- takeMVar globalCount
  putMVar globalCount $ num + 1
  return num

fNotify :: MVar [MVar ()] -> IO ()
fNotify waitQ = do
  queue <- takeMVar waitQ
  mapM_ (flip tryPutMVar ()) queue
  putMVar waitQ []
