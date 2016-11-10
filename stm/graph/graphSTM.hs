
import qualified Data.IntMap.Stict as IntMap
import qualified Data.Set as Set
import Unsafe.Coerce
import Control.Concurrent.MVar 

data Graph = Graph (IntMap.IntMap Node) (IntMap.IntMap (Set.Set ID))

type Node = IO()

type ID = Int

data STMState = TST {touchedTVars :: IntMap.IntMap (IO (IO())),
                     tvarGraph    :: Graph,
                     notifys      :: IO(),
                     retryMVar    :: MVar ()} 

data STM a = STM (STMState -> IO (STMResult a))

data STMResult a = Success STMState a
                 | Retry STMState
                 | InValid

data TVar a = TVar (Mvar a)
                   ID
                   (MVar [MVar ()])

readTVar :: TVar a -> STM a 
readTVar (TVar mv id waitQ) = STM (\state ->
                case find id (tvarGraph state) of
                   Just v -> return (Success state (unsafeCoerce v))
                   Nothing -> do
                     res <- readMVar mv
                     let newState = state{tvarGraph = insert id (return()) 
                                                             Set.empty 
                                                             (tvarGraph state)}
                     return (Success newState (res,[])))

writeTVar :: TVar a -> STM a -> STM a
writeTVar (TVar mv id waitQ) (STM act) = STM (\stmState -> do
        res <- act stmState
        case res of 
           Retry newState -> return $ Retry newState
           InValid -> return InValid 
           Success newState (val,deps) -> do
             let fstate = newState{tvarGraph = insert id val deps (tvarGraph newState)}
