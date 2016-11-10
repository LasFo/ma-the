module STMA where 


import Unsafe.Coerce
import Control.Concurrent.MVar
import Control.Arrow
import Control.Category
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap

type ID = Int

data STM a b = STM (a -> (STMState -> IO (STMResult b)))

instance Category STM where
  id = STM (\a -> (\state -> return (Success state a)))
  (STM tr1) . (STM tr2) = STM (\a -> (\state -> do
                             let fun1 = tr2 a
                             res1 <- fun1 state 
                             case res1 of
                               Retry newState       -> return (Retry newState)
                               InValid              -> return InValid
                               Success newState val -> (tr1 val) newState))
                                  


instance Arrow STM where 
  arr fun = STM (\a -> (\stmState -> return (Success stmState (fun a))))
  first (STM tr) = STM (\(a,b) -> (\stmState -> do
                                        res <- (tr a) stmState
                                        case res of 
                                           Retry newState       -> return (Retry newState)
                                           InValid              -> return InValid
                                           Success newState val -> return (Success newState (val,b))))



data STMState = TST {touchedTVars :: Set.Set ID,
                     writeSet     :: IntMap.IntMap ((),IO()),
                     notifys      :: IO(),
                     retryMVar    :: MVar ()}

data STMResult a = Retry STMState
                 | InValid
                 | Success STMState a

date TVar a = TVar (MVar a)
                   ID 
                   (MVar [MVar ()])


writeTVar :: STM (TVar a, a) ()
writeTVar = STM (\(tv,val) -> (\stmState -> do
               let TVar mv id waitq = tv
                   newState = stmState
                        {touchedTVars = Set.insert a (touchedTVars stmState),
                         writeSet     = IntMap.insert id 
                                          (unsafeCoerce val, putMVar mv val) 
                                          (writeSet stmState),
                         notifys      = notifys stmState >> fNotify waitQ}
               return (Success newState ()))) 


fNotify :: MVar [MVar ()] -> IO ()
fNotify waitQ = do
  queue <- takeMVar waitQ
    mapM_ (flip tryPutMVar ()) queue 
      putMVar waitQ []


