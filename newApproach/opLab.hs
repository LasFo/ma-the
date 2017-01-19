op :: STM a -> STM b -> STM (Val a, Val b)
(SMT act1) op (STM act2) = STM (\state -> do 
   res1 <- act1 state
   case res1 of 
   Success interimState val1 -> do
      res2 <- act2 interimState
      case res2 of 
        Success newState val2 -> --add actions to garbage
             return (Success newState (union (fst val1) (fst val2),(val1,val2)))
        Retry newState -> return & Retry newState
        InValid -> return InValid
    Retry newState -> return Retry newState
    InValid -> return InValid)

--Problems with types
act1 :: STM a
act2 :: STM b
act1 op act2 :: STM (Val a, Val b)
t2 :: TVar c
act1 op act2 **> writeTVar t2 fun
--fun need the following type
fun :: (Val a, Val b) -> c
--not possible, since Val is not exported
