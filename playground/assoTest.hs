import Unsafe.Coerce


data Mon a = Mon (Log -> IO (a, Log))

type Log = [Mon ()]


instance Functor Mon where 
  fmap f (Mon a) = Mon (\log -> do (b,log1) <- a log
                                   return (f b, log1))
 
appFst :: (a -> b) -> (a,c) -> (b,c)
appFst f (a,b) = (f a, b)

instance Applicative Mon where
  pure a = Mon (\log -> return (a,log))
  (Mon a1) <*> (Mon a2) = Mon (\log -> do 
                        (b,log1) <- a1 log
                        (c,log2) <- a2 log1
                        return (b c, log2))

instance Monad Mon where 
  return = pure
  (Mon a) >>= f = Mon (\log -> do (b,log1) <- a log
                                  let Mon fun = (f b)  
                                  fun log1)
  act@(Mon a) >> (Mon b) = Mon (\log -> do 
        a log
        let newLog = (unsafeCoerce act) : log
        b newLog)
                                         

atom :: Mon a -> IO a
atom (Mon act) = do 
  (res,log) <- act []
  processLog log []
  return res

processLog :: Log -> Log -> IO ()
processLog []     _     = return ()
processLog (x:xs) inLog = do 
    let Mon a = x
    (_,log) <- a inLog
    processLog xs log

test = 
  monAct "Hallo" >>
  monAct "Welt" >>
  monAct "!"

monAct str = Mon (\log -> do print str
                             return ((),log))
