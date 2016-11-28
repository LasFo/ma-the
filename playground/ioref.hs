import Control.Concurrent (forkIO)
import Data.IORef

main = do
  ref <- newIORef 5
  forkIO $ printloop ref
  writeloop ref

printloop :: IORef Int -> IO ()
printloop ref = do 
   val <- readIORef ref
   print val
   printloop ref
   
writeloop :: IORef Int -> IO ()
writeloop ref = do
  modifyIORef' ref (+ 1)
  writeloop ref     
