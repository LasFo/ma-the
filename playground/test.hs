import Control.Concurrent
import Data.IORef
import Control.Monad
import System.IO.Unsafe


--I need either a sharing on the results of monadic (or at least IO) actions,
--which is on the other hand paradox since an action(especially IO) in general is not 
--referential transparent, so sharing is not meaningful.

main = do 
  ref <- newIORef 12 
  let p = act ref
  print p
  writeIORef ref 10
  print p


act ioref = 
  unsafePerformIO $ readIORef ioref
  
