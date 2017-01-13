import Control.Concurrent.MVar
import STM

transaction t1 t2 = do
  a <- readTVar t1 
  writeTVar t2 a

--A type like STM(a,b) does not allow different context information for the first and
--the second element. I need an inhomogeneous data structure which allows us to specify
--informations for single entries. Inhomogenous data structures by them self are diffcult 
--in Haskell.


transaction2 t1 t2 = 
  readTVar t1 <<* writeTVar t2
