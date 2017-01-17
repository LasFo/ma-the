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


-----------------------------------------------------
--------------Pure Applicative-----------------------
-----------------------------------------------------
--Aim of this section is to use Applicative functions
--to rebuild the function of Monad
(>>) = (*>)
return = pure
(>>=) =

--sequenceA is similar to sequence, but uses Applicative
--operations rather than monadic operations. 
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)

--collect the results of actions of different types
--by using this operator the dependencies are no longer specific
--for a single value rather than for all the collected values.
(>*<) :: Applicatife f => f a -> f b -> f (a,b)
(>*<) = (<*>) . fmap (,)
infixl 4 >*<

--process two actions, but only keep one result
(*>) :: Applicative f => f a -> f b -> f b
(<*) :: Applicative f => f a -> f b -> f a

--apply a pure function the the result of and action
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

--use the result of previous actions
--this is more or less the bind operator
--The only difference is that these operators
--do not make the value critical. The dependencies
--are not used and passed through.
(*>>) :: STM a -> (Val a -> STM b) -> STM b
(<<*) :: STM a -> (Val a -> STM b) -> STM a

dummyM = do
  a <- act1
  b <- act2
  p (a + b)
  q (a - b)
--In general it is not possible to achieve this with Applicative,
--since the type of p and q is (Num n => n -> f b).
--Back to STM

transaction = do
  a <- act1
  b <- act2
  writeTVar t (a + b)
  writeTVar t (a - b)

--a generic combinator for the 'fmap (uncurry (+))' part would be nice.
--Lets call it specialWrite for now. 
--specialWrite :: STM (a,(b,(c,(..)))) -> ((a,(b,(c,(..)))) -> z) -> STM (a,(b,(c,(..))))
--it is not possible to create 'specialWrite' straightforward (it may be possible with
--template haskell or similar code generating tools. HList/HVector might work aswell). 
transaction = 
  ((act1 >*< act2) <<* writeTVar t . fmap (uncurry (+))) <<* writeTVar t . fmap (uncurry (-)) 

transaction = do
  a <- act1
  writeTVar t a
  b <- act2
  writeTVar t (a - b)

transaction = 
  act1 <<* writeTVar t >*< act2 <<* writeTVar t . fmap (uncurry (+))

transaction = (<<*) 
                 (>*<)
                      (<<*) act1
                            writeTVar t
                      act2
                 (writeTVar t . fmap (uncurry (+)))

transaction = do
  a <- readTVar t
  b <- readTVar t2
  c <- readTVar t3
  writeTVar t2 a

--basically its a stack and you can access a single value by
--act <<* writeTVar t . fmap (querry)
--where querry is ([fst .] snd). So if you want the last pushed
--value your querry will be snd. For every fst you pop the stack
--and can access deeper elements by this.
transaction = do 
 readTVar t >*< readTVar t2 >*< readTVar t3 <<* writeTVar t2 . fmap snd

--using multiple elements
transaction = do 
  a <- readTVar t1
  b <- readTVar t2
  c <- readTVar t3
  writeTVar t3 (a + c)

transaction = 
  readTVar t1 >*< readTVar t2 >*< readTVar t3 <<* writeTVar t3 . fmap (uncurry ((+) . fst))

--for people who don't get along with pointfree style
transaction = 
  readTVar t1 >*< readTVar t2 >*< readTVar t3 <<* writeTVar t3 . fmap fun
    where fun ((a,b),c) = a + c

