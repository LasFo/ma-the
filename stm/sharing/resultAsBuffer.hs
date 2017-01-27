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

--pointfree is possible, but if you work with more than 3 variables it becomes
--unreadable. But you can use programming with points. For example:
readTVar t1 >*< readTVar t2 >*< readTVar t3 >*< readTVar t4 <<* writeTVar t1 . fmap fun
  where fun ((a,b),c),d) = p a c
--in a pointfree manner:
 fun = fun = uncurry (uncurry ((const .) . p . fst))

--This is a way to apply pure functions to values that are going to be written.
--This prevents the user to branch on this values in a manner that it effects 
--the control flow of the transaction.
--There is still a problem with the dependencies. There is one collection of dependencies for 
--the whole set of values. Thus no individual dependencies for values.

----------------------------------------------------------------------------
-----------------------Discussion of Alternatives---------------------------
----------------------------------------------------------------------------
--instead of <<* operator we can alter writeTVar to, 
--where writeTVar' is the original write operation:
writeTVar :: TVar a -> (b -> a) -> STM b -> STM b
writeTVar tv fun act = act <<* writeTVar' tv . fmap fun
--or pointfree:
writeTVar = ((<<*) .) . (. fmap) . (.) . writeTVar'
--when using the old operator **> we can do:
transaction = 
   readTVar t1 **> writeTVar t1 (+ 1) >*< readTVar t2 **> writeTVar t2 (uncurry (+))

--Furthermore is it possible to alter the definition of readTVar,
--where readTVar' it the original read operation:
readTVar :: TVar a -> (STM b) -> STM (b,a)
readTVar tv act = act >*< readTVar' tv

--This allows us to combine all operation with the same combinator
--There is a problem with the first operation, but we ignore this for now:
transaction = 
  readTVar' t1 **> 
  readTVar t2 **> 
  writeTVar t3 (uncurry (+)) **> 
  readTVar t4 **> 
  writeTVar t5 fun

--there is just a problem with the definitions of 'fun'
--since the type depends on the amount of readTVar operations, which were
--done before. So the type changes if you read t1 and t2 in a changed order.

-----------------------------------------------------------------------------
-------------------------Combination with Bind-------------------------------
-----------------------------------------------------------------------------
act = do
  a <- readTVar t1
  if p a 
     then writeTVar t1 (a - 1)
     else writeTVar t2 (a + 1)
  return a

transaction = 
  readTVar t2 **> readTVar t3 >*< act **> writeTVar t2 fun
    where fun ((a,b),c) = a + b - c
--This means actions containing a bind are outsourced in other functions,
--which can then be used like normal actions.

act ::TVar b -> (b -> Bool) -> TVar a -> TVar a ->  STM (a,b)
act t1 p t3 t4 = do 
  a <- readTVar t1
  if p a 
     then readTVar t3 >*< return a
     else readTVar t4 >*< return a


-----------------------------------------------------------------------------------
----------------------------Furthr Discussion--------------------------------------
-----------------------------------------------------------------------------------
--The problem with the awkward types remains. The core problem is that these values
--need to be passed and collected in the result of STM. We could tackle this problem,
--by adding the a field in the STM state to collect the values for later uses.
--This requires an inhomgeneous datastrcture(or unsafeCoerce). 
--Another issue is the way you access these stored data. How do you identify
--the value(s) you need? Since the current structure is a stack like structure
--the same problem allready exists.

--Besides this there is a general issue with the values. If we use <*> to alter
--the current stack we lose the read values. For example:
action = 
  pure (+ 1) <*> readTVar t1
--There is no longer a way to access the result of 'readTVar t1', since this value is
--already overwritten. In other words the following is not possible:
transaction = do
  a <- readTVar t1
  let b = f a
  writeTVar t2 b
  writeTVar t3 a
  writeTVar t4 b

--The user need to backup the values manually, before he uses them to compute new values
transaction = 
  readTVar' t1 <**> pure fun **> writeTVar t2 snd **> writeTVar t3 fst **> writeTVar t4 snd
    where fun = (\x -> (x,f x))
--This leads to the following new combinator:
(>**<) :: STM a -> (a -> b) -> STM (a,b)
(>**<) act fun = act <**> helper 
  where helper = pure (\x -> (x,fun x))
--This retains the old value and adds the new Value to the top of the Stack.

