--This case is too general
--To decide how to transform this to the optimal solution,
--we need more information obout 'f'
main = do 
  a <- readTVar 
  f a   

--Transformation 1
--core idea of transformation
main = do 
  a <- readTVar t1
  writeTVar' t2 a
    ||
    ||
    \/ 
main = do 
  readTVar t1 **> wrtieTVar t2
  
--Transformation 2
--extended feature. The control flow is not effected by 'a'
--so no rollback is needed if 'a' changes
main = do 
  a <- readTVar t1
  let b = f a 
  writeTVar' t2 b
    ||
    ||
    \/
main = do
  let b = f <$> (readTVar t1) 
  writeTVar t2 b 
 
--Transformation 3
--Can be deduced from Transformation 2, by using it recursively
main = do
  a1 <- readTVar t1
  a2 <- readTVar t2
  ...
  an <- readTVar tn
  let res = f a1 a2 ... an
    ||
    ||
    \/
main = do 
  let c = f <$> (readTVar t1) <*> readTVar t2 <*> ... <*> readTVar tn

  
--Transformation 4
--Combination of Transformation 1 and 2
main = do
  a <- readTVar t1
  writeTVar t2 (f a)
    ||
    ||
    \/
main = do 
  f <$> (readTVar t1) **> writeTVar t2

--Transformation 5
--Binding a value of a readTVar and afterwards overwriting this value 
--can not be transformed properly into a pure applicative version
--You cannot even express this with the pure applicative syntax by
--hand. UPDATE see below for solution.
main = do
  a <- readTVar t1
  writeTVar t1 x
  b <- readTVar t2
  writeTVar t2 a

--This example illustrated the increasing complexety with an increase in 
--the lines of code. Even more complicated transformations arise if the 
--binded values are combined with pure values, because if we apply the 
--transformations, these pure values need to be lifted in the STM context
--to be able to combine them with the SMT actions.
main = do
  a <- readTVar t1
  writeTVar' t2 (f a)
  let b = g a 42
  writeTVar' t3 (h b)
     ||
     ||
     \/
main = do
  let a = readTVar t1
  writeTVar t2 (f <$> a)
  let b = g <$> a <*> pure 42
  writeTVar t3 (h <$> b)



--Cases where no transformation is possbile:

--If-Then-Else
--No tranformation at all, since the value is part of a branch condition.
--This is case where the rollback is actually desired, if the value changes.
main = do
  a <- readTVar t1
  ...
  if p a 
    then ...
    else ...
      ||
      ||
      \/
main = do 
  a <- readTVar t1
  ...
  if p a 
    then ...
    else ... 

--Read-Overwrite-Use
--It is not possible to swap two values with only the applicative operations.
--This may be a syntactical problem rather than a problem in general, but is not 
--yet verified. 
main = do
  a <- readTVar t1 
  readTVar t2 **> writeTVar t1
  writeTVar' t2 a

--UPDATE: This problem was fixed by adding the function 'eval :: STM a -> STM(STM a)'
--that executes an action and stalls its result. This allows you to stall the result 
--of a readTVar to use it later on. 
main = do
  a <- eval $ readTVar t1
  readTVar t2 **> writeTVar t1
  writeTVar t2 a

--Functions
--If the exctracted values are used in functions to create new STM action,
--we can not transform this without deeper knowledge on the function.
--The function may use guards on the vlaue which is some kind of branch.
--Criteria for branch funktions:
--1. Patternmatching on that value
--2. Guards
--3. If then else conditions on that value
--4. Case conditions of that value
--5. If the value is a monadic action and executed
main = do
  a <- readTVar t1
  h a 

{-
Es gibt keine Moeglichtkeit Funktionen in Haskell zu vergleichen,
weswegen ein Abbildung von (a -> STM b) nach (STM a -> STM b)
nicht moeglich ist. Diese waere noetig um das ganze allgemein zu 
machen. Selbst der Vergleich im Compiler (ueber pointer) 
waere wenig sinnig, da die partielle Applikation das wieder 
kaput macht. Also mit oder ohne Compiler, man kommt nicht an
eine (wie oben angedeutete) Transformation ohne die Syntax
zu aendern. Wobei das Syntax aendern die ganze Transformation
unsinnig machn wuerde.

Rewrite Rules are a promising tool to grant the desired program transformation.
The rewrite rules are applied before the program is desugared.
This means we have to apply the rewrite rules to the do-notation.
This is noch possible, since the left hand side of rewrite rules need to 
be a top level applyable expression. Hence statements such as 'a <- readTVar t1'
are not eligable as LHS and could not be tranformed with rewrite rules.
Nevertheless can the rewrite rules be used to 'formalize' the desired
transformations.

Thanks to the semantics of STM some rearangements of the code do not 
change the semantics.
-}
