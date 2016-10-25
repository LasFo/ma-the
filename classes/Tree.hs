import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Concurrent.STM

data Tree a = (Tree a) :+ (Tree a)
            | Leaf a
            | Empty
  deriving Show

instance Functor Tree where 
  fmap f (t1 :+ t2) = (fmap f t1) :+ (fmap f t2)
  fmap f (Leaf a)   = Leaf (f a)
  fmap _ Empty      = Empty

instance Applicative Tree where
  pure = Leaf

  (t1 :+ t2) <*> t3 = (t1 <*> t3) :+ (t2 <*> t3)
  (Leaf f)   <*> t2 = f <$> t2
  Empty      <*> t2 = Empty 

instance Alternative Tree where
   empty = Empty
   (<|>) = (:+)

instance Monad Tree where
  return = Leaf
  
  (t1 :+ t2) >>= f = (t1 >>= f) :+ (t2 >>= f)
  (Leaf a)   >>= f = f a
  Empty      >>= f = Empty 

instance MonadPlus Tree

first (t1 :+ _) = first t1
first (Leaf a) = a 

mergeTree :: Tree a -> Tree a -> Tree a
mergeTree = (<|>)

mergeTestM t t2 = do
 x <- t
 y <- t2
 return (x,y) 

mergeTestA t1 t2 = 
  pure (,) <*> t1 <*> t2

modTestM t f = do
  x <- t 
  return (f x)

modTestA t f =
  pure f <*> t

condWriteM :: Monad m => m a -> (a -> Bool) -> m b -> m b -> m b
condWriteM mo p t e = do
  x <- mo 
  if p x 
     then t 
     else e

condWriteA :: Applicative f => f a -> (a -> Bool) -> f b -> f b -> f b
condWriteA ap p t e = 
  pure specialIf <*> pure p <*> t <*> e <*> ap  

testA t1 = condWriteA t1 (< 3) [1] [24,2] 

apWrite :: TVar Int -> TVar Int -> TVar Int -> STM ()
apWrite t1 t2 t3 = condWriteA (readTVar t1) (< 6) (writeTVar t2 5) (writeTVar t3 5)

moWrite :: TVar Int -> TVar Int -> TVar Int -> STM ()
moWrite t1 t2 t3 = condWriteM (readTVar t1) (< 3) (writeTVar t2 5) (writeTVar t3 5)

main = do
  [t1,t2,t3] <- atomically $ sequence $ map newTVar [0,0,0]
  atomically $ moWrite t1 t2 t3
  val <- atomically $ sequence $ map readTVar [t1,t2,t3]
  print val

specialIf :: (a -> Bool) -> b -> b -> a -> b
specialIf p t e v = if p v then t else e  

testTree1 = [1..5]  

fromList :: [a] -> Tree a
fromList [a] = Leaf a
fromList xs  = fromList (split xs) :+ fromList (split (tail xs))

split :: [a] -> [a]
split [] = []
split (x:xs) = case xs of
    []   -> [x]
    y:ys -> x : split ys     

