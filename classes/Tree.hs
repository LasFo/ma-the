import Data.Functor
import Control.Applicative
import Control.Monad

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

ifTestM :: Monad m => m a -> (a -> Bool) -> (a -> m b) -> (a -> m b) -> m b
ifTestM mo p t e = do
  x <- mo 
  if p x 
     then t x
     else e x

ifTestA :: Applicative f => f a -> (a -> Bool) -> (f (a -> b)) -> (f (a -> b)) -> f b
ifTestA ap p t e = 
  pure specialIf <*> pure p <*> t <*> e <*> ap  

testA t1 = ifTestA t1 (< 3) (pure (subtract 1)) (pure (+ 1)) 
{-
testWrite t1 t2 t3 = 
  ifTestA (readTVar t1) odd (writeTVar t2) (writeTVar t3) 
-}
specialIf :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
specialIf p t e v = if p v then t v else e v 
  

fromList :: [a] -> Tree a
fromList [a] = Leaf a
fromList xs  = fromList (split xs) :+ fromList (split (tail xs))

split :: [a] -> [a]
split [] = []
split (x:xs) = case xs of
    []   -> [x]
    y:ys -> x : split ys     

