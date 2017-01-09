module Graph where

import Data.Maybe

type G1 a = [(a,a)]

g1 = [(1,2),(2,2),(2,3),(3,1),(3,2)]

newtype G2 a = G2 [(a,[a])]
 deriving Show

g2 = addNode 5 [2] [] $
     addNode 4 [1] [3] $
     addNode 3 [] [1,2] $
     addNode 2 [1,2] [2] $
     addNode 1 [] [] $
     emptyGraph

data G3 a = G3 [a] (a -> [a])

data Graph a = G [(a,[a])] [(a,[a])]

succs2 :: Eq a => a -> G2 a -> [a]
succs2 n (G2 g) = fromJust $ lookup n g

dfs :: Eq a => [a] -> G2 a -> [a]
dfs []     _       = []
dfs (n:ns) g = case matchNode n g of
                 Just (g',_,succ) -> n : dfs (succ ++ ns) g'
                 Nothing          -> dfs ns g

bfs :: Eq a => [a] -> G2 a -> [a]
bfs []     _       = []
bfs (n:ns) g = case matchNode n g of
                 Just (g',_,succ) -> n : bfs (ns ++ succ) g'
                 Nothing          -> bfs ns g


nodes :: G2 a -> [a]
nodes (G2 xs) = map fst xs


emptyGraph :: G2 a
emptyGraph = G2 []

addNode :: Eq a => a -> [a] -> [a] -> G2 a -> G2 a
addNode n pred succ (G2 g) 
  | isNothing (lookup n g) = G2 ((n,succ):addPred g)
  | otherwise = error "addNode tries to add existing node"
       where addPred = map (\(m,ms) -> (m,(if m `elem` pred then (n:) 
                                                            else id) ms))
matchNode :: Eq a => a -> G2 a -> Maybe (G2 a, [a], [a])
matchNode n (G2 g) = do 
  succs <- lookup n g
  let preds = [ m | (m,ms) <- g, n `elem` ms]
      g' = [ (m,filter (n/=) ms) | (m,ms) <- g, m /=n]
  return (G2 g', preds, succs)
                     







