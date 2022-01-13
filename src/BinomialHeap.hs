module BinomialHeap () where

data Tree a = Node Int a [Tree a]

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2) = if x1 >= x2 then Node (r + 1) x1 (t2 : c1) else Node (r + 1) x2 (t1 : c2)