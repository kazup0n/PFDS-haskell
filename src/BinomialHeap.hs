module BinomialHeap where

data Tree a = Node a [Tree a] deriving (Show, Eq)

link :: (Ord a) => TreeR a -> TreeR a -> TreeR a
link (r, t1@(Node x1 c1)) (_, t2@(Node x2 c2)) = if x1 <= x2 then (r + 1, Node x1 (t2 : c1)) else (r + 1, Node x2 (t1 : c2))

type TreeR a = (Int, Tree a)

type Heap a = [TreeR a]

rank :: (Ord a) => TreeR a -> Int
rank (r, Node _ _) = r

insTree :: (Ord a) => TreeR a -> Heap a -> Heap a
insTree t [] = [t]
insTree t ts@(t' : ts') = if rank t < rank t' then t : ts else insTree (link t t') ts'

insert :: (Ord a) => a -> Heap a -> Heap a
insert x = insTree (0, Node x [])

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@(t1 : ts1') ts2@(t2 : ts2')
  | rank t1 < rank t2 = t1 : merge ts1' ts2
  | rank t2 < rank t1 = t2 : merge ts1 ts2'
  | otherwise = insTree (link t1 t2) (merge ts1' ts2')

-- removeMInTreeを経由しないfindMin
findMin2 :: (Ord a) => Heap a -> Maybe a
findMin2 [] = Nothing
findMin2 [t] = Just $ root t
findMin2 (t: ts) = min (root t) <$> findMin2 ts

root :: TreeR a -> a
root (_, Node  a _) = a

empty :: (Ord a) => Heap a
empty = []