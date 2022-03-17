module BinomialHeap where

data Tree a = Node Int a [Tree a] deriving (Show, Eq)

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2) = if x1 <= x2 then Node (r + 1) x1 (t2 : c1) else Node (r + 1) x2 (t1 : c2)

type Heap a = [Tree a]

rank :: (Ord a) => Tree a -> Int
rank (Node r _ _) = r

insTree :: (Ord a) => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t ts@(t' : ts') = if rank t < rank t' then t : ts else insTree (link t t') ts'

insert :: (Ord a) => a -> Heap a -> Heap a
insert x = insTree (Node 0 x [])

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@(t1 : ts1') ts2@(t2 : ts2')
  | rank t1 < rank t2 = t1 : merge ts1' ts2
  | rank t2 < rank t1 = t2 : merge ts1 ts2'
  | otherwise = insTree (link t1 t2) (merge ts1' ts2')

removeMinTree :: (Ord a) => Heap a -> Maybe (Tree a, Heap a)
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t : ts) =
  let f (t', ts') = if root t < root t' then (t, ts) else (t', t : ts')
   in f <$> removeMinTree ts

findMin :: (Ord a) => Heap a -> Maybe a
findMin ts = root . fst <$> removeMinTree ts

-- removeMInTreeを経由しないfindMin
findMin2 :: (Ord a) => Heap a -> Maybe a
findMin2 [] = Nothing
findMin2 [t] = Just $ root t
findMin2 (t: ts) = min (root t) <$> findMin2 ts

root :: Tree a -> a
root (Node _ a _) = a

empty :: (Ord a) => Heap a
empty = []