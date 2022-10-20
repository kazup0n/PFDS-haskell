module SprayHeap where

data Tree a
  = E
  | T (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x t = T (smaller x t) x (bigger x t)

insert2 :: Ord a => a -> Tree a -> Tree a
insert2 x t =
  let (small, big) = partition x t
   in T small x big

bigger :: Ord a => a -> Tree a -> Tree a
bigger _ E = E
bigger pivot (T a x b)
  | x <= pivot = bigger pivot b
  | otherwise =
    case a of
      E -> T E x b
      (T a1 y a2)
        | y < pivot -> T (bigger pivot a2) x b
        | otherwise -> T (bigger pivot a1) y (T a2 x b)

smaller :: Ord a => a -> Tree a -> Tree a
smaller _ E = E
smaller pivot (T a x b)
  | x > pivot = smaller pivot b
  | otherwise =
    case a of
      E -> T E x b
      (T a1 y a2)
        | y > pivot -> T b y (smaller pivot a1)
        | otherwise -> T (T a1 y b) x (smaller pivot a2)

partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition _ E = (E, E)
partition pivot t@(T a x b)
  | x <= pivot =
    case b of
      E -> (t, E)
      T b1 y b2
        | y <= pivot ->
          let (small, big) = partition pivot b2
           in (T (T a x b1) y small, big)
        | otherwise ->
          let (small, big) = partition pivot b1
           in (T a x small, T big y b2)
  | otherwise =
    case a of
      E -> (E, t)
      T a1 y a2
        | y <= pivot ->
          let (small, big) = partition pivot a2
           in (T a1 y small, T big x b)
        | otherwise ->
          let (small, big) = partition pivot a1
           in (small, T big y (T a2 x b))
