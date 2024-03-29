module SprayHeap where

data Tree a
  = E
  | T (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x t = T (smaller x t) x (bigger x t)

insert2 :: Ord a => a -> Tree a -> (Tree a, [a])
insert2 x t =
  let (small, big, as) = partition2 x t
   in (T small x big, as)

partition2 :: Ord a => a -> Tree a -> (Tree a, Tree a, [a])
partition2 pivot E = (E, E, [])
partition2 pivot t@(T a x b)
  -- 右の部分木を走査する(大きいのは右にある)
  | x <= pivot =
    case b of
      E -> (t, E, [x]) -- 右が空の場合はtにはピボットより小さいのしかない
      T b1 y b2
        | y <= pivot ->
          -- 右の部分木b2を走査する(pivotより大きいのは右にしかない)
          let (small, big, as) = partition2 pivot b2
           in (T (T a x b1) y small, big, [x, y] ++ as)
        | otherwise ->
          -- 左の部分木を走査する(右ノードにもpivotより大きいのが残っているかも)
          let (small, big, as) = partition2 pivot b1
           in (T a x small, T big y b2, [x, y] ++ as)
  -- 左の部分木を走査する(小さいのは左)
  | otherwise =
    case a of
      E -> (E, t, [x])
      T a1 y a2
        | y <= pivot ->
          let (small, big, as) = partition2 pivot a2
           in (T a1 y small, T big x b, [y, x] ++ as)
        | otherwise ->
          let (small, big, as) = partition2 pivot a1
           in (small, T big y (T a2 x b), [y, x] ++ as)

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

merge :: Ord a => Tree a -> Tree a -> Tree a
merge E t = t
merge (T a x b) t =
  let (ta, tb) = partition x t
   in T (merge ta a) x (merge tb b)

findMin :: Ord a => Tree a -> Maybe a
findMin (T E x b) = Just x
findMin (T a x b) = findMin a
findMin _ = Nothing

deleteMin :: Ord a => Tree a -> Maybe (Tree a)
deleteMin (T (T E x b) y c) = Just $ T b y c
deleteMin (T (T a x b) y c) = fmap (\l -> T l x (T b y c)) (deleteMin a)
deleteMin _ = Nothing