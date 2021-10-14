module Heap where

class Heap h where
  empty :: h a
  isEmpty :: h a -> Bool
  insert :: Ord a => a -> h a -> h a
  merge :: Ord a => h a -> h a -> h a
  findMin :: h a -> Maybe a
  deleteMin :: Ord a => h a -> Maybe (h a)

data HeapT a = E | T Int a (HeapT a) (HeapT a) deriving (Show, Eq)

rank :: HeapT a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> HeapT a -> HeapT a -> HeapT a
makeT x a b =
  if rank a >= rank b
    then T (rank b + 1) x a b
    else T (rank a + 1) x b a

insert2 :: Ord a => a -> HeapT a -> HeapT a
insert2 x E = T 1 x E E
-- 任意の左の子のランクが、右の子のランクと同じかそれ 以上である
-- ノードのランクは、そのノードの右スパイン
-- (right spine、そのノードから空ノードまでの最も右の経路)の長さとして定義される
-- x >= y の場合: 右に挿入する
-- x < y の場合: xを新しい根にしてyを左に挿入する
insert2 x (T _ y a b) = if x >= y then makeT y a (insert2 x b)
                                  else makeT x (insert2 y a) b


instance Heap HeapT where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  merge E E = E
  merge h@T {} E = h
  merge E h@T {} = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
    if x <= y
      then makeT x a1 (merge b1 h2)
      else makeT y a2 (merge h1 b2)

  insert x h = merge (T 1 x E E) h

  findMin E = Nothing
  findMin (T _ x a b) = Just x

  deleteMin E = Nothing
  deleteMin (T _ x a b) = Just $ merge a b