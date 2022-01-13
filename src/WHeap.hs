module WHeap where

import Heap (Heap, deleteMin, empty, findMin, insert, isEmpty, merge)

data WHeap a = E | T a (WHeap a) (WHeap a) deriving (Show, Eq)

rank :: WHeap a -> Int
rank E = 1
rank (T _ l r) = rank l + rank r + 1

makeT :: a -> WHeap a -> WHeap a -> WHeap a
makeT x a b =
  if rank a >= rank b
    then -- ランクが大きい方を左に寄せる
      T x a b
    else T x b a

instance Heap WHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  merge E E = E
  merge h@T {} E = h
  merge E h@T {} = h
  merge h1@(T x a1 b1) h2@(T y a2 b2)
    | x <= y = -- yが大きい場合:
    -- 小さい方(x) をルートにして
    -- 1. b1(左側)と大きい方のツリーをマージ
    -- 2. a1と接続する; a1 と1のrankの大きい方を左に寄せる
    -- then makeT x a1 (merge b1 h2)
    -- rankの比較を先に行うことで途中で生成されるmergeの部分木の評価を遅延させることができる
    -- そのため
    -- 1. 当該部分木にアクセスしなければ評価しなくてもいい
    -- 2. 変更されないので並列に処理できる
      if rank a1 >= rank b1 + rank h2 then T y a1 (merge b1 h2) else T y (merge b1 h2) a1
    | rank a2 >= rank h1  + rank b2 = T x (merge h1 b2) a2
    | otherwise = T y a2 (merge b1 h2)

  insert x h = merge (T x E E) h

  findMin E = Nothing
  findMin (T x _ _) = Just x

  deleteMin E = Nothing
  deleteMin (T _ a b) = Just $ merge a b