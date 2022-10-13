module Queue where

import Prelude hiding(tail, head)

class Queue q where
  empty :: q a
  is_empty :: q a -> Bool
  -- 末尾に要素を追加
  snoc :: q a -> a -> q a
  -- 先頭から要素を取り出し
  head :: q a -> Maybe a
  -- 先頭以外を取り出し
  tail :: q a -> Maybe (q a)

data BatchedQueue a =
  BatchedQueue [a] [a]

checkf :: BatchedQueue a -> BatchedQueue a
checkf (BatchedQueue [] r) = BatchedQueue (reverse r) []
checkf q                   = q

instance Queue BatchedQueue where
  empty = BatchedQueue [] []
  is_empty (BatchedQueue f _) = null f
  -- 末尾に追加
  snoc (BatchedQueue f r) x = checkf $ BatchedQueue f (x : r)
  -- 先頭から取り出し
  head (BatchedQueue [] _)    = Nothing
  head (BatchedQueue (x:f) _) = Just x
  -- 先頭を捨てる
  tail (BatchedQueue [] _)    = Nothing
  tail (BatchedQueue (x:f) r) = Just $ checkf $ BatchedQueue f r

class (Queue q) =>
      Deque q
  -- 先頭に要素を追加
  where
  cons :: q a -> a -> q a
  -- 末尾から要素を取り出し
  last :: q a -> Maybe a
  -- 末尾を捨てる
  init :: q a -> Maybe (q a)

data Deq a = Deq [a] [a] deriving (Eq, Show)

checkf2 :: Deq a -> Deq a
checkf2 (Deq [] r) = let (ff, rr) = splitHalf r
                     in Deq (reverse ff) rr
checkf2 (Deq f []) = let (ff, rr) = splitHalf f
                     in Deq ff (reverse rr)
checkf2 q = q

splitHalf :: [a] -> ([a], [a])
splitHalf as = let l = (length as) + 1 `div` 2
               in splitAt l as

instance Queue Deq where
  empty = Deq [] []
  is_empty (Deq f r) = (null f) && (null r)
  -- 末尾に追加
  snoc (Deq f r) x = checkf2 $ (Deq f (x:r))

  -- 先頭から取り出し
  head (Deq [] _) = Nothing
  head (Deq (x:f) _) = Just x

  -- 先頭を捨てる
  tail (Deq [] _) = Nothing
  tail (Deq (x:f) r) = Just $ checkf2 $ Deq f r

instance Deque Deq where
  cons (Deq f r) a = checkf2 $ Deq (a:f) r

  last (Deq f []) = Nothing
  last (Deq _ (x:xs)) = Just x

  init (Deq _ []) = Nothing
  init (Deq f (x:r)) =  Just $ checkf2 $ Deq f r