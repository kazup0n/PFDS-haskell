{-# LANGUAGE GADTs #-}
module Tree where
import Data.Maybe ( fromMaybe )

data Tree a = E | T (Tree a) a (Tree a) deriving (Show, Eq)

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T a y b)
  | x < y = member x a
  | x > y = member x b
  | otherwise = True

-- quiz 2.2
member2 :: Ord a => a -> Tree a -> Bool
member2 _ E = False
member2 x (T left y right)
  | x >= y = case right of
                E -> x == y
                _ -> member2 x right
  | otherwise = member2 x left

insert :: Ord a => a -> Tree a -> Tree a
insert x E = T E x E
insert x s@(T left y right)
  | x < y = T (insert x left) y right
  | x > y = T left y (insert x right)
  | otherwise = s

insert2 :: Ord a => a -> Tree a -> Tree a
insert2 x t = fromMaybe t (insert2' t)
  where
  insert2' E = Just $ T E x E
  insert2' s@(T left y right)
    | x < y = Just $ T (insert2 x left) y right
    | x > y = Just $ T left y (insert2 x right)
    | otherwise = Nothing

-- exersize 2.4
insert3 :: Ord a => a -> Tree a -> Tree a
insert3 x t = fromMaybe t (insert3' t)
  where
    insert3' E = Just $ T E x E
    insert3' s@(T left y right)
      | x >= y = case right of
                    E | x == y -> Nothing
                      | otherwise -> Just $ T left y (T E x E)
                    _ -> Just $ T left y (insert3 x right)
      | otherwise = Just $ T (insert3 x left)  y right

-- exersise 2.5 (a)
complete :: Ord a => a -> Int -> Tree a
complete x 1 = T E x E
complete x n | n <= 0 = E
             | otherwise = T sub x sub
  where
    sub = complete x (n - 1)