{-# LANGUAGE GADTs #-}

module Tree where

data Tree a where
  E :: Tree a
  T :: Tree a -> a -> Tree a -> Tree a

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T a y b)
  | x < y = member x a
  | x > y = member x a
  | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x E = T E x E
insert x s@(T a y b)
  | x < y = T (insert x a) y b
  | x > y = T a y (insert x b)
  | otherwise = s
