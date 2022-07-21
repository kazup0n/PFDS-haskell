module RBTree where

data Color  = R | B
data Tree a = E | T Color (Tree a) a (Tree a)

member :: (Ord a) => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b) |  x < y = member x a
                     |  x > y = member x b
                     | otherwise = True


balance :: Tree a -> Tree a
balance (T B (T R (T R a x b) y c) z d) = T R (T B a x b) y (T B c z d)
balance (T B (T R a x (T R b y c ) z d)) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R (T R b y c) z d)) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
balance = id