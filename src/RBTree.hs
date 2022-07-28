module RBTree where

data Color  = R | B deriving (Show, Eq)
data Tree a = E | T Color (Tree a) a (Tree a) deriving (Show, Eq)

member :: (Ord a) => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b) |  x < y = member x a
                     |  x > y = member x b
                     | otherwise = True

insert :: (Ord a) => a -> Tree a -> Tree a
insert x s = T B a y b
  where ins E = (T R E x E)
        ins s@(T color a y b) | x < y = balance (T color (ins a) y b)
                              | x > y = balance (T color a y (ins b))
                              | otherwise = s
        (T _ a y b) = ins s


balance :: Tree a -> Tree a
balance (T B (T R (T R a x b) y c) z d) = T R (T B a x b) y (T B c z d)
balance (T B (T R a x (T R b y c)) z d) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R (T R b y c) z d)) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
balance x = x

fromOrdList :: (Ord a) => [a] -> Tree a
fromOrdList = foldl (\ t a -> insert a t) E