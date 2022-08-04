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
        ins s@(T color a y b) | x < y, x `lt` value a = llbalance (T color (ins a) y b)
                              | x < y, x `gt` value a = lrbalance (T color (ins a) y b)
                              | x > y, x `lt` value b = rlbalance (T color a y (ins b))
                              | x > y, x `gt` value b = rrbalance (T color a y (ins b))
                              | otherwise = s
        (T _ a y b) = ins s
        (lt) x m = maybe True (> x) m
        (gt) x m = maybe True (< x) m
        value E = Nothing
        value (T _ _ x _) = Just x

rrbalance :: Tree a -> Tree a
rrbalance (T B a x (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
rrbalance  x = x

rlbalance :: Tree a -> Tree a
rlbalance (T B a x (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
rlbalance  x = x

llbalance :: Tree a -> Tree a
llbalance (T B (T R (T R a x b) y c) z d) = T R (T B a x b) y (T B c z d)
llbalance x = x
lrbalance :: Tree a -> Tree a
lrbalance (T B (T R a x (T R b y c)) z d) = T R (T B a x b) y (T B c z d)
lrbalance x = x

fromOrdList :: (Ord a) => [a] -> Tree a
fromOrdList = foldl (\ t a -> insert a t) E