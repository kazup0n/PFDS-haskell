
{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

module UnbalancedMap where
import FiniteMap ( FiniteMap(..) )
data UnbalancedMap k a = E | T (UnbalancedMap k a) k a (UnbalancedMap k a)  deriving (Show, Eq)
instance Ord a => FiniteMap UnbalancedMap a where

  empty = E

  lookup _ E = Nothing
  lookup x (T a y v b) | x < y = FiniteMap.lookup x a
                        | x > y = FiniteMap.lookup x b
                        | otherwise = Just v

  bind k a E = T E k a E
  bind x a m@(T l y v r) | x < y = T (bind x a l) y v r
                          | x > y = T l y v (bind x a r)
                          | otherwise = m