{-# LANGUAGE ExistentialQuantification #-}

module ExplicitMin where

import Heap (Heap, deleteMin, empty, findMin, insert, isEmpty, merge)

data ExplicitMin h a
  = E
  | T a (h a)

data EM a = forall t. Heap t => EM (ExplicitMin t a)

instance Heap EM where
  empty = EM E
  isEmpty (EM E) = True
  isEmpty (EM _) = False
  findMin (EM E) = Nothing
  findMin (EM (T a _)) = Just a
  insert a (EM E) = EM (T a (insert a empty))
  insert a (EM (T _ h)) =
    let h' = insert a h
        a' = findMin h'
     in case a' of
          Just m -> EM (T m h')
          Nothing -> (EM E)
  merge (EM (T a h)) (EM E) = EM (T a h)
  merge (EM E) (EM (T a h)) = EM (T a h)
  merge (EM (T a1 h1)) (EM (T a2 h2))
    | a1 > a2 = EM (T a2 (merge h1 h2))
    | otherwise = EM (T a1 (merge h1 h2))
  deleteMin (EM E) = Nothing
  deleteMin (EM (T a h)) = do
    h' <- deleteMin h
    a' <- findMin h'
    return (EM (T a' h'))
