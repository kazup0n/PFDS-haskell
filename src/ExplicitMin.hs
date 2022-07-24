module ExplicitMin where

import Heap (Heap, deleteMin, empty, findMin, insert, isEmpty, merge)

data ExplicitMin h a
  = E
  | T a (h a)

instance Heap h => Heap (ExplicitMin h) where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  findMin E = Nothing
  findMin (T a _) = Just a

  insert a E = T a (insert a empty)
  insert a (T _ h) =
    let h' = insert a h
        a' = findMin h'
     in case a' of
          Just m -> T m h'
          Nothing -> E

  merge (T a h)  E = T a h
  merge E (T a h) = T a h
  merge (T a1 h1) (T a2 h2)
    | a1 > a2 =  T a2 (merge h1 h2)
    | otherwise = T a1 (merge h1 h2)

  deleteMin (T a h) = do
    h' <- deleteMin h
    a' <- findMin h'
    return (T a' h')
  deleteMin _ = Nothing