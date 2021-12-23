module WHeap where
  import Heap(Heap, empty, isEmpty, insert, merge, findMin, deleteMin)

  data WHeap a = E | T a (WHeap a) (WHeap a) deriving (Show, Eq)

  rank :: WHeap a -> Int
  rank E = 1
  rank (T _ l r) = rank l + rank r + 1

  makeT:: a -> WHeap a -> WHeap a -> WHeap a
  makeT x a b =
    if rank a >= rank b
      then T x a b
      else T x b a

  instance Heap WHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    merge E E = E
    merge h@T{} E = h
    merge E h@T{} = h
    merge h1@(T x a1 b1) h2@(T y a2 b2) = 
      if x <= y
        then makeT x a1 (merge b1 h2)
        else makeT y a2 (merge h1 b2)

    insert x h = merge (T x E E) h

    findMin E = Nothing
    findMin (T x a b) = Just x