module Stack where
    import Data.List
    import Prelude hiding ((++))

    class Stack s where
        empty :: s a
        isEmpty :: s a -> Bool
        cons :: a -> s a -> s a
        head:: s a -> a
        tail:: s a -> s a
        (++):: s a -> s a -> s a
        update:: s a -> Int -> a -> s a
        suffixes:: s a -> s (s a)

    newtype ListStack a = LS [a] deriving (Show, Eq)

    instance Stack ListStack where
        empty = LS []

        isEmpty (LS []) = True
        isEmpty              _ = False

        head (LS as) =  Data.List.head as
        tail (LS as) = LS (Data.List.tail as)

        cons a (LS as) = LS (a:as)

        (++) (LS []) ys   = ys
        (++) (LS (x:xs)) ys = cons x ((LS xs) Stack.++ ys)

        update (LS (x:xs)) 0 y = cons y (LS xs)
        update (LS (x:xs)) i y = cons x (update (LS xs) (i-1) y)

        suffixes (LS xs) = LS [LS xs]

