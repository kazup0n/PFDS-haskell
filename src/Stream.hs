module Stream where

data Stream a
  = Nil
  | Cons a (Stream a)
  deriving (Show, Eq)

(+++) :: Stream a -> Stream a -> Stream a
Nil +++ t        = t
(Cons x s) +++ t = Cons x (s +++ t)

take' :: Int -> Stream a -> Stream a
take' 0 _          = Nil
take' n Nil        = Nil
take' n (Cons x s) = Cons x (take' (n - 1) s)

drop :: Int -> Stream a -> Stream a
drop n s =
  let drop' :: Int -> Stream a -> Stream a
      drop' 0 s          = s
      drop' _ Nil        = Nil
      drop' n (Cons x s) = drop' (n - 1) s
   in drop' n s

sort :: Ord a => Stream a -> Stream a
sort Nil = Nil
sort (Cons x Nil) = (Cons x Nil)
sort (Cons x s) =
  let insert :: Ord a => a -> Stream a -> Stream a
      insert x Nil = Cons x Nil
      insert x (Cons y Nil)
        | x < y = Cons x (Cons y Nil)
        | otherwise = Cons y (Cons x Nil)
      insert x s =
        case sort s of
          Cons y s'
            | x < y -> Cons x (Cons y s')
            | otherwise -> Cons y (insert x s')
   in insert x s
