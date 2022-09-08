## Exercise 4.1

```
-- pが遅延評価だったら eを正格評価する
fun lazy f p = e
fun f x = $case x of p ⇒ force e
```

```
fun lazy drop (0, s) = s
       | drop (n, $Nil) = $Nil
       | drop (n, $Cons (x, s)) = drop (n−1, s)
```


```
fun lazy drop2 (n,s) = let fun drop′ (0,s)=s
                          | drop′ (n, $Nil) = $Nil
                          | drop′ (n, $Cons (x, s)) = drop′ (n−1, s) 
                   in drop′ (n, s) end
```

1. dropとdrop'は同じ

```
drop (n, $Nil) = force $Nil ~= $Nil
drop (n $Cons (x, s)) = force drop (n-1, s) ~= drop (n-1, s)
```

2. dropとdrop2は同じ

```
drop2 = force $ drop'(n s) 
     ~= force $ drop (n, s)  -- drop=drop'より
     ~= drop -- force $e = e より
```