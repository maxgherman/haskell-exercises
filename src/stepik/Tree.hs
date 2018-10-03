data Tree a = Leaf a | Node (Tree a) (Tree a)


height :: Tree a -> Int
height (Leaf _) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node a b) = 1 + (size a) + (size b)

values (Leaf a) = [a]
values (Node a b) = (values a) ++ (values b)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go tree = let elements = values tree in (length elements, sum elements)

