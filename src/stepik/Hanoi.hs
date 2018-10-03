
data Move = Move { disk::Int, source::Char, destination::Char }
    deriving Show

hanoi :: Int -> Char -> Char -> Char -> [Move]
hanoi 0 _ _ _ = []
hanoi n s a d =
    (hanoi (n - 1) s d a) ++ [Move n s d] ++ (hanoi (n - 1) a s d)