-- 5 3
-- 1 2 100
-- 2 5 100
-- 3 4 100


-- 5 3 1 2 100 2 5 100 3 4 100

-- 10 3 1 5 3 4 8 7 6 9 1

import Data.Array

-- input = [
--     [1, 2, 100 ],
--     [2, 5, 100 ],
--     [3, 4, 100]
--     ] :: [[Int]]

-- original = [0,0,0,0,0,0] :: [Int] --  (5 + 1) elements

value :: String -> Int
value = read

updateArray :: [Int] -> Int -> Array Int Int -> Array Int Int
updateArray [start, finish, value] size arr =
    arr // ((start, arr!start + value) : secondPair finish)
    where
        secondPair index
            | (index + 1) <= size = [(index + 1, arr!(index + 1) - value)]
            | otherwise = []
    
updateAll :: [[Int]] -> Int -> Array Int Int -> Array Int Int
updateAll list size arr = foldr (\curr acc -> updateArray curr size acc) arr list 

total :: Int -> Array Int Int -> Int
total size arr = snd $ foldr (\curr (total, max') ->  (total + arr!curr,  max (total + arr!curr) max')) (0, 0) $ [size, (size -1)..1]

parse :: String -> (Int, [[Int]])
parse ls = (value size, list $ drop 2 entries)
    where
        entries = words ls
        size = head entries
        list [] = []
        list xs = (map value $ take 3 xs) : list (drop 3 xs)

run :: String -> String
run ls = let (size, list) = parse ls
             totalSize = size + 1
         in show $ total size $ arr' list totalSize
         where
            arr size = listArray (1, size) $ take size $ repeat 0
            arr' list size = updateAll list size $ arr size



run' = do
    text <- readFile "./1.txt"
    return $ run text