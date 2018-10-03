-- input
-- 1 1 1 0 0 0
-- 0 1 0 0 0 0
-- 1 1 1 0 0 0
-- 0 0 2 4 4 0
-- 0 0 0 2 0 0
-- 0 0 1 2 4 0

--"1 1 1 0 0 0 0 1 0 0 0 0 1 1 1 0 0 0 0 0 2 4 4 0 0 0 0 2 0 0 0 0 1 2 4 0"

--"-9 -9 -9 1 1 1 0 -9 0 4 3 2 -9 -9 -9 1 2 3 0 0 8 6 6 0 0 0 0 -2 0 0 0 0 1 2 4 0"


main = interact $ show . run

run:: String -> Int
run = maximum . map sum . glasses
    where
        glasses xs = [getGlass (toNumbers xs) i j | i <- [0..3], j <- [0..3]]
        toNumbers = list . numbers
        numbers = map (\x -> read x ::Int) . words
        list [] = []
        list xs = [take 6 xs] ++ list (drop 6 xs)

positions = [[0,0], [0, 1], [0, 2], [1, 1], [2,0], [2, 1], [2, 2]]


getGlass :: [[Int]] -> Int -> Int -> [Int]
getGlass list i j = map (\[pi, pj] -> list !!(i+pi) !!(j+pj)) positions



