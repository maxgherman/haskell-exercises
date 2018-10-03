import Data.Array

parseValue :: String -> Int
parseValue = read

parse :: String -> [(Int, Array Int Int)]
parse = parseArrays . (drop 1) . words
    where
        parseArrays [] = []
        parseArrays (x:xs) = parseCount (parseValue x) xs 
        parseCount n xs = [(n, parseArray n $ take n xs)] ++ parseArrays (drop n xs)
        parseArray size list = array (1, size) $ zipWith makeEntry [1..] list
        makeEntry a b = (a, parseValue b)

count :: (Int,Array Int Int) -> Maybe Int
count (size, arr) = countWith size size arr
     
countWith :: Int -> Int -> Array Int Int -> Maybe Int
countWith 0 _ _ = Just 0
countWith index size arr
    | (arr!index) - index > 2 = Nothing
    | otherwise = (+) <$> Just countEntry <*> countWith (index - 1) size arr
    where countEntry = countGaps (arr!index) (1 `max` (arr!index - 2)) (index - 1) arr

countGaps :: Int -> Int -> Int -> Array Int Int -> Int
countGaps base start end arr = foldr countEntry 0 [start..end]
    where
        countEntry curr acc = if arr!curr > base then acc + 1 else acc

run :: String -> String
run = unlines . map (toString . count) . parse
    where
        toString Nothing = "Too chaotic"
        toString (Just n) = show n

