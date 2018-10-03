
--    4 3 1 2
-- 1  2 3 1 4
-- 2  3 2 1 4
-- 3  1 2 3 4


--    4 3 1 2
-- 1  1 3 4 2   (0, 2)
-- 2  1 2 4 3   (1, 3)
-- 3  1 2 3 4   (2, 3)

--    2 3 4 1 5
-- 1  1 3 4 2 5
-- 2  1 2 4 3 5
-- 3  1 2 3 4 5

--    1 3 5 2 4 6 8
-- 1  1 2 5 3 4 6 8
-- 2  1 2 3 5 4 6 8
-- 3  1 2 3 4 5 6 8


import Data.Array.IO
import Control.Monad

parseValue :: String -> Int
parseValue = read

parse :: String -> IO (Int, IOArray Int Int)
parse xs = do
    let list = words xs
    let size = parseValue (head list)
    arr <- newArray (1, size) 0
    fillArray (drop 1 list) 1 arr
    return (size, arr)

fillArray :: [String] -> Int -> IOArray Int Int -> IO ()
fillArray [] _ arr = return ()
fillArray (x:xs) index arr = do
    writeArray arr index (parseValue x)
    fillArray xs (index + 1) arr

shuffle :: Int -> Int -> IOArray Int Int -> IO Int
shuffle index size arr
    | index == size = return 0
    | otherwise = do
        value <- readArray arr index
        (count, arr') <- swap index arr
        if count == 0
            then shuffle (index + 1) size arr'
            else (+) <$> return count <*> shuffle index size arr'

            
swap :: Int -> IOArray Int Int -> IO (Int, IOArray Int Int)
swap index arr = do
    value <- readArray arr index
    if value == index
        then return (0, arr)
        else do
            temp <- readArray arr value
            writeArray arr value value
            writeArray arr index temp
            return (1, arr)

            
run str = do
    (size, list) <- parse str
    count <- shuffle 1 size list
    return count


swap' :: Int -> IO (IOArray Int Int) -> IO (IOArray Int Int)
swap' index m = do
    arr <- m
    (_, arr2) <- swap index arr 
    return arr2




printElement :: Int -> IO (IOArray Int Int) -> IO ()
printElement index m = do
    arr <- m
    value <- readArray arr index 
    putStr (show value)

printArray :: Int -> Int -> IO (IOArray Int Int) -> IO [()]
printArray start end arr = mapM id [ printElement i arr | i <- [start..end] ]

printArray' :: Int -> Int -> IOArray Int Int -> IO [()]
printArray' start end arr = mapM id [ printElement' i arr | i <- [start..end] ]

printElement' :: Int -> IOArray Int Int -> IO ()
printElement' index arr = do
    value <- readArray arr index 
    putStr (show value)
