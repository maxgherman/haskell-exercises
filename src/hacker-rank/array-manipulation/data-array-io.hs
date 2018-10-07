import Data.Array.IO

value :: String -> Int
value = read

readMultipleLines' :: [String] -> Int -> IOArray Int Int -> IO (IOArray Int Int)
readMultipleLines' [] _ arr = return arr
readMultipleLines' xs size arr = do
    arr' <- updateArray (take 3 xs) size arr
    arr'' <- readMultipleLines' (drop 3 xs) size arr'
    return arr''



readMultipleLines :: Int -> Int -> IOArray Int Int -> IO (IOArray Int Int)
readMultipleLines 0 size arr = return arr
readMultipleLines n size arr = do
    line <- getLine
    arr' <- updateArray (words line) size arr
    arr'' <- readMultipleLines (n - 1) size arr'
    return arr''
    

updateArray ::[String] -> Int -> IOArray Int Int -> IO (IOArray Int Int)
updateArray [s, t, v] size arr = do
    let start = value s
        finish = value t
        val = value v
    startValue <- readArray arr start
    writeArray arr start (startValue + val)
    if finish + 1 <= size
        then do
            finishValue <- readArray arr (finish + 1)
            writeArray arr (finish + 1) (finishValue - val)
            return arr
        else
            return arr
    
total :: Int -> Int -> Int -> Int -> (IOArray Int Int) -> IO (Int)
total max' start index size arr
    | index > size = return max'
    | otherwise = do
        value <- readArray arr index
        let next = start + value
        total (max max' next) next (index + 1) size arr

main = do
    firstLine <- getLine
    let [n, m] = words firstLine
        size = value n
        lines = value m
    arr <- newArray (1, size + 1) 0
    arr' <- readMultipleLines lines (size + 1) arr
    result <- total 0 0 1 size arr'
    return result


run = do
    text <- readFile "./2.txt"
    let lines = words text
    let size = value (head lines)
    arr <- newArray (1, size + 1) 0
    arr' <- readMultipleLines' (drop 2 lines) (size + 1) arr
    result <- total 0 0 1 size arr'
    return result
