--5 4 1 2 3 4 5



parse :: String -> (Int, [String])
parse xs = (read $ head $ list, drop 1 $ list)
    where
        list = drop 1 $ words xs

rotate :: Int -> [String] -> [String]
rotate _ [] = []
rotate 0 xs = xs
rotate n xs = (drop n xs) ++ (take n xs)
    
run:: String -> String
run list = unwords $ rotate (d `mod` length xs) xs
    where (d, xs) = parse list


main = interact run