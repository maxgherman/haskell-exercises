import System.Directory (getDirectoryContents, removeFile)
import Data.List (isInfixOf)


main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine
    if name == "" then main' else putStrLn $ "Hi, " ++ name ++ "!"


main'' :: IO ()
main'' = do
    putStr "Substring: "
    name <- getLine
    if null name
        then putStrLn "Canceled"
        else do  
            contents <- getDirectoryContents "./test"
            let files = filter (isInfixOf name) contents 
            removeFiles files
 
removeFiles :: [String] -> IO ()
removeFiles [] = return ()
removeFiles (x:xs) = do
    putStr "Removing file: "
    putStrLn x
    removeFile x
    removeFiles xs
   