module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


f:: Int
f = let a=b; b=a in a + b

