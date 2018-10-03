import Data.Char
import Data.Function

main = putStrLn "Hello, world!"

lenVec3 x y z =  sqrt ( x*x + y*y + z*z )

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x) && (isDigit y) then (digitToInt x) * 10 + (digitToInt y) else 100

fibonacci :: Integer -> Integer
fibonacci (-1) = 1
fibonacci 1 = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)
            | otherwise = 0


fibonacci' :: Integer -> Integer
fibonacci' (-1) = 1
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n | n >= 0 = helper n 0 1
             | otherwise = helper' n 0 1 
                        
helper 0 a _  = a 
helper 1 _ b  = b
helper n a b = helper (n-1) b (a+b) 

helper' 0 a _  = a 
helper' (-1) _ b  = b
helper' n a b = helper' (n+1) b (a-b) 



seqA :: Integer -> Integer
seqA n = helper2 n 1 2 3
    where
        helper2 0 a _ _ = a
        helper2 1 a b _ = b
        helper2 2 _ _ c = c
        helper2 n a b c = helper2 (n - 1) b c (c + b - 2*a)



sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = let
        s = sum result
        l = length' result
    in (s, l)
        where
            result = digits $ abs x
            digits x = if wholePart == 0 then [ remainder ] else digits wholePart ++ [remainder] 
                where
                    wholePart = div x 10
                    remainder = mod x 10
            length' [] = 0
            length' (x:xs) = 1 + length' xs
            

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * (first + seq)
    where
        n = 10000
        h = (b - a) / n
        seq = sum $ map f [ h * x + a | x <- [1..(n - 1)]]
        first = (f a + f b) / 2

getSecondFrom:: t1 -> t2 -> t3 -> t2
getSecondFrom _ b _ = b

multSecond = g `on` h
    where
        g = (*)
        h = snd

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


doItYourself = f . g . h
    where
        f = logBase 2
        g = (^3)
        h = max 42

fff1 :: a -> (a,b) -> a -> (b,a,a)
fff1 x (x1, x2) x3 = (x2, x, x)

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

swap' = f (g h)
    where
        h = (,)
        g = flip
        f = uncurry


f1 = flip
f2 = (,)
f3 x y = flip (,) x y



class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = concat ["(", toString a, ",", toString b, ")"]




class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a
        | (doesEnrageMork a) && (doesEnrageGork a) = stomp $ stab a
        | (doesEnrageMork a) = stomp a
        | (doesEnrageGork a) = stab a
        | otherwise = a   


-- "127.224.120.12"
ip = show a ++ show b ++ show c ++ show d
    where
        a = 127.2
        b = 24.1
        c = 20.1
        d = 2


class (Bounded a, Enum a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc a = if a == maxBound then minBound else succ a

    spred :: a -> a
    spred a = if a == minBound then maxBound else pred a

instance SafeEnum Bool

avg :: Int -> Int -> Int -> Double
avg a b c =   (fromIntegral a + fromIntegral b + fromIntegral c) / 3


foo a = a
bar = const foo
baz x = const True
quux = let x = x in x
corge = "Sorry, my value was changed"  -- Y

grault x 0 = x
grault x y = x

garply = grault 'q'
waldo = foo


nTimes:: a -> Int -> [a]
nTimes a n = map (const a) [1..n]

nTimes':: a -> Int -> [a]
nTimes' a n
    | n < 0 = undefined
    | n == 0 = []
    | otherwise = a : nTimes' a (n - 1) 
    
    
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = [] 
oddsOnly (x:xs) = if x `mod` 2 /= 0 then x : oddsOnly xs else oddsOnly xs

oddsOnly' :: Integral a => [a] -> [a]
oddsOnly' = filter (\x -> x `mod` 2 /= 0)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 al bl cl = add al bl cl []
    where
        add [] [] [] result = result
        
        add [] (b:bs) (c:cs) result = (b + c) : add [] bs cs result
        add (a:as) [] (c:cs) result = (a + c) : add as [] cs result
        add (a:as) (b:bs) [] result = (a + b) : add as bs [] result
        
        add [] [] (c:cs) result = c : add [] [] cs result
        add [] (b:bs) [] result = b : add [] bs [] result
        add (a:as) [] [] result = a : add as [] [] result
        
        add (a:as) (b:bs) (c:cs) result = (a + b + c) : add as bs cs result

sum3' :: Num a => [a] -> [a] -> [a] -> [a]
sum3' as bs cs = (add as + add bs + add cs) : sum3 (rest as) (rest bs) (rest cs)
    where
        add [] = 0
        add (x:_) = x
        rest [] = []
        rest (_:xs) = xs



groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (el:els) = add [el] [] els
    where
        add current result [] = result ++ [current]
        add current result (x:xs) =
            if x == (head current) then add (x:current) result xs
            else add [x] (result ++ [current]) xs
            

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (\x -> p1 x || p2 x)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ x : qsort right
    where
        left = filter (< x) xs
        right = filter (>= x) xs

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])
    
perms :: [a] -> [[a]]
perms [] = [[]]
perms lst@(x:xs) = concatMap ((rotate $ length lst).(x:)) (perms xs)
    where rotate len xs = take len (iterate (\(y:ys) -> ys ++ [y]) xs)

-- perms :: [a] -> [[a]]
-- perms [] = [[]]
-- perms [x] = [[x]]
-- perms (x:xs) = concatMap (insertElem x) (perms xs) where
--             insertElem x [] = [[x]]
--             insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)

delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words
    
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max (max x y) z)

ints n = n : ints (n+1)

fibStream :: [Integer]
fibStream =  0 : 1 : zipWith (+) fibStream (tail fibStream)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

repeat' = iterate id

data Odd = Odd Integer
    deriving (Eq, Show)

instance Enum Odd where
    -- succ :: a -> a
    succ (Odd a) = Odd (a+2)

    -- pred :: a -> a
    pred (Odd a) = Odd (a-2)

    -- toEnum :: Int -> a
    toEnum a = Odd (fromIntegral a)

    -- fromEnum :: a -> Int
    fromEnum (Odd a) = fromIntegral a

    -- enumFrom :: a -> [a]
    enumFrom (Odd a) = map Odd $ iterate (+2) a

    -- enumFromThen :: a -> a -> [a]
    enumFromThen (Odd a) (Odd b) = map Odd $ [a, b..]
    
    --enumFromTo :: a -> a -> [a]
    enumFromTo (Odd a) (Odd b) = map Odd $ takeWhile (<= b) (iterate (+2) a)
    
    --enumFromThenTo :: a -> a -> a -> [a]
    enumFromThenTo (Odd a) (Odd b) (Odd c) = map Odd $ [a, b..c]



coins ::(Ord a, Num a) => [a]
coins = [2, 3, 7]
    
change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change n = [ c:cs | c <- coins, c <= n, cs <-change (n - c) ]


concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\_ s -> s + 1) 0

lengthList' :: [a] -> Int
lengthList' = foldr (\_ s -> s + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr f 0
    where f x s | x `mod`2 == 0 = s
                | otherwise = x + s