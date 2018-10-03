import Data.Char
import Data.List

meanList :: [Double] -> Double
meanList = result . foldr (\x (l, s) -> (l+1, s+x)) (0, 0)
    where result (a, b) = b / a


evenOnly :: [a] -> [a]
evenOnly = (foldr attache []) . foo
    where
        foo xs = zipWith (,) [1..] xs
        attache (p, e) es = if odd p then es else e:es


lastElem :: [a] -> a
lastElem = foldl1 $ flip  const

revRange :: (Char,Char) -> [Char]
revRange (start, stop) = unfoldr g stop
  where g x = if x < start then Nothing else Just (x, pred x)


data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp a b = compare (toInt a) (toInt b) 
    where
        toInt Error = 3
        toInt Warning = 2
        toInt Info = 1


data Result' = Fail Int | Success

instance Show Result' where
    show (Fail n) = "Fail: " ++ show n
    show Success = "Success"


data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

add' :: Z -> Z -> Z
add' z1 z2 = intToZ $ (toInt z1) + (toInt z2)
    where
        toInt z@(Z s bits) = (signToInt s) * toUnsinedInt z 
        toUnsinedInt (Z _ bits) = foldr (\(n, x) acc -> (bitToInt x)*2^n +  acc ) 0 . zip [0..] $ reverse bits
        bitToInt Zero = 0
        bitToInt One = 1
        signToInt Minus = -1
        signToInt Plus = 1
        intToZ a | a < 0 = Z Minus $ unsigndeIntToBits a
                 | a == 0 =  Z Plus [Zero]
                 | a > 0 = Z Plus $ unsigndeIntToBits a
        unsigndeIntToBits = map intToBit . reverse . unfoldr (\x -> if x == 0 then Nothing else Just (mod x 2, div x 2))
        intToBit 1 = One
        intToBit 0 = Zero


add :: Z -> Z -> Z
--add (Z _ []) (Z _ []) = Z Plus []
add z1 z2 = intToZ $ (toInt z1) + (toInt z2)

mul :: Z -> Z -> Z
mul z1 z2 = intToZ $ (toInt z1) * (toInt z2)

toInt z@(Z s bits) = (signToInt s) * toUnsinedInt z 

toUnsinedInt (Z _ bits) = foldr (\(n, x) acc -> (bitToInt x)*2^n +  acc ) 0 $ zip [0..] bits

bitToInt Zero = 0
bitToInt One = 1

signToInt Minus = -1
signToInt Plus = 1

intToZ a | a < 0 = Z Minus $ unsigndeIntToBits $ (-1) * a
         | a == 0 =  Z Plus [Zero]
         | a > 0 = Z Plus $ unsigndeIntToBits a

unsigndeIntToBits x | x == 0 = [Zero]
                    | otherwise = map intToBit $ unfoldr toBaseTwo x
        where
            toBaseTwo x | x <= 0 = Nothing
                        | otherwise = Just (mod x 2, div x 2)

intToBit 1 = One
intToBit 0 = Zero



checkSum :: Int -> Int -> Bool
checkSum a b = a + b == (toInt $ add (intToZ a) (intToZ b))

checkMul :: Int -> Int -> Bool
checkMul a b = a * b == (toInt $ mul (intToZ a) (intToZ b))


data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p = p { firstName = cut $ firstName p }
        where cut a = if length a < 2 then a else [(head a) , '.'] 


data Coord a = Coord a a deriving (Show, Eq)

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord a b) = Coord (getDistance a) (getDistance b) 
    where getDistance p = (fromIntegral p + 1) * width - width / 2

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord a b) = Coord (devide a) (devide b)
    where
        devide p = floor $ p / width



data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = [] 
fromList (Cons a xs) = a : fromList xs  

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x $ toList xs


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

-- expand :: Expr -> Expr
-- expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
-- expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
-- expand ((e :+: e1) :+: e2) =  expand e :+: (expand e1 :+: expand e2)
-- expand ((e :*: e1) :*: e2) =  expand e :*: (expand e1 :*: expand e2)
-- expand (e1 :+: e2) = expand e1 :+: expand e2
-- expand (e1 :*: e2) = expand e1 :*: expand e2
-- expand e = e

-- expand :: Expr -> Expr


expand :: Expr -> Expr
expand a
    | a == traversed = a
    | otherwise = expand traversed
    where
        traversed = traverce a
        traverce ((e1 :+: e2) :*: e) = (traverce e1) :*: traverce e :+: traverce e2 :*: traverce e
        traverce (e :*: (e1 :+: e2)) = traverce e :*: traverce e1 :+: traverce e :*: traverce e2
        traverce (e1 :+: e2) = traverce e1 :+: traverce e2
        traverce (e1 :*: e2) = traverce e1 :*: traverce e2
        traverce e = e



newtype Xor = Xor { getXor :: Bool } deriving (Eq,Show)
    
instance Monoid Xor where
    mempty = Xor False
    mappend a b = Xor (a /= b)

newtype Maybe' a = Maybe' { getMaybe :: Maybe a } deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just mempty
    mappend (Maybe' Nothing) _ = (Maybe' Nothing)
    mappend _ (Maybe' Nothing) = (Maybe' Nothing) 
    mappend (Maybe' a) (Maybe' b) = Maybe' $ mappend a b
