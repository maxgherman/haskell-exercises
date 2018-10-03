data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add a b = toNat $ (fromNat a) + (fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = toNat $ (fromNat a) * (fromNat b)

fac :: Nat -> Nat
fac Zero = (Suc Zero)
fac (Suc Zero) = (Suc Zero)
fac a = toNat $ product [1..(fromNat a)]

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc $ toNat (n-1)