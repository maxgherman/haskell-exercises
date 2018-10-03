newtype Compose f g a = Compose { unCompose :: f (g a) }
    deriving Show

data F a = F a deriving Show
data I a = I a deriving Show

cmp1 :: Compose F I Integer
cmp1 = Compose (F (I 10))

cmp2 :: Integer -> Compose [] Maybe Integer
cmp2 x = Compose [ Just x, Nothing, Just $ x + 1 ]

instance (Functor f, Functor g) => Functor (Compose f g) where
 -- fmap :: (a -> b) -> f a -> f b
 -- fmap :: (a -> b) -> Compose f g a -> Compose f g b
 -- f :: a -> b
 -- fg :: f (g a)
 -- app1:: f (f0 a -> f0 b)
    -- fmap f (Compose fg) = Compose app
    --     where
    --         -- app1 :: f b1
    --         -- g a -> b1
    --         --app1:: _APP
    --         app = fmap ff fg
    --         -- ff:: _FF
    --         ff x = fmap f x
    fmap f (Compose fg) = Compose $ fmap (fmap f) fg

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure
--(<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
--     Compose f <*> Compose k = Compose $ liftedAp <*> k 
--     -- f :: f (g (a -> b))
--     --liftedAp :: f (g a -> g b)
--     -- k :: f (g a)
--         where 
--         liftedAp = fmap (<*>) f
    (<*>) (Compose fgx) (Compose fga) = Compose $ (fmap (<*>) fgx) <*> fga


data Compose2 f g a = Frs { unFs:: f a } | Scn { unSc:: g a } deriving Show    

cmp3 = Frs [10]
cmp4 = Scn (Just 1)

instance (Functor f, Functor g) => Functor (Compose2 f g) where
-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> Compose2 f g a ->  Compose2 f g b
-- fa :: f a
-- f :: a -> b
    fmap f (Frs fa) = Frs $ fmap f fa
    fmap f (Scn ga) = Scn $ fmap f ga


data Compose3 f g h a = Compose3 { unComp3:: f (g (h a)) } deriving Show


instance (Functor f, Functor g, Functor h) => Functor (Compose3 f g h) where
--  fmap :: (a -> b) -> f a ->  f b
--  fmap :: (a -> b) -> Compose3 f g h a -> Compose3 f g h b
--  fgh :: f (g (h a))
--  f :: a -> b     
    fmap f (Compose3 fgh) = Compose3 $ fmap (fmap $ fmap f) fgh

instance (Applicative f, Applicative g, Applicative h) => Applicative (Compose3 f g h) where
 -- pure :: a -> f a
 -- pure a  ==> h a
 -- pure (h a) ==> g (h a)
 -- pure (g (h a)) ==> f (g (h a))
 -- pure :: a -> Compose3 f g h a   
    pure = Compose3 . pure . pure . pure
 -- (<*>) :: f (a -> b) -> f a -> f b
 -- (<*>) :: Compose3 f g h (a -> b) -> Compose3 f g h a -> Compose3 f g h b
 -- fghy :: f (g (h (a -> b)))
 -- fgha :: f (g (h a))
    (<*>) (Compose3 fghy) (Compose3 fgha) = Compose3 $ liftApp <*> fgha
        where
            -- liftApp :: f (g (h a) -> g (h b))
            liftApp = fmap (<*>) $ fmap (fmap (<*>)) fghy
            

data Compose4 f g h i a = Compose4 { unComp4:: f (g (h (i a))) } deriving Show

instance (Functor f, Functor g, Functor h, Functor i) => Functor (Compose4 f g h i) where
        fmap f (Compose4 fgh) = Compose4 $ fmap (fmap (fmap $ fmap f)) fgh
    

instance (Applicative f, Applicative g, Applicative h, Applicative i) => Applicative (Compose4 f g h i) where
    pure = Compose4 . pure . pure . pure . pure
    -- fghiy :: f (g (h (i (a -> b))))
    -- fghia :: f (g (h (i a)))
    (<*>) (Compose4 fghiy) (Compose4 fghia) = Compose4 $ liftApp <*> fghia
        where
            --liftApp :: f (g (h (i a)) -> g (h (i b)))
            liftApp = fmap (<*>) $ fmap (fmap (<*>)) $ fmap (fmap (fmap (<*>))) fghiy
        
