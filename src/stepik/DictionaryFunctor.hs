data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show


instance Functor (Entry k1 k2) where
    fmap f (Entry pair v) = Entry pair $ f v

instance Functor (Map k1 k2) where
    fmap f (Map list) = Map $ fmap (fmap f) list