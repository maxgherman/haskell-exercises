import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)


newtype ListMap' k v = ListMap' { getListMap' :: [(k,v)] }
    deriving (Eq,Show)


instance MapLike ListMap where
    empty = ListMap []
    lookup key list =  case L.find (\(k,_) -> k == key) (getListMap list) of
        Nothing -> Nothing
        Just (_,v) -> Just v
    insert key value (ListMap []) = ListMap [(key, value)]
    insert key value (ListMap (x:xs)) = if key == (fst x)
        then ListMap $ (key,value):xs
        else ListMap $ x:getListMap (insert key value $ ListMap xs)
    delete key (ListMap []) = empty
    delete key (ListMap (x:xs)) = if key == (fst x)
        then ListMap xs
        else ListMap $ x:getListMap (delete key $ ListMap xs)


instance MapLike ListMap' where
    empty = ListMap' []
    lookup key (ListMap' list) = L.lookup key list
    insert key value list = ListMap' $ (key,value) : getListMap' ( delete key list )
    delete key (ListMap' list) = ListMap' $ filter ((key ==) . fst) list  
        

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap ( \_ -> Nothing )
    lookup key (ArrowMap lambda) = lambda key
    insert key value (ArrowMap lambda) =
        ArrowMap { getArrowMap = getArrow  }
        where getArrow k = if key == k then Just value else lambda k
    delete key (ArrowMap lambda) =
        ArrowMap { getArrowMap = getArrow  }
        where getArrow k = if k == key then Nothing else lambda k
    fromList = foldr (\(k,v) acc -> insert k v acc) empty               
                
                
    -- instance MapLike ArrowMap where
    --     empty = ArrowMap $ const Nothing
    --     lookup k (ArrowMap f) = f k
    --     insert k v (ArrowMap f) = ArrowMap $ \x -> if x == k then return v else f x
    --     delete k (ArrowMap f) = ArrowMap $ \x -> if x == k then Nothing else f x
    --     fromList = foldr (uncurry insert) empty

data Tree a = Leaf a | Tree (Tree a) a (Tree a)
    deriving (Show)
