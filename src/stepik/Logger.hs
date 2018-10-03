import Control.Monad (liftM, ap)


data Log a = Log [String] a deriving (Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f x = \a -> Log [x] $ f a

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f1 f2 =
    let log1 = f1 x  -- Log list1 b
        apply (Log _ a) = f2 a -- Log lst2 c
        combine (Log list1 _) (Log list2 c) = Log (list1 ++ list2) c
    in combine log1 $ apply log1
    
-- execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
-- execLoggers x f g = Log (msgs1 ++ msgs2) res2
--     where Log msgs1 res1 = f x
--             Log msgs2 res2 = g res1


returnLog :: a -> Log a
returnLog x = Log [] x

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log list x) f = Log (list ++ list2) x2
    where (Log list2 x2) = f x


instance Functor Log where
    fmap = liftM

instance Applicative Log where
    pure = return
    (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog


execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x = foldl (>>=) $ return x


-- GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
-- Log ["nothing done yet","added one"] 1


-- GHCi> execLoggers 3 add1Log mult2Log
-- Log ["added one","multiplied by 2"] 8


-- GHCi> let add1Log = toLogger (+1) "added one"
-- GHCi> add1Log 3
-- Log ["added one"] 4