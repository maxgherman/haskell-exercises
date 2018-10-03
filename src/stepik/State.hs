import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad (liftM, ap)

newtype State s a = State { runState:: s -> (a, s) }

instance Functor (State s) where
    fmap = liftM
  
instance Applicative (State s) where
    pure = return
    (<*>) = ap
  
instance Monad (State s) where
    return a = State (\st -> (a, st))
    (>>=) m k = State $ \st ->
        let (a, st') = runState m st
            m' = k a
        in runState m' st'


get:: State s s
get = State $ \st -> (st, st) 

put:: s -> State s ()
put st = State $ \_-> ((), st) 

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState:: State s a -> s -> a
evalState m s = fst (runState m s)

readerToState :: Reader r a -> State r a
readerToState m = State $ \st -> (f st, st)
        where f = runReader m

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State $ \st -> (a, mappend st w)
        where (a, w) = runWriter m



fibStep :: State (Integer, Integer) ()
fibStep = do
    (a,b) <- get
    put (b, a + b)
        

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM n m

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

num :: Tree () -> State Integer (Tree Integer)
num (Leaf _) = do
    n <- get
    put (n+1)
    return $ Leaf n
num (Fork l a r) = do
    left <- num l
    n <- get
    put (n+1)
    right <- num r
    return (Fork left n right)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (num tree) 1