module State where
{-
Inspiration from:
    Programming in Haskell, Graham Hutton, Cambridge University Press, 2016.
    COMP3012 Compilers module taught by Venanzio Capretta, University of Nottingham, 2022.
-}

-- State Transformer type inside IO monad
type STIO st a = STM st IO a

-- State transformer inside a monad
newtype STM st m a = S (st -> m (a, st))

app :: STM st m a -> st -> m (a, st)
app (S st) = st

lift :: Monad m => m a -> STM st m a
lift mx = S (\s -> do
    x <- mx
    return (x, s))

instance Monad m => Functor (STM st m) where
    -- fmap :: (a -> b) -> STM st m a -> STM st m b
    fmap g stx = do
        x <- stx
        return (g x)

instance Monad m => Applicative (STM st m) where
    -- pure :: a -> STM st m a
    pure x = S (\s -> return (x, s))

    -- (<*>) :: STM st m (a -> b) -> STM st m a -> STM st m b
    stf <*> stx = do
        f <- stf
        x <- stx
        return (f x)
    
instance Monad m => Monad (STM st m) where
    -- return :: a -> STM st m a
    return = pure

    -- (>>=) :: STM st m a -> (a -> STM st m b) -> STM st m b
    st >>= f = S (\s -> do
        (x, s') <- app st s
        app (f x) s')

stUpdate :: Monad m => st -> STM st m ()
stUpdate s = S (\_ -> return ((), s))

stState :: Monad m => STM st m st
stState = S (\s -> return (s, s))
