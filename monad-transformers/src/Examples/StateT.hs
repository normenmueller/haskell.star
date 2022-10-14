{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.StateT where

--import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Functor.Identity
import System.Random

newtype StateT s m a =
    StateT
        { runStateT :: s -> m (a, s)
        }

instance Functor m => Functor (StateT s m) where
    fmap f (StateT m) = StateT $ \s -> fmap (first f) (m s)
    -- fmap f (StateT m) = StateT $ \s -> fmap (\(a, s') -> (f a, s')) (m s)

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> return (a, s)
    (StateT f) <*> (StateT x) =
        StateT $ \s -> do
            (f', s') <- f s
            (x', s'') <- x s
            return (f' x', s'')

instance Monad m => Monad (StateT s m) where
    return a =  StateT $ \s -> return (a, s)
    (StateT m) >>= k =
        StateT $ \s -> do
            (a, s') <- m s
            runStateT (k a) s'

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

type State s a = StateT s Identity a

get :: Monad m => StateT s m s
get =  StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

state :: Monad m => (s -> (a, s)) -> StateT s m a
state = StateT . fmap return

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

-- cf. All About Monads

data MyType =
    MT Int Bool Char Int
    deriving (Show)

makeRandomValue :: StdGen -> (MyType, StdGen)
makeRandomValue g =
    let (n, g1) = randomR (1, 100) g
        (b, g2) = random g1
        (c, g3) = randomR ('a', 'z') g2
        (m, g4) = randomR (-n, n) g3
     in (MT n b c m, g4)

{- Using the State monad, we can define a function that returns a random value
   and updates the random generator state at the same time. -}
getAny :: (Random a) => State StdGen a
getAny = do
    (g :: StdGen) <- get
    (x :: a, g' :: StdGen) <- return $ random g
    put g' :: State StdGen ()
    return x :: State StdGen a

get' :: State StdGen StdGen
get' = state (\s -> (s, s))

-- similar to 'getAny', but it bounds the random value returned
getOne :: (Random a) => (a, a) -> State StdGen a
getOne bounds = do
    (g :: StdGen) <- get
    (x :: a, g' :: StdGen) <- return $ randomR bounds g
    put g' :: State StdGen ()
    return x

{- Using the State monad with StdGen as the state, we can build random complex
   types without manually threading the random generator states through the
   code. -}
makeRandomValueST :: StdGen -> (MyType, StdGen)
makeRandomValueST =
    runState $ do
        n <- getOne (1, 100)
        b <- getAny
        c <- getOne ('a', 'z')
        m <- getOne (-n, n)
        return $ MT n b c m
